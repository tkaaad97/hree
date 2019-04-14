{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , addMesh
    , deleteScene
    , geometryFromVertexVector
    , newScene
    , removeMesh
    , renderScene
    ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldr')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import qualified Data.Traversable as Traversable (mapM)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Storable)
import qualified Foreign (nullPtr, with)
import qualified GLW
import qualified GLW.Groups.ClearBufferMask as GLW (glColorBufferBit)
import qualified GLW.Groups.PrimitiveType as PrimitiveType
import qualified GLW.Internal.Objects as GLW (Buffer(..))
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Geometry
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import Unsafe.Coerce (unsafeCoerce)

data MeshInfo = MeshInfo
    { mishInfoId          :: !Int
    , meshInfoMesh        :: !Mesh
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GLW.VertexArray
    } deriving (Show)

data Scene = Scene
    { sceneMeshCounter      :: !(IORef Int)
    , sceneMeshes           :: !(IORef (IntMap MeshInfo))
    , sceneBufferRefCounter :: !(IORef (IntMap Int))
    , scenePrograms         :: !(IORef (Map ProgramSpec ProgramInfo))
    }

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GLW.glClearColor 1 1 1 1
    GLW.glClear GLW.glColorBufferBit
    meshs <- readIORef (sceneMeshes scene)
    renderMeshes [("projectionViewMatrix", Uniform projectionViewMatrix)] meshs

renderMeshes :: [(ByteString, Uniform)] -> IntMap MeshInfo ->  IO ()
renderMeshes uniforms = renderMany uniforms . fmap toRenderInfo

toRenderInfo :: MeshInfo -> RenderInfo
toRenderInfo m = renderInfo
    where
    program = meshInfoProgram m
    vao = meshInfoVertexArray m
    dm = resolveDrawMethod . meshGeometry . meshInfoMesh $ m
    uniforms = [] -- TODO
    texture = Nothing -- TODO
    renderInfo = RenderInfo program dm vao uniforms texture

resolveDrawMethod :: Geometry -> DrawMethod
resolveDrawMethod geo | isNothing (geometryIndexBuffer geo) =
    let count = fromIntegral . geometryCount $ geo
    in DrawArrays PrimitiveType.glTriangles 0 count
resolveDrawMethod geo =
    let count = fromIntegral . geometryCount $ geo
    in DrawElements PrimitiveType.glTriangles count GL.GL_UNSIGNED_INT Foreign.nullPtr

addMesh :: Scene -> Mesh -> IO Int
addMesh scene mesh = do
    i <- genMeshId

    program <- mkProgramIfNotExists scene pspec
    GLW.glUseProgram (programInfoProgram program)

    let bos = map fst . IntMap.elems $ buffers
    let bs' = maybe bos (: bos) maybeIndexBuffer
    vao <- mkVertexArray (geometryAttribBindings geo) buffers maybeIndexBuffer program
    let minfo = MeshInfo i mesh bs' program vao
    insertMeshInfo i minfo
    incrementBufferRefCounter bs'
    return i

    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshes scene
    sceneBufferRef = sceneBufferRefCounter scene
    genMeshId = atomicModifyIORef' mcRef (\a -> (a + 1, a))
    insertMeshInfo i minfo = atomicModifyIORef' meshesRef $ \a -> (IntMap.insert i minfo a, ())
    insertsWith f = foldr' (uncurry $ IntMap.insertWith f)
    incrementBufferRefCounter bs =
        let bs' = List.nub . map (fromIntegral . GLW.unBuffer) $ bs
        in atomicModifyIORef' sceneBufferRef $
            \a -> (insertsWith (+) a (bs' `zip` repeat 1), ())
    geo = meshGeometry mesh
    buffers = geometryBuffers geo
    maybeIndexBuffer = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh

removeMesh :: Scene -> Int -> IO ()
removeMesh scene i = do
    minfo <- atomicModifyIORef' meshesRef delMesh
    case minfo of
        Just (MeshInfo _ _ bs _ vao) -> do
            GLW.deleteObject vao
            ds <- decrementBufferRefCounter bs
            GLW.deleteObjects ds
        Nothing -> return ()
    where
    meshesRef = sceneMeshes scene
    sceneBufferRef = sceneBufferRefCounter scene
    delMesh m =
        let (x, m') = IntMap.updateLookupWithKey (const $ const Nothing) i m
        in (m', x)
    updateLookupWith' f k (xs, m) =
        let (x, m') = IntMap.updateLookupWithKey f k m
        in (x : xs, m')
    decrementAndLookupDeletion bs m =
        let ks = List.nub . map (fromIntegral . GLW.unBuffer) $ bs
            decrementOrDelete a
                | a <= 1 = Nothing
                | otherwise = Just (a - 1)
            (xs, m') = foldr' (updateLookupWith' (const decrementOrDelete)) ([], m) ks
            ds = map fst . filter ((== Just 1) . snd) $ (ks `zip` xs)
            ds' = map (GLW.Buffer . fromIntegral) ds
        in (m', ds')

    decrementBufferRefCounter bs =
        atomicModifyIORef' sceneBufferRef (decrementAndLookupDeletion bs)

geometryFromVertexVector :: forall a. (Storable a, Vertex a) => GLW.BindingIndex -> Vector a -> GL.GLenum -> Scene -> IO Geometry
geometryFromVertexVector bindingIndex storage usage scene = do
    buffer <- mkBuffer (BufferSource storage usage)
    let buffers = IntMap.singleton (fromIntegral . GLW.unBindingIndex $ bindingIndex) (buffer, bbs)
    return (Geometry attribBindings buffers Nothing num)
    where
    VertexSpec bbs fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField name format) = AttribBinding bindingIndex format
    attribBindings = Map.fromList $ zip keys bindings
    num = Vector.length storage

--setBackground

mkProgramIfNotExists :: Scene -> ProgramSpec -> IO ProgramInfo
mkProgramIfNotExists scene pspec = do
    programs <- readIORef programsRef
    let maybeProgram = Map.lookup pspec programs
    maybe (mkProgramAndInsert programsRef pspec) return maybeProgram
    where
    programsRef = scenePrograms scene

mkProgramAndInsert :: IORef (Map ProgramSpec ProgramInfo) -> ProgramSpec -> IO ProgramInfo
mkProgramAndInsert programsRef pspec = do
    program <- mkProgram pspec
    atomicModifyIORef' programsRef (\a -> (Map.insert pspec program a, ()))
    return program

newScene :: IO Scene
newScene = do
    counter <- newIORef 1
    meshes <- newIORef IntMap.empty
    buffers <- newIORef IntMap.empty
    programs <- newIORef Map.empty
    return $ Scene counter meshes buffers programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    meshes <- atomicModifyIORef' meshesRef $ \a -> (IntMap.empty, a)
    mapM_ (mapM_ GLW.deleteObject . meshInfoBuffers) meshes
    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshes scene
