{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , SceneState(..)
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
import Data.Maybe (isNothing, mapMaybe)
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

newtype Scene = Scene { unScene :: IORef SceneState }

data SceneState = SceneState
    { ssMeshCounter      :: !Int
    , ssMeshes           :: !(IntMap MeshInfo)
    , ssBufferRefCounter :: !(IntMap Int)
    , ssPrograms         :: !(Map ProgramSpec ProgramInfo)
    }

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GLW.glClearColor 1 1 1 1
    GLW.glClear GLW.glColorBufferBit
    meshes <- fmap ssMeshes . readIORef . unScene $ scene
    renderMeshes [("projectionViewMatrix", Uniform projectionViewMatrix)] meshes

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
    (program, programAdded) <- mkProgramIfNotExists scene pspec
    GLW.glUseProgram (programInfoProgram program)
    vao <- mkVertexArray (geometryAttribBindings geo) buffers maybeIndexBuffer program
    atomicModifyIORef' (unScene scene) (addMeshFunc program programAdded vao)

    where
    insertsWith f kvs m = foldr' (uncurry $ IntMap.insertWith f) m kvs
    addMeshFunc program programAdded vao state =
        let meshId = ssMeshCounter state
            meshCounter = meshId + 1
            bos = map fst . IntMap.elems $ buffers
            bos' = maybe bos (: bos) maybeIndexBuffer
            nubBufferIds = List.nub . map (fromIntegral . GLW.unBuffer) $ bos'
            minfo = MeshInfo meshId mesh bos' program vao
            meshes = IntMap.insert meshId minfo (ssMeshes state)
            bufferRefCounter =
                insertsWith (+) (nubBufferIds `zip` repeat 1) (ssBufferRefCounter state)
            programs = if programAdded
                        then Map.insert pspec program (ssPrograms state)
                        else ssPrograms state
            newState = SceneState meshCounter meshes bufferRefCounter programs
        in (newState, meshId)

    geo = meshGeometry mesh
    buffers = geometryBuffers geo
    maybeIndexBuffer = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh

removeMesh :: Scene -> Int -> IO ()
removeMesh scene meshId = do
    (vao, deletedBuffers) <- atomicModifyIORef' state (removeMeshFunc meshId)
    maybe (return ()) GLW.deleteObject vao
    GLW.deleteObjects deletedBuffers
    where
    state = unScene scene

removeMeshFunc :: Int -> SceneState -> (SceneState, (Maybe GLW.VertexArray, [GLW.Buffer]))
removeMeshFunc meshId state =
    let (result, meshes) = IntMap.updateLookupWithKey (const $ const Nothing) meshId (ssMeshes state)
        vao = fmap meshInfoVertexArray result
        bos = maybe [] meshInfoBuffers result
        (bufferRefCounter, deletedBuffers) = decrementAndLookupDeletion bos (ssBufferRefCounter state)
        newState = state { ssMeshes = meshes, ssBufferRefCounter = bufferRefCounter }
    in (newState, (vao, deletedBuffers))
    where

    updateLookupWith' f k (xs, m) =
        let (x, m') = IntMap.updateLookupWithKey f k m
        in (x : xs, m')

    decrementAndLookupDeletion [] m = (m, [])
    decrementAndLookupDeletion bs m =
        let ks = List.nub . map (fromIntegral . GLW.unBuffer) $ bs
            decrementOrDelete a
                | a <= 1 = Nothing
                | otherwise = Just (a - 1)
            (xs, m') = foldr' (updateLookupWith' (const decrementOrDelete)) ([], m) ks
            ds = map fst . filter ((== Just 1) . snd) $ (ks `zip` xs)
            ds' = map (GLW.Buffer . fromIntegral) ds
        in (m', ds')

addBuffer :: Scene -> BufferSource -> IO GLW.Buffer
addBuffer scene bufferSource = do
    buffer <- mkBuffer bufferSource
    let bufferId = fromIntegral . GLW.unBuffer $ buffer
    atomicModifyIORef' (unScene scene) (addBufferFunc bufferId)
    return buffer
    where
    addBufferFunc bufferId state =
        let bufferRefCounter = IntMap.insert bufferId 0 (ssBufferRefCounter state)
        in (state { ssBufferRefCounter = bufferRefCounter }, ())

geometryFromVertexVector :: forall a. (Storable a, Vertex a) => GLW.BindingIndex -> Vector a -> GL.GLenum -> Scene -> IO Geometry
geometryFromVertexVector bindingIndex storage usage scene = do
    buffer <- addBuffer scene (BufferSource storage usage)
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

mkProgramIfNotExists :: Scene -> ProgramSpec -> IO (ProgramInfo, Bool)
mkProgramIfNotExists scene pspec = do
    programs <- fmap ssPrograms . readIORef . unScene $ scene
    let maybeProgram = Map.lookup pspec programs
    maybe (flip (,) True <$> mkProgramAndInsert scene pspec) (return . flip (,) False) maybeProgram

mkProgramAndInsert :: Scene -> ProgramSpec -> IO ProgramInfo
mkProgramAndInsert scene pspec = do
    program <- mkProgram pspec
    atomicModifyIORef' (unScene scene) (insertProgram program)
    return program
    where
    insertProgram program state =
        let programs = Map.insert pspec program (ssPrograms state)
            newState = state { ssPrograms = programs }
        in (newState, ())

newScene :: IO Scene
newScene = Scene <$> newIORef initialSceneState

initialSceneState :: SceneState
initialSceneState =
    let counter = 1
        meshes = IntMap.empty
        buffers = IntMap.empty
        programs = Map.empty
    in SceneState counter meshes buffers programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    (vaos, buffers) <- atomicModifyIORef' (unScene scene) deleteSceneFunc
    GLW.deleteObjects vaos
    GLW.deleteObjects buffers

    where
    deleteSceneFunc state =
        let meshIds = IntMap.keys . ssMeshes $ state
            (state', xs) = foldr' removeMeshFunc' (state, []) meshIds
            vaos = mapMaybe fst xs
            buffers = map (GLW.Buffer . fromIntegral) . IntMap.keys . ssBufferRefCounter $ state'
        in (initialSceneState, (vaos, buffers))

    removeMeshFunc' i (s, xs) =
        let (s', x) = removeMeshFunc i s
        in (s, x : xs)