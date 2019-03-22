{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , addMesh
    , deleteScene
    , newScene
    , removeMesh
    , renderScene
    ) where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Traversable as Traversable (mapM)
import qualified Foreign (nullPtr, with)
import qualified GLW
import qualified GLW.Groups.ClearBufferMask as GLW (glColorBufferBit)
import qualified GLW.Groups.PrimitiveType as PrimitiveType
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Geometry
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import Unsafe.Coerce (unsafeCoerce)

data MeshInfo = MeshInfo
    { mishInfoId          :: !Int
    , meshInfoMesh        :: !Mesh
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GLW.VertexArray
    }

data Scene = Scene
    { sceneMeshCounter :: !(IORef Int)
    , sceneMeshs       :: !(IORef (IntMap MeshInfo))
    , scenePrograms    :: !(IORef (Map ProgramSpec ProgramInfo))
    }

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GLW.glClearColor 1 1 1 1
    GLW.glClear GLW.glColorBufferBit
    meshs <- readIORef (sceneMeshs scene)
    renderMeshs [("projectionViewMatrix", Uniform projectionViewMatrix)] meshs

renderMeshs :: [(ByteString, Uniform)] -> IntMap MeshInfo ->  IO ()
renderMeshs uniforms = renderMany uniforms . fmap toRenderInfo

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

    bs <- Traversable.mapM mkBuffer' buffers
    let bos = map fst . IntMap.elems $ bs
    maybeIndexBuffer <- maybe (return Nothing) (fmap Just . mkBuffer . uncurry BufferSource) indexBufferSource
    let bs' = maybe bos (: bos) maybeIndexBuffer
    vao <- mkVertexArray (geometryAttribBindings geo) bs maybeIndexBuffer program
    let minfo = MeshInfo i mesh bs' program vao
    insertMeshInfo i minfo
    return i

    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshs scene
    genMeshId = atomicModifyIORef' mcRef (\a -> (a + 1, a))
    insertMeshInfo i minfo = atomicModifyIORef' meshesRef $ \a -> (IntMap.insert i minfo a, ())
    geo = meshGeometry mesh
    buffers = geometryBuffers geo
    indexBufferSource = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh
    mkBuffer' (source, setting) = do
        b <- mkBuffer source
        return (b, setting)

removeMesh :: Scene -> Int -> IO ()
removeMesh scene i = do
    minfo <- atomicModifyIORef' meshesRef del
    case minfo of
        Just (MeshInfo _ _ bs _ vao) -> do
            GLW.deleteObjects bs
            GLW.deleteObject vao
        Nothing -> return ()
    where
    meshesRef = sceneMeshs scene
    del a =
        let x = IntMap.lookup i a
        in maybe (a, Nothing) ((,) (IntMap.delete i a) . Just) x

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
    programs <- newIORef Map.empty
    return $ Scene counter meshes programs

deleteScene :: Scene -> IO ()
deleteScene scene = do
    meshes <- atomicModifyIORef' meshesRef $ \a -> (IntMap.empty, a)
    mapM_ (mapM_ GLW.deleteObject . meshInfoBuffers) meshes
    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshs scene
