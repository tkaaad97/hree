module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , addMesh
    , removeMesh
    , deleteScene
    ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Traversable as Traversable (mapM)
import qualified Foreign (nullPtr, with)
import qualified Graphics.GL as GLRaw
import Graphics.Hree.Camera
import Graphics.Hree.Geometry
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Mesh
import Graphics.Hree.Program
import qualified Graphics.Rendering.OpenGL as GL
import Unsafe.Coerce (unsafeCoerce)

data MeshInfo = MeshInfo
    { mishInfoId          :: !Int
    , meshInfoMesh        :: !Mesh
    , meshInfoBuffers     :: ![GL.BufferObject]
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GL.VertexArrayObject
    }

data Scene = Scene
    { sceneMeshCounter :: !(IORef Int)
    , sceneMeshs       :: !(IORef (IntMap MeshInfo))
    , scenePrograms    :: !(IORef (Map ProgramSpec ProgramInfo))
    }

renderScene :: Scene -> Camera -> IO ()
renderScene scene camera = do
    projectionViewMatrix <- getCameraMatrix camera
    GL.clearColor GL.$= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    meshs <- readIORef (sceneMeshs scene)
    renderMeshs meshs

renderMeshs :: IntMap MeshInfo -> IO ()
renderMeshs = renderMany . fmap toRenderInfo

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
    in DrawArrays GL.Triangles 0 count
resolveDrawMethod geo =
    let count = fromIntegral . geometryCount $ geo
    in DrawElements GL.Triangles count GL.UnsignedInt Foreign.nullPtr

addMesh :: Scene -> Mesh -> IO Int
addMesh scene mesh = do
    i <- genMeshId
    bs <- Traversable.mapM (mkBuffer GL.ArrayBuffer) bufferSources
    maybeIndexBuffer <- maybe (return Nothing) (fmap Just . mkBuffer GL.ElementArrayBuffer . uncurry BufferSource) indexBufferSource
    let bs' = maybe (IntMap.elems bs) (: IntMap.elems bs) maybeIndexBuffer
    program <- mkProgramIfNotExists scene pspec
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
    bufferSources = geometryBufferSources geo
    indexBufferSource = geometryIndexBuffer geo
    pspec = resolveProgramSpec mesh

removeMesh :: Scene -> Int -> IO ()
removeMesh scene i = do
    minfo <- atomicModifyIORef' meshesRef del
    case minfo of
        Just (MeshInfo _ _ bs vao _) -> do
            GL.deleteObjectNames bs
            Foreign.with (unsafeCoerce vao) (GLRaw.glDeleteVertexArrays 1)
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

deleteScene :: Scene -> IO ()
deleteScene scene = do
    meshes <- atomicModifyIORef' meshesRef $ \a -> (IntMap.empty, a)
    mapM_ (mapM_ GL.deleteObjectName . meshInfoBuffers) meshes
    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshs scene
