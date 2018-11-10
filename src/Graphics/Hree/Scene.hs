module Graphics.Hree.Scene
    ( MeshInfo(..)
    , Scene(..)
    , addMesh
    , removeMesh
    , deleteScene
    ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef')
import Graphics.Hree.Geometry
import Graphics.Hree.GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Mesh
import qualified Graphics.Rendering.OpenGL as GL

data MeshInfo = MeshInfo
    { mishInfoId      :: !Int
    , meshInfoMesh    :: !Mesh
    , meshInfoBuffers :: ![GL.BufferObject]
    }

data Scene = Scene
    { sceneMeshCounter :: !(IORef Int)
    , sceneMeshs       :: !(IORef (IntMap MeshInfo))
    }

addMesh :: Scene -> Mesh -> IO Int
addMesh scene mesh = do
    i <- genMeshId
    bs <- mapM (mkBuffer GL.ArrayBuffer) (IntMap.elems bufferSources)
    bs' <- case indexBufferSource of
            (Just (ibs, usage)) -> (: bs) <$> mkBuffer GL.ElementArrayBuffer (BufferSource ibs usage)
            _          -> return bs
    let minfo = MeshInfo i mesh bs'
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

removeMesh :: Scene -> Int -> IO ()
removeMesh scene i = do
    minfo <- atomicModifyIORef' meshesRef del
    case minfo of
        Just (MeshInfo _ _ bs) -> GL.deleteObjectNames bs
        Nothing                -> return ()
    where
    meshesRef = sceneMeshs scene
    del a =
        let x = IntMap.lookup i a
        in maybe (a, Nothing) ((,) (IntMap.delete i a) . Just) x

--setBackground

deleteScene :: Scene -> IO ()
deleteScene scene = do
    meshes <- atomicModifyIORef' meshesRef $ \a -> (IntMap.empty, a)
    mapM_ (mapM_ GL.deleteObjectName . meshInfoBuffers) meshes
    where
    mcRef = sceneMeshCounter scene
    meshesRef = sceneMeshs scene
