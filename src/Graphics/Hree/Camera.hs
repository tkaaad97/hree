module Graphics.Hree.Camera
    ( Camera
    , LookAt(..)
    , Orthographic(..)
    , Perspective(..)
    , Projection(..)
    , lookAt
    , newCamera
    , orthographic
    , perspective
    , getCameraLookAt
    , getCameraMatrices
    , getCameraProjection
    , updateProjection
    , updateLookAt
    ) where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Foreign (ForeignPtr, mallocForeignPtr, poke, withForeignPtr)
import Graphics.Hree.GL.Block (Block(..), Std140(..))
import Graphics.Hree.GL.Types (Mat4, Vec3)
import qualified Linear (identity, lookAt, normalize, ortho, perspective)

data Camera = Camera !(IORef CameraState) !(Foreign.ForeignPtr (Std140 CameraBlock))

data CameraState = CameraState
    { cameraProjection            :: !Projection
    , cameraLookAt                :: !LookAt
    , cameraStateProjectionMatrix :: !Mat4
    , cameraStateViewMatrix       :: !Mat4
    , cameraStateNeedUpdate       :: !Bool
    } deriving (Show, Eq)

data LookAt = LookAt
    { lookAtEye    :: !Vec3
    , lookAtCenter :: !Vec3
    , lookAtUp     :: !Vec3
    } deriving (Show, Eq)

data Perspective = Perspective
    { perspectiveFov    :: !Float
    , perspectiveAspect :: !Float
    , perspectiveNear   :: !Float
    , perspectiveFar    :: !Float
    } deriving (Show, Eq)

data Orthographic = Orthographic
    { orthographicLeft   :: !Float
    , orthographicRight  :: !Float
    , orthographicBottom :: !Float
    , orthographicTop    :: !Float
    , orthographicNear   :: !Float
    , orthographicFar    :: !Float
    } deriving (Show, Eq)

data Projection =
    PerspectiveProjection !Perspective |
    OrthographicProjection !Orthographic
    deriving (Show, Eq)

data CameraBlock = CameraBlock
    { cameraBlockProjectionMatrix :: !Mat4
    , cameraBlockViewMatrix       :: !Mat4
    , cameraBlockViewPosition     :: !Vec3
    } deriving (Show, Eq)

instance Block CameraBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 144 -- 64 + 64 + 16

    peekByteOffStd140 ptr off = do
        projMat <- peekByteOffStd140 ptr off
        viewMat <- peekByteOffStd140 ptr (off + 64)
        viewPos <- peekByteOffStd140 ptr (off + 128)
        return $ CameraBlock projMat viewMat viewPos

    pokeByteOffStd140 ptr off (CameraBlock projMat viewMat viewPos) = do
        pokeByteOffStd140 ptr off projMat
        pokeByteOffStd140 ptr (off + 64) viewMat
        pokeByteOffStd140 ptr (off + 128) viewPos

perspective :: Float -> Float -> Float -> Float -> Projection
perspective fov aspect near far = PerspectiveProjection $ Perspective fov aspect near far

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> Projection
orthographic left right bottom top near far = OrthographicProjection $ Orthographic left right bottom top near far

lookAt :: Vec3 -> Vec3 -> Vec3 -> LookAt
lookAt eye center up = LookAt eye center (Linear.normalize up)

newCamera :: Projection -> LookAt -> IO Camera
newCamera p l = do
    state <- newIORef (CameraState p l Linear.identity Linear.identity True)
    ptr <- Foreign.mallocForeignPtr
    let camera = Camera state ptr
    _ <- updateCameraBlock camera
    return camera

getCameraMatrices :: Camera -> IO (Mat4, Mat4)
getCameraMatrices (Camera cameraRef _) =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p l _ _ True) =
        let pm = projectionMatrix p
            vm = lookAtMatrix l
        in (CameraState p l pm vm False, (pm, vm))
    f a @ (CameraState _ _ pm vm False) = (a, (pm, vm))

updateCameraBlock :: Camera -> IO CameraBlock
updateCameraBlock (Camera cameraRef ptr) = do
    (updated, block) <- atomicModifyIORef' cameraRef f
    when updated $
        Foreign.withForeignPtr ptr (`Foreign.poke` Std140 block)
    return block
    where
    f (CameraState p l _ _ True) =
        let pm = projectionMatrix p
            vm = lookAtMatrix l
            LookAt eye _ _ = l
        in (CameraState p l pm vm False, (True, CameraBlock pm vm eye))
    f a @ (CameraState _ l pm vm False) =
        let LookAt eye _ _ = l
        in (a, (False, CameraBlock pm vm eye))

projectionMatrix :: Projection -> Mat4
projectionMatrix (PerspectiveProjection (Perspective fov aspect near far)) = Linear.perspective fov aspect near far
projectionMatrix (OrthographicProjection (Orthographic left right bottom top near far)) = Linear.ortho left right bottom top near far

lookAtMatrix :: LookAt -> Mat4
lookAtMatrix (LookAt eye center up) = Linear.lookAt eye center up

updateProjection :: Camera -> Projection -> IO ()
updateProjection (Camera cameraRef _) p =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState _ l pm vm _) = (CameraState p l pm vm True, ())

updateLookAt :: Camera -> LookAt -> IO ()
updateLookAt (Camera cameraRef _) l =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p _ pm vm _) = (CameraState p l pm vm True, ())

getCameraLookAt :: Camera -> IO LookAt
getCameraLookAt (Camera cameraRef _) = cameraLookAt <$> readIORef cameraRef

getCameraProjection :: Camera -> IO Projection
getCameraProjection (Camera cameraRef _) = cameraProjection <$> readIORef cameraRef
