module Hree.Camera
    ( Camera
    , CameraBlock
    , LookAt(..)
    , Orthographic(..)
    , Perspective(..)
    , Projection(..)
    , lookAt
    , newCamera
    , orthographic
    , perspective
    , getCameraLookAt
    , getCameraProjection
    , updateCameraBlock
    , updateProjection
    , updateLookAt
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Hree.GL.Block (Block(..))
import Hree.GL.Types (Mat4, Vec3)
import qualified Linear (identity, lookAt, normalize, ortho, perspective)

newtype Camera = Camera (IORef CameraState)

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
    let camera = Camera state
    _ <- updateCameraBlock camera
    return camera

updateCameraBlock :: Camera -> IO CameraBlock
updateCameraBlock (Camera cameraRef) =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p l _ _ True) =
        let pm = projectionMatrix p
            vm = lookAtMatrix l
            block = CameraBlock pm vm (lookAtEye l)
        in (CameraState p l pm vm False, block)
    f a @ (CameraState _ l pm vm False) =
        let block = CameraBlock pm vm (lookAtEye l)
        in (a, block)

projectionMatrix :: Projection -> Mat4
projectionMatrix (PerspectiveProjection (Perspective fov aspect near far)) = Linear.perspective fov aspect near far
projectionMatrix (OrthographicProjection (Orthographic left right bottom top near far)) = Linear.ortho left right bottom top near far

lookAtMatrix :: LookAt -> Mat4
lookAtMatrix (LookAt eye center up) = Linear.lookAt eye center up

updateProjection :: Camera -> Projection -> IO ()
updateProjection (Camera cameraRef) p =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState _ l pm vm _) = (CameraState p l pm vm True, ())

updateLookAt :: Camera -> LookAt -> IO ()
updateLookAt (Camera cameraRef) l =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p _ pm vm _) = (CameraState p l pm vm True, ())

getCameraLookAt :: Camera -> IO LookAt
getCameraLookAt (Camera cameraRef) = cameraLookAt <$> readIORef cameraRef

getCameraProjection :: Camera -> IO Projection
getCameraProjection (Camera cameraRef) = cameraProjection <$> readIORef cameraRef
