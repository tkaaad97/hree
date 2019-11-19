module Graphics.Hree.Camera
    ( Camera
    , LookAt(..)
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

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Linear (M44, V3)
import qualified Linear (identity, lookAt, normalize, ortho, perspective)

newtype Camera = Camera (IORef CameraState)
    deriving Eq

data CameraState = CameraState
    { cameraProjection            :: !Projection
    , cameraLookAt                :: !LookAt
    , cameraStateProjectionMatrix :: !(M44 Float)
    , cameraStateViewMatrix       :: !(M44 Float)
    , cameraStateNeedUpdate       :: !Bool
    } deriving (Show, Eq)

data LookAt = LookAt
    { lookAtEye    :: !(V3 Float)
    , lookAtCenter :: !(V3 Float)
    , lookAtUp     :: !(V3 Float)
    } deriving (Show, Eq)

data Projection =
    Perspective
        !Float -- perspectiveFov
        !Float -- perspectiveAspect
        !Float -- perspectiveNear
        !Float -- perspectiveFar
    |
    Orthographic
        !Float -- orthographicLeft
        !Float -- orthographicRight
        !Float -- orthographicBottom
        !Float -- orthographicTop
        !Float -- orthographicNear
        !Float -- orthographicFar
    deriving (Show, Eq)

perspective :: Float -> Float -> Float -> Float -> Projection
perspective = Perspective

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> Projection
orthographic = Orthographic

lookAt :: V3 Float -> V3 Float -> V3 Float -> LookAt
lookAt eye center up = LookAt eye center (Linear.normalize up)

newCamera :: Projection -> LookAt -> IO Camera
newCamera p l = Camera <$> newIORef (CameraState p l Linear.identity Linear.identity True)

getCameraMatrices :: Camera -> IO (M44 Float, M44 Float)
getCameraMatrices (Camera cameraRef) =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p l _ _ True) =
        let pm = projectionMatrix p
            vm = lookAtMatrix l
        in (CameraState p l pm vm False, (pm, vm))
    f a @ (CameraState _ _ pm vm False) = (a, (pm, vm))

projectionMatrix :: Projection -> M44 Float
projectionMatrix (Perspective fov aspect near far) = Linear.perspective fov aspect near far
projectionMatrix (Orthographic left right bottom top near far) = Linear.ortho left right bottom top near far

lookAtMatrix :: LookAt -> M44 Float
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
