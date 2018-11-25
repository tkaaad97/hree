module Graphics.Hree.Camera
    ( Camera
    , newCamera
    , perspective
    , orthographic
    , getCameraMatrix
    , updateProjection
    , updateLookAt
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Linear (M44, V3, (!*!))
import qualified Linear (identity, lookAt, ortho, perspective)

newtype Camera = Camera
    { unCamera :: IORef CameraState
    } deriving Eq

data CameraState = CameraState
    { cameraProjection      :: !Projection
    , cameraLookAt          :: !LookAt
    , cameraStateMatrix     :: !(M44 Float)
    , cameraStateNeedUpdate :: !Bool
    } deriving (Show, Eq)

data LookAt = LookAt
    { lookAtEye    :: !(V3 Float)
    , lookAtCenter :: !(V3 Float)
    , lookAtUp     :: !(V3 Float)
    } deriving (Show, Eq)

data Projection =
    Perspective
    { perspectiveFov    :: !Float
    , perspectiveAspect :: !Float
    , perspectiveNear   :: !Float
    , perspectiveFar    :: !Float
    } |
    Orthographic
    { orthographicLeft   :: !Float
    , orthographicRight  :: !Float
    , orthographicTop    :: !Float
    , orthographicBottom :: !Float
    , orthographicNear   :: !Float
    , orthographicFar    :: !Float
    } deriving (Show, Eq)

perspective :: Float -> Float -> Float -> Float -> Projection
perspective = Perspective

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> Projection
orthographic = Orthographic

newCamera :: Projection -> LookAt -> IO Camera
newCamera p l = Camera <$> newIORef (CameraState p l Linear.identity True)

getCameraMatrix :: Camera -> IO (M44 Float)
getCameraMatrix (Camera cameraRef) =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p l _ True) =
        let m = projectionMatrix p !*! lookAtMatrix l
        in (CameraState p l m False, m)
    f a @ (CameraState _ _ m False) = (a, m)

projectionMatrix :: Projection -> M44 Float
projectionMatrix (Perspective fov aspect near far) = Linear.perspective fov aspect near far
projectionMatrix (Orthographic left right top bottom near far) = Linear.ortho left right top bottom near far

lookAtMatrix :: LookAt -> M44 Float
lookAtMatrix (LookAt eye center up) = Linear.lookAt eye center up

updateProjection :: Camera -> Projection -> IO ()
updateProjection (Camera cameraRef) p =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState _ l m _) = (CameraState p l m True, ())

updateLookAt :: Camera -> LookAt -> IO ()
updateLookAt (Camera cameraRef) l =
    atomicModifyIORef' cameraRef f
    where
    f (CameraState p _ m _) = (CameraState p l m True, ())
