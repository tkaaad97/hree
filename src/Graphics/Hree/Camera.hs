module Graphics.Hree.Camera
    ( Camera
    , LookAt(..)
    , Projection(..)
    , lookAt
    , newCamera
    , orthographic
    , perspective
    , getCameraLookAt
    , getCameraMatrix
    , getCameraProjection
    , updateProjection
    , updateLookAt
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Linear (M44, V3(..), V4(..), (!*!), (^-^))
import qualified Linear
import qualified Linear (identity, lookAt, normalize, ortho, perspective)

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
    , orthographicBottom :: !Float
    , orthographicTop    :: !Float
    , orthographicNear   :: !Float
    , orthographicFar    :: !Float
    } deriving (Show, Eq)

perspective :: Float -> Float -> Float -> Float -> Projection
perspective = Perspective

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> Projection
orthographic = Orthographic

lookAt :: V3 Float -> V3 Float -> V3 Float -> LookAt
lookAt eye center up = LookAt eye center (Linear.normalize up)

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
projectionMatrix (Orthographic left right bottom top near far) = Linear.ortho left right bottom top near far
projectionMatrix (Perspective fov aspect near far) =
    let tanHalfFovy = tan $ fov * 0.5
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        z = far / (near - far)
        w = near * far / (near - far)
    in V4
        (V4 x 0 0 0)
        (V4 0 y 0 0)
        (V4 0 0 z w)
        (V4 0 0 (-1) 0)

lookAtMatrix :: LookAt -> M44 Float
lookAtMatrix (LookAt eye center up) =
    let za @ (V3 zax zay zaz) = Linear.normalize $ eye ^-^ center
        xa @ (V3 xax xay xaz) = Linear.normalize $ Linear.cross up za
        ya @ (V3 yax yay yaz) = Linear.cross za xa
        xd = - Linear.dot xa eye
        yd = - Linear.dot ya eye
        zd = - Linear.dot za eye
    in V4
        (V4 xax xay xaz xd)
        (V4 yax yay yaz yd)
        (V4 zax zay zaz zd)
        (V4 0 0 0 1)

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

getCameraLookAt :: Camera -> IO LookAt
getCameraLookAt (Camera cameraRef) = cameraLookAt <$> readIORef cameraRef

getCameraProjection :: Camera -> IO Projection
getCameraProjection (Camera cameraRef) = cameraProjection <$> readIORef cameraRef
