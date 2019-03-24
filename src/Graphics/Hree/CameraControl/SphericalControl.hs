module Graphics.Hree.CameraControl.SphericalControl
    ( SphericalControl
    , SphericalControlSettings(..)
    , defaultSphericalControlSettings
    , newSphericalControl
    , newSphericalControlDefault
    , enterSphericalControl
    , updateSphericalControl
    , leaveSphericalControl
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Maybe (maybe)
import Graphics.Hree.Camera
import Linear (Additive((^+^), (^-^)), Quaternion(..), V2(..), V3(..))
import qualified Linear

data SphericalControl = SphericalControl
    { orbitControlSettings :: !SphericalControlSettings
    , orbitControlCamera   :: !Camera
    , orbitControlState    :: !(IORef (Maybe SphericalControlState))
    }

data SphericalControlSettings = SphericalControlSettings
    { polarAngleFactor   :: !Float
    , azimuthAngleFactor :: !Float
    } deriving (Show, Eq)

data SphericalControlState = SphericalControlState
    { prevControlPosition :: !(V2 Float)
    } deriving (Show, Eq)

defaultSphericalControlSettings :: SphericalControlSettings
defaultSphericalControlSettings = SphericalControlSettings a a
    where
    a = 2 * pi

newSphericalControl :: SphericalControlSettings -> Camera -> IO SphericalControl
newSphericalControl settings camera =
    SphericalControl settings camera <$> newIORef Nothing

newSphericalControlDefault :: Camera -> IO SphericalControl
newSphericalControlDefault = newSphericalControl defaultSphericalControlSettings

enterSphericalControl :: SphericalControl -> V2 Float -> IO ()
enterSphericalControl (SphericalControl settings camera ref) cp = do
    LookAt eye center up <- getCameraLookAt camera
    let s = SphericalControlState cp
    writeIORef ref (Just s)

updateSphericalControl :: SphericalControl -> V2 Float -> IO ()
updateSphericalControl (SphericalControl settings camera ref) cp1 = do
    la <- getCameraLookAt camera
    updatedLookAt <- atomicModifyIORef' ref (go la)
    maybe (return ()) (updateLookAt camera) updatedLookAt
    where
    go _ Nothing = (Nothing, Nothing)
    go (LookAt eye center up) (Just (SphericalControlState cp0)) =
        let (V2 dx dy) = cp1 ^-^ cp0
            deltaPhi = dx * polarAngleFactor settings
            deltaTheta = dy * azimuthAngleFactor settings
            offset = eye ^-^ center
            axis = if Linear.nearZero . abs $ up `Linear.dot` offset
                then rotationBetween (V3 0 1 0) up `Linear.rotate` V3 (-1) 0 0
                else Linear.normalize $ offset `Linear.cross` up
            qa = Linear.axisAngle up deltaPhi
            qp = Linear.axisAngle axis deltaTheta
            v = (qa * qp) `Linear.rotate` offset
            eye' = v ^+^ center
        in (Just (SphericalControlState cp1), Just (LookAt eye' center up))

leaveSphericalControl :: SphericalControl -> V2 Float -> IO ()
leaveSphericalControl control cp1 = do
    updateSphericalControl control cp1
    writeIORef (orbitControlState control) Nothing

rotationBetween :: V3 Float -> V3 Float -> Quaternion Float
rotationBetween v0 v1 = q
    where
    v0' = Linear.normalize v0
    v1' = Linear.normalize v1
    axis = v0' `Linear.cross` v1'
    theta = v0 `Linear.dot` v1
    q = Linear.axisAngle axis theta
