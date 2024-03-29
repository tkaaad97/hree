module Hree.CameraControl.SphericalControl
    ( SphericalControl(..)
    , SphericalControlMode(..)
    , SphericalControlSettings(..)
    , defaultSphericalControlSettings
    , newSphericalControl
    , newSphericalControlDefault
    , enterSphericalControl
    , updateSphericalControl
    , leaveSphericalControl
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Hree.Camera
import Linear (Additive((^+^), (^-^)), Quaternion(..), V2(..), V3(..), (*^))
import qualified Linear

data SphericalControlMode =
    SphericalControlModeOrbit |
    SphericalControlModeZoom
    deriving (Show, Eq)

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
    { currentControlMode  :: !SphericalControlMode
    , prevControlPosition :: !(V2 Float)
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

enterSphericalControl :: SphericalControl -> SphericalControlMode -> V2 Float -> IO ()
enterSphericalControl (SphericalControl _ _ ref) mode cp = do
    let s = SphericalControlState mode cp
    writeIORef ref (Just s)

updateSphericalControl :: SphericalControl -> V2 Float -> IO ()
updateSphericalControl (SphericalControl settings camera ref) cp1 = do
    la <- getCameraLookAt camera
    updatedLookAt <- atomicModifyIORef' ref (go la)
    maybe (return ()) (updateLookAt camera) updatedLookAt
    where
    go _ Nothing = (Nothing, Nothing)
    go (LookAt eye center up) (Just (SphericalControlState SphericalControlModeOrbit cp0)) =
        let (V2 dx dy) = cp0 ^-^ cp1
            offset = eye ^-^ center
            quat = rotationBetween up (V3 0 1 0)
            quatInv = Linear.conjugate quat
            offset' = quat `Linear.rotate` offset
            (r, phi0, theta0) = calcSpherical offset'
            deltaPhi = dy * polarAngleFactor settings
            deltaTheta = dx * azimuthAngleFactor settings
            phi = min pi $ max 0 (phi0 + deltaPhi)
            theta = theta0 + deltaTheta
            qp = Linear.axisAngle (V3 1 0 0) phi
            qa = Linear.axisAngle (V3 0 1 0) theta
            v = (quatInv * qa * qp) `Linear.rotate` (r *^ V3 0 1 0)
            eye' = v ^+^ center
        in (Just (SphericalControlState SphericalControlModeOrbit cp1), Just (LookAt eye' center up))

    go (LookAt eye center up) (Just (SphericalControlState SphericalControlModeZoom cp0)) =
        let V2 _ y0 = cp0
            V2 _ y1 = cp1
            dy = y1 - y0
            f = 1.0 + dy * 10.0
            offset = eye ^-^ center
            offset' = f *^ offset
            eye' = offset' ^+^ center
        in (Just (SphericalControlState SphericalControlModeZoom cp1), Just (LookAt eye' center up))

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
    theta = acos $ v0' `Linear.dot` v1'
    q = Linear.axisAngle axis theta

calcSpherical :: V3 Float -> (Float, Float, Float)
calcSpherical v @ (V3 x y z) =
    if Linear.nearZero r
        then (r, 0, 0)
        else (r, acos (y / r), atan2 x z)
    where
    r = Linear.norm v
