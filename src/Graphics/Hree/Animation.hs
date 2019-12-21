{-# LANGUAGE GADTs #-}
module Graphics.Hree.Animation
    ( Animation(..)
    , Channel(..)
    , Interpolation(..)
    , KeyFrames(..)
    , Track(..)
    , addChannel
    , addKeyFrames
    , animation
    , applyAnimation
    , applyChannel
    , channel
    , channelDuration
    , interpolate
    , interpolateLinear
    , interpolateStep
    , interpolateQuaternion
    , linearRotation
    , linearScale
    , linearTranslation
    , lowerBound
    , singleAnimation
    , singleChannel
    , stepRotation
    , stepScale
    , stepTranslation
    ) where

import qualified Data.Vector as BV (Vector, foldl, map, mapM_, maximum, null,
                                    singleton, snoc)
import qualified Data.Vector.Unboxed as UV (Unbox, Vector, head, last, length,
                                            null, (!))
import Graphics.Hree.Math (Quaternion, Transform(..), Vec3)
import Graphics.Hree.Scene (applyTransformToNode)
import Graphics.Hree.Types (NodeId, Scene)
import Linear (Additive(..), slerp, (^*))

data Interpolation =
    InterpolationLinear |
    InterpolationStep
    deriving (Show, Eq)

data Track =
    TrackNodeTranslation !(UV.Vector Vec3) |
    TrackNodeRotation !(UV.Vector Quaternion) |
    TrackNodeScale !(UV.Vector Vec3)
    deriving (Show, Eq)

data KeyFrames = KeyFrames
    { keyFramesInterpolation :: !Interpolation
    , keyFramesTimepoints    :: !(UV.Vector Float)
    , keyFramesTrack         :: !Track
    } deriving (Show, Eq)

data Channel = Channel
    { channelNode      :: !NodeId
    , channelKeyFrames :: !(BV.Vector KeyFrames)
    } deriving (Show, Eq)

data Animation = Animation
    { animationChannels :: !(BV.Vector Channel)
    , animationDuration :: !Float
    } deriving (Show, Eq)

stepTranslation :: UV.Vector Float -> UV.Vector Vec3 -> KeyFrames
stepTranslation times values = KeyFrames InterpolationStep times (TrackNodeTranslation values)

stepRotation :: UV.Vector Float -> UV.Vector Quaternion -> KeyFrames
stepRotation times values = KeyFrames InterpolationStep times (TrackNodeRotation values)

stepScale :: UV.Vector Float -> UV.Vector Vec3 -> KeyFrames
stepScale times values = KeyFrames InterpolationStep times (TrackNodeScale values)

linearTranslation :: UV.Vector Float -> UV.Vector Vec3 -> KeyFrames
linearTranslation times values = KeyFrames InterpolationLinear times (TrackNodeTranslation values)

linearRotation :: UV.Vector Float -> UV.Vector Quaternion -> KeyFrames
linearRotation times values = KeyFrames InterpolationLinear times (TrackNodeRotation values)

linearScale :: UV.Vector Float -> UV.Vector Vec3 -> KeyFrames
linearScale times values = KeyFrames InterpolationLinear times (TrackNodeScale values)

singleChannel :: NodeId -> KeyFrames -> Channel
singleChannel nodeId keyFrames = Channel nodeId (BV.singleton keyFrames)

singleAnimation :: NodeId -> KeyFrames -> Animation
singleAnimation nodeId keyFrames =
    let ch = singleChannel nodeId keyFrames
        duration = channelDuration ch
    in Animation (BV.singleton ch) duration

channelDuration :: Channel -> Float
channelDuration ch =
    let keys = channelKeyFrames ch
        timepoints = BV.map keyFramesTimepoints keys
        durations = BV.map (\v -> if UV.null v then 0 else UV.last v) timepoints
    in if BV.null durations then 0 else BV.maximum durations

addChannel :: Animation -> Channel -> Animation
addChannel a c =
    let cs = animationChannels a `BV.snoc` c
    in a { animationChannels = cs }

addKeyFrames :: Channel -> KeyFrames -> Channel
addKeyFrames c k =
    let ks = channelKeyFrames c `BV.snoc` k
    in c { channelKeyFrames = ks }

channel :: NodeId -> BV.Vector KeyFrames -> Channel
channel = Channel

animation :: BV.Vector Channel -> Float -> Animation
animation = Animation

lowerBound :: UV.Vector Float -> Float -> Maybe Int
lowerBound vec a =
    go 0 (middle 0 (size - 1)) (size - 1)
    where
    size = UV.length vec
    middle imin imax = imin + (imax - imin) `div` 2
    go imin imid imax
        | vec UV.! imid >= a =
            if imin == imid
                then Just imid
                else go imin (middle imin imid) imid
        | otherwise =
            if imid == imax
                then Nothing
                else go (imid + 1) (middle (imid + 1) imax) imax

interpolate :: KeyFrames -> Float -> Transform -> Transform
interpolate (KeyFrames InterpolationStep timepoints (TrackNodeTranslation values)) t trans =
    trans { transformTranslation = interpolateStep timepoints values t, transformUpdated = True }
interpolate (KeyFrames InterpolationStep timepoints (TrackNodeRotation values)) t trans =
    trans { transformQuaternion = interpolateStep timepoints values t, transformUpdated = True }
interpolate (KeyFrames InterpolationStep timepoints (TrackNodeScale values)) t trans =
    trans { transformScale = interpolateStep timepoints values t, transformUpdated = True }
interpolate (KeyFrames InterpolationLinear timepoints (TrackNodeTranslation values)) t trans =
    trans { transformTranslation = interpolateLinear timepoints values t, transformUpdated = True }
interpolate (KeyFrames InterpolationLinear timepoints (TrackNodeRotation values)) t trans =
    trans { transformQuaternion = interpolateQuaternion timepoints values t, transformUpdated = True }
interpolate (KeyFrames InterpolationLinear timepoints (TrackNodeScale values)) t trans =
    trans { transformScale = interpolateLinear timepoints values t, transformUpdated = True }

interpolateStep :: (Additive f, UV.Unbox (f a), Num a) => UV.Vector Float -> UV.Vector (f a) -> Float -> f a
interpolateStep timepoints values t =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  -> values UV.! (n - 1)

interpolateLinear :: (Additive f, Fractional a, UV.Unbox (f a)) => UV.Vector Float -> UV.Vector (f a) -> Float -> f a
interpolateLinear timepoints values t =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = v0 ^+^ (v1 ^-^ v0) ^* realToFrac ((t - t0) / (t1 - t0))
            in v

interpolateQuaternion :: UV.Vector Float -> UV.Vector Quaternion -> Float -> Quaternion
interpolateQuaternion timepoints values t =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = slerp v0 v1 ((t - t0) / (t1 - t0))
            in v

applyChannel :: Scene -> Channel -> Float -> IO ()
applyChannel scene (Channel nodeId keys) t =
    let f = BV.foldl (\a k -> interpolate k t . a) id keys
    in applyTransformToNode scene nodeId f

applyAnimation :: Scene -> Animation -> Float -> IO ()
applyAnimation scene (Animation channels _) t =
    BV.mapM_ (flip (applyChannel scene) t) channels
