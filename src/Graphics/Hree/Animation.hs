{-# LANGUAGE GADTs #-}
module Graphics.Hree.Animation
    ( Animation(..)
    , Channel(..)
    , Interpolation(..)
    , KeyFrames(..)
    , Track(..)
    , lowerBound
    , interpolate
    ) where

import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Unboxed as UV (Unbox, Vector, head, last, length,
                                            (!))
import Graphics.Hree.Math (Quaternion, Vec3)
import Graphics.Hree.Types (NodeId)
import Linear (Additive(..), (^*))

data Interpolation =
    InterpolationLinear |
    InterpolationStep
    deriving (Show, Eq)

data KeyFrames a = KeyFrames
    { keyFramesInterpolation :: !Interpolation
    , keyFramesTimepoints    :: !(UV.Vector Float)
    , keyFramesValues        :: !(UV.Vector a)
    } deriving (Show, Eq)

data Channel = Chennel
    { channelNode   :: !NodeId
    , channelTracks :: !(BV.Vector Track)
    } deriving (Show, Eq)

data Track =
    TrackNodeTranslation !(KeyFrames Vec3) |
    TrackNodeRotation !(KeyFrames Quaternion) |
    TrackNodeScale !(KeyFrames Vec3)
    deriving (Show, Eq)

data Animation a = Animation
    { animationChannels :: !(BV.Vector Channel)
    , animationDuration :: !Float
    } deriving (Show, Eq)

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

interpolate :: (Fractional a, UV.Unbox (f a), Additive f) => KeyFrames (f a) -> Float -> f a
interpolate (KeyFrames InterpolationStep timepoints values) t =
    case lowerBound timepoints t of
        Nothing -> UV.last values
        Just 0  -> UV.head values
        Just n  -> values UV.! (n - 1)
interpolate (KeyFrames InterpolationLinear timepoints values) t =
    case lowerBound timepoints t of
        Nothing -> UV.last values
        Just 0  -> UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = v0 ^+^ (v1 ^-^ v0) ^* realToFrac ((t - t0) / (t1 - t0))
            in v
