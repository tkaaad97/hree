{-# LANGUAGE GADTs #-}
module Graphics.Hree.Animation
    ( Animation(..)
    , Interpolation(..)
    , KeyFrames(..)
    , NodePath(..)
    , Target(..)
    , lowerBound
    , interpolateStep
    , interpolateLinear
    ) where

import qualified Data.Vector.Unboxed as UV (Unbox, Vector, head, last, length,
                                            (!))
import Graphics.Hree.Math (Quaternion, Vec3)
import Graphics.Hree.Types (NodeId)
import Linear (Additive(..), (^*))

data KeyFrames a = KeyFrames
    { keyFrameTimepoints :: !(UV.Vector Float)
    , keyFrameValues     :: !(UV.Vector a)
    } deriving (Show, Eq)

data NodePath a where
    NodePathTranslation :: NodePath Vec3
    NodePathRotation :: NodePath Quaternion
    NodePathScale :: NodePath Vec3

data Target a =
    TargetNode !NodeId !(NodePath a)
    deriving (Show, Eq)

data Interpolation =
    InterpolationLinear |
    InterpolationStep |
    InterpolationCubicSpline
    deriving (Show, Eq)

data Animation a = Animation
    { animationTarget        :: !(Target a)
    , animationInterpolation :: !Interpolation
    , animationKeyframes     :: !(KeyFrames a)
    } deriving (Show, Eq)

instance Show (NodePath a) where
    show NodePathTranslation = "NodePathTranslation"
    show NodePathRotation    = "NodePathRotation"
    show NodePathScale       = "NodePathScale"

instance Eq (NodePath a) where
    (==) NodePathTranslation NodePathTranslation = True
    (==) NodePathRotation NodePathRotation       = True
    (==) NodePathScale NodePathScale             = True
    (==) _ _                                     = False

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

interpolateStep :: UV.Unbox a => KeyFrames a -> Float -> a
interpolateStep (KeyFrames timepoints values) t =
    case lowerBound timepoints t of
        Nothing -> UV.last values
        Just 0  -> UV.head values
        Just n  -> values UV.! (n - 1)

interpolateLinear :: (Fractional a, UV.Unbox (f a), Additive f) => KeyFrames (f a) -> Float -> f a
interpolateLinear (KeyFrames timepoints values) t =
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
