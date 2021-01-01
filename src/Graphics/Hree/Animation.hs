{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Graphics.Hree.Animation
    ( AnimationChannel(..)
    , AnimationClip(..)
    , Interpolation(..)
    , KeyFrames(..)
    , TransformChannel(..)
    , TransformTrack(..)
    , VariationChannel(..)
    , VariationTrack(..)
    , animationClip
    , animationClipTransform
    , animationClipVariation
    , applyAnimationClip
    , interpolateLinear
    , interpolateQuaternion
    , interpolateStep
    , interpolateTransform
    , linearRotation
    , linearScale
    , linearTranslation
    , cubicSplineRotation
    , cubicSplineScale
    , cubicSplineTranslation
    , lowerBound
    , singleTransformChannel
    , singleTransformClip
    , singleVariationClip
    , stepMesh
    , stepRotation
    , stepScale
    , stepTranslation
    , transformChannel
    , transformClip
    ) where

import Chronos (Timespan(..))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as BV (Vector, foldl, map, mapM_, maximum, null,
                                    singleton)
import qualified Data.Vector.Generic as GV (Vector(..), head, last, null, (!),
                                            (!?))
import qualified Data.Vector.Storable as SV (Vector)
import qualified Data.Vector.Unboxed as UV (Vector, head, last, length, null,
                                            (!))
import Graphics.Hree.Math (Quaternion, Transform(..), Vec3)
import Graphics.Hree.Scene (applyTransformToNode, updateNode)
import Graphics.Hree.Types (MeshId, Node(nodeMesh), NodeId, Scene)
import Linear (Additive(..), slerp, (*^), (^/))

data Interpolation =
    InterpolationLinear |
    InterpolationStep |
    InterpolationCubicSpline
    deriving (Show, Eq)

data TransformTrack =
    TransformTrackTranslation !(UV.Vector Vec3) |
    TransformTrackRotation !(UV.Vector Quaternion) |
    TransformTrackScale !(UV.Vector Vec3)
    deriving (Show, Eq)

data VariationTrack a where
    VariationTrackDiscrete :: (GV.Vector v a) => v a -> VariationTrack (v a)
    VariationTrackVector :: (Additive f, Fractional a, GV.Vector v (f a)) => v (f a) -> VariationTrack (v (f a))

instance Show a => Show (VariationTrack a) where
    show (VariationTrackDiscrete a) = "VariationTrackDiscrete {" ++ show a ++ "}"
    show (VariationTrackVector a) = "VariationTrackVector {" ++ show a ++ "}"

instance Eq a => Eq (VariationTrack a) where
    (==) (VariationTrackDiscrete a) (VariationTrackDiscrete b) = a == b
    (==) (VariationTrackVector a) (VariationTrackVector b)     = a == b
    (==) _ _                                                   = False

data KeyFrames track = KeyFrames
    { keyFramesInterpolation :: !Interpolation
    , keyFramesTimepoints    :: !(UV.Vector Int64)
    , keyFramesTrack         :: !track
    } deriving (Show, Eq)

data TransformChannel = TransformChannel
    { transformChannelTarget :: !NodeId
    , transformChannelTracks :: !(BV.Vector (KeyFrames TransformTrack))
    } deriving (Show, Eq)

data VariationChannel = forall a v. (Show (v a)) => VariationChannel
    { variationChannelSetter :: !(a -> IO ())
    , variationChannelTrack  :: !(KeyFrames (VariationTrack (v a)))
    }

instance Show VariationChannel where
    show (VariationChannel _ track) = "VariationChannel { variationChannelSetter, variationChannelTrack = " ++ show track ++ " }"

data AnimationChannel =
    AnimationChannelTransform !TransformChannel |
    AnimationChannelVariation !VariationChannel
    deriving (Show)

data AnimationClip = AnimationClip
    { animationClipChannels :: !(BV.Vector AnimationChannel)
    , animationClipDuration :: !Timespan
    } deriving (Show)

stepTranslation :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
stepTranslation times values = KeyFrames InterpolationStep times (TransformTrackTranslation values)

stepRotation :: UV.Vector Int64 -> UV.Vector Quaternion -> KeyFrames TransformTrack
stepRotation times values = KeyFrames InterpolationStep times (TransformTrackRotation values)

stepScale :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
stepScale times values = KeyFrames InterpolationStep times (TransformTrackScale values)

linearTranslation :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
linearTranslation times values = KeyFrames InterpolationLinear times (TransformTrackTranslation values)

linearRotation :: UV.Vector Int64 -> UV.Vector Quaternion -> KeyFrames TransformTrack
linearRotation times values = KeyFrames InterpolationLinear times (TransformTrackRotation values)

linearScale :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
linearScale times values = KeyFrames InterpolationLinear times (TransformTrackScale values)

cubicSplineTranslation :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
cubicSplineTranslation times values = KeyFrames InterpolationCubicSpline times (TransformTrackTranslation values)

cubicSplineRotation :: UV.Vector Int64 -> UV.Vector Quaternion -> KeyFrames TransformTrack
cubicSplineRotation times values = KeyFrames InterpolationCubicSpline times (TransformTrackRotation values)

cubicSplineScale :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames TransformTrack
cubicSplineScale times values = KeyFrames InterpolationCubicSpline times (TransformTrackScale values)

transformChannel :: NodeId -> BV.Vector (KeyFrames TransformTrack) -> AnimationChannel
transformChannel nodeId keyFrames = AnimationChannelTransform (TransformChannel nodeId keyFrames)

transformClip :: NodeId -> BV.Vector (KeyFrames TransformTrack) -> AnimationClip
transformClip nodeId keyFrames =
    let channels = BV.singleton $ transformChannel nodeId keyFrames
        d = channelsDuration channels
    in AnimationClip channels d

singleTransformChannel :: NodeId -> KeyFrames TransformTrack -> AnimationChannel
singleTransformChannel nodeId key =
    AnimationChannelTransform (TransformChannel nodeId (BV.singleton key))

singleTransformClip :: NodeId -> KeyFrames TransformTrack -> AnimationClip
singleTransformClip nodeId key =
    let duration = keyFrameTrackDuration key
        channel = AnimationChannelTransform (TransformChannel nodeId (BV.singleton key))
    in AnimationClip (BV.singleton channel) duration

singleVariationClip :: (Show (v a)) => (a -> IO ()) -> KeyFrames (VariationTrack (v a)) -> AnimationClip
singleVariationClip setter key =
    let duration = keyFrameTrackDuration key
        channel = AnimationChannelVariation (VariationChannel setter key)
    in AnimationClip (BV.singleton channel) duration

animationClip :: BV.Vector AnimationChannel -> AnimationClip
animationClip channels =
    let duration = channelsDuration channels
    in AnimationClip channels duration

animationClipTransform :: BV.Vector TransformChannel -> AnimationClip
animationClipTransform channels =
    let channels' = BV.map AnimationChannelTransform channels
        duration = channelsDuration channels'
    in AnimationClip channels' duration

animationClipVariation :: BV.Vector VariationChannel -> AnimationClip
animationClipVariation channels =
    let channels' = BV.map AnimationChannelVariation channels
        duration = channelsDuration channels'
    in AnimationClip channels' duration

stepMesh :: Scene -> NodeId -> UV.Vector Int64 -> SV.Vector MeshId -> AnimationClip
stepMesh scene nodeId times values = AnimationClip (BV.singleton (AnimationChannelVariation $ VariationChannel setter key)) duration
    where
    setter mesh = void $ updateNode scene nodeId $ \n -> n { nodeMesh = Just mesh }
    key = KeyFrames InterpolationStep times (VariationTrackDiscrete values)
    duration = keyFrameTracksDuration (BV.singleton key)

keyFrameTrackDuration :: KeyFrames a -> Timespan
keyFrameTrackDuration k =
    let timepoints = keyFramesTimepoints k
        duration = if UV.null timepoints then 0 else UV.last timepoints
    in Timespan duration

keyFrameTracksDuration :: BV.Vector (KeyFrames a) -> Timespan
keyFrameTracksDuration keys =
    if BV.null keys
        then Timespan 0
        else BV.maximum . BV.map keyFrameTrackDuration $ keys

channelDuration :: AnimationChannel -> Timespan
channelDuration (AnimationChannelTransform (TransformChannel _ keys)) = keyFrameTracksDuration keys
channelDuration (AnimationChannelVariation (VariationChannel _ key)) = keyFrameTrackDuration key

channelsDuration :: BV.Vector AnimationChannel -> Timespan
channelsDuration channels =
    if BV.null channels
        then Timespan 0
        else BV.maximum . BV.map channelDuration $ channels

lowerBound :: UV.Vector Int64 -> Int64 -> Maybe Int
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

interpolateTransform :: KeyFrames TransformTrack -> Timespan -> Transform -> Transform
interpolateTransform (KeyFrames InterpolationStep timepoints (TransformTrackTranslation values)) t trans =
    trans { transformTranslation = fromMaybe (transformTranslation trans) $ interpolateStep timepoints values t }
interpolateTransform (KeyFrames InterpolationStep timepoints (TransformTrackRotation values)) t trans =
    trans { transformQuaternion = fromMaybe (transformQuaternion trans) $ interpolateStep timepoints values t }
interpolateTransform (KeyFrames InterpolationStep timepoints (TransformTrackScale values)) t trans =
    trans { transformScale = fromMaybe (transformScale trans) $ interpolateStep timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TransformTrackTranslation values)) t trans =
    trans { transformTranslation = fromMaybe (transformTranslation trans) $ interpolateLinear timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TransformTrackRotation values)) t trans =
    trans { transformQuaternion = fromMaybe (transformQuaternion trans) $ interpolateQuaternion timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TransformTrackScale values)) t trans =
    trans { transformScale = fromMaybe (transformScale trans) $ interpolateLinear timepoints values t }
interpolateTransform (KeyFrames InterpolationCubicSpline timepoints (TransformTrackTranslation values)) t trans =
    trans { transformTranslation = fromMaybe (transformTranslation trans) $ interpolateCubicSpline timepoints values t }
interpolateTransform (KeyFrames InterpolationCubicSpline timepoints (TransformTrackRotation values)) t trans =
    trans { transformQuaternion = fromMaybe (transformQuaternion trans) $ interpolateCubicSpline timepoints values t }
interpolateTransform (KeyFrames InterpolationCubicSpline timepoints (TransformTrackScale values)) t trans =
    trans { transformScale = fromMaybe (transformScale trans) $ interpolateCubicSpline timepoints values t }

interpolateVariation :: KeyFrames (VariationTrack (v a)) -> Timespan -> Maybe a
interpolateVariation (KeyFrames _ timepoints (VariationTrackDiscrete values)) t =
    interpolateStep timepoints values t
interpolateVariation (KeyFrames InterpolationStep timepoints (VariationTrackVector values)) t =
    interpolateStep timepoints values t
interpolateVariation (KeyFrames InterpolationLinear timepoints (VariationTrackVector values)) t =
    interpolateLinear timepoints values t
interpolateVariation (KeyFrames InterpolationCubicSpline timepoints (VariationTrackVector values)) t =
    interpolateCubicSpline timepoints values t

interpolateStep :: GV.Vector v a => UV.Vector Int64 -> v a -> Timespan -> Maybe a
interpolateStep timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if GV.null values then Nothing else Just $ GV.last values
        Just 0  -> if GV.null values then Nothing else Just $ GV.head values
        Just n  -> Just $ values GV.! (n - 1)

interpolateLinear :: (Additive f, Fractional a, GV.Vector v (f a)) => UV.Vector Int64 -> v (f a) -> Timespan -> Maybe (f a)
interpolateLinear timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if GV.null values then Nothing else Just $ GV.last values
        Just 0  -> if GV.null values then Nothing else Just $ GV.head values
        Just n  ->
            let t0 = timepoints GV.! (n - 1)
                t1 = timepoints GV.! n
                v0 = values GV.! (n - 1)
                v1 = values GV.! n
                v = lerp (fromIntegral (t - t0) / fromIntegral (t1 - t0)) v1 v0
            in Just v

interpolateCubicSpline :: (Additive f, Fractional a, GV.Vector v (f a)) => UV.Vector Int64 -> v (f a) -> Timespan -> Maybe (f a)
interpolateCubicSpline timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if GV.null values then Nothing else Just $ GV.last values
        Just 0  -> if GV.null values then Nothing else Just $ GV.head values
        Just n  -> do
            t0 <- timepoints GV.!? (n - 1)
            t1 <- timepoints GV.!? n
            let dt = t1 - t0
            v0 <- values GV.!? ((n - 1) * 3 + 1)
            v1 <- values GV.!? (n * 3 + 1)
            m0 <- values GV.!? ((n - 1) * 3)
            m1 <- values GV.!? (n * 3 + 2)
            let m0' = m0 ^/ fromIntegral dt
                m1' = m1 ^/ fromIntegral dt
            let s = fromIntegral (t - t0) / fromIntegral (t1 - t0)
                ss = s * s
                sss = ss * s
                v = (2.0 * sss - 3.0 * ss + 1.0) *^ v0
                    ^+^ (sss - 2.0 * ss + s) *^ m0'
                    ^+^ ((-2.0) * sss  + 3.0 * ss) *^ v1
                    ^+^ (sss - ss) *^ m1'
            return v

interpolateQuaternion :: UV.Vector Int64 -> UV.Vector Quaternion -> Timespan -> Maybe Quaternion
interpolateQuaternion timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then Nothing else Just $ UV.last values
        Just 0  -> if UV.null values then Nothing else Just $ UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = slerp v0 v1 (fromIntegral (t - t0) / fromIntegral (t1 - t0))
            in Just v

applyTransformChannel :: Scene -> TransformChannel -> Timespan -> IO ()
applyTransformChannel scene (TransformChannel nodeId keys) t = do
    applyTransformToNode scene nodeId f
    where
    f = BV.foldl (\a k -> interpolateTransform k t . a) id keys

applyVariationChannel :: VariationChannel -> Timespan -> IO ()
applyVariationChannel (VariationChannel setter k) t =
    maybe (return ()) setter (interpolateVariation k t)

applyAnimationChannel :: Scene -> AnimationChannel -> Timespan -> IO ()
applyAnimationChannel scene (AnimationChannelTransform channel) t = applyTransformChannel scene channel t
applyAnimationChannel _ (AnimationChannelVariation channel) t = applyVariationChannel channel t

applyAnimationClip :: Scene -> AnimationClip -> Timespan -> IO ()
applyAnimationClip scene (AnimationClip channels d) t =
    BV.mapM_ (flip (applyAnimationChannel scene) t') channels
    where
    t' = max (Timespan 0) . min d $ t
