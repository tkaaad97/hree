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
    , interpolateTransform
    , interpolateTransformLinear
    , interpolateTransformStep
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
    , stepMesh
    ) where

import Chronos (Timespan(..))
import Control.Monad (void)
import Data.Int (Int64)
import qualified Data.Vector as BV (Vector, foldl, map, mapM_, maximum, null,
                                    singleton, snoc)
import qualified Data.Vector.Storable as SV (Vector, head, last, length, null,
                                             (!))
import qualified Data.Vector.Unboxed as UV (Unbox, Vector, head, last, length,
                                            null, (!))
import Graphics.Hree.Math (Quaternion, Transform(..), Vec3)
import Graphics.Hree.Scene (applyTransformToNode, updateNode)
import Graphics.Hree.Types (MeshId, Node(nodeMesh), NodeId, Scene)
import Linear (Additive(..), slerp)

data Interpolation =
    InterpolationLinear |
    InterpolationStep
    deriving (Show, Eq)

data Track =
    TrackNodeTranslation !(UV.Vector Vec3) |
    TrackNodeRotation !(UV.Vector Quaternion) |
    TrackNodeScale !(UV.Vector Vec3) |
    TrackNodeMesh !(SV.Vector MeshId)
    deriving (Show, Eq)

data KeyFrames = KeyFrames
    { keyFramesInterpolation :: !Interpolation
    , keyFramesTimepoints    :: !(UV.Vector Int64)
    , keyFramesTrack         :: !Track
    } deriving (Show, Eq)

data Channel = Channel
    { channelNode      :: !NodeId
    , channelKeyFrames :: !(BV.Vector KeyFrames)
    } deriving (Show, Eq)

data Animation = Animation
    { animationChannels :: !(BV.Vector Channel)
    , animationDuration :: !Timespan
    } deriving (Show, Eq)

stepTranslation :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames
stepTranslation times values = KeyFrames InterpolationStep times (TrackNodeTranslation values)

stepRotation :: UV.Vector Int64 -> UV.Vector Quaternion -> KeyFrames
stepRotation times values = KeyFrames InterpolationStep times (TrackNodeRotation values)

stepScale :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames
stepScale times values = KeyFrames InterpolationStep times (TrackNodeScale values)

stepMesh :: UV.Vector Int64 -> SV.Vector MeshId -> KeyFrames
stepMesh times values = KeyFrames InterpolationStep times (TrackNodeMesh values)

linearTranslation :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames
linearTranslation times values = KeyFrames InterpolationLinear times (TrackNodeTranslation values)

linearRotation :: UV.Vector Int64 -> UV.Vector Quaternion -> KeyFrames
linearRotation times values = KeyFrames InterpolationLinear times (TrackNodeRotation values)

linearScale :: UV.Vector Int64 -> UV.Vector Vec3 -> KeyFrames
linearScale times values = KeyFrames InterpolationLinear times (TrackNodeScale values)

singleChannel :: NodeId -> KeyFrames -> Channel
singleChannel nodeId keyFrames = Channel nodeId (BV.singleton keyFrames)

singleAnimation :: NodeId -> KeyFrames -> Animation
singleAnimation nodeId keyFrames =
    let ch = singleChannel nodeId keyFrames
        duration = channelDuration ch
    in Animation (BV.singleton ch) duration

channelDuration :: Channel -> Timespan
channelDuration ch =
    let keys = channelKeyFrames ch
        timepoints = BV.map keyFramesTimepoints keys
        durations = BV.map (\v -> if UV.null v then 0 else UV.last v) timepoints
    in if BV.null durations
            then Timespan 0
            else Timespan $ BV.maximum durations

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

animation :: BV.Vector Channel -> Timespan -> Animation
animation = Animation

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

interpolateTransform :: KeyFrames -> Timespan -> Transform -> Transform
interpolateTransform (KeyFrames InterpolationStep timepoints (TrackNodeTranslation values)) t trans =
    trans { transformTranslation = interpolateTransformStep timepoints values t }
interpolateTransform (KeyFrames InterpolationStep timepoints (TrackNodeRotation values)) t trans =
    trans { transformQuaternion = interpolateTransformStep timepoints values t }
interpolateTransform (KeyFrames InterpolationStep timepoints (TrackNodeScale values)) t trans =
    trans { transformScale = interpolateTransformStep timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TrackNodeTranslation values)) t trans =
    trans { transformTranslation = interpolateTransformLinear timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TrackNodeRotation values)) t trans =
    trans { transformQuaternion = interpolateQuaternion timepoints values t }
interpolateTransform (KeyFrames InterpolationLinear timepoints (TrackNodeScale values)) t trans =
    trans { transformScale = interpolateTransformLinear timepoints values t }
interpolateTransform (KeyFrames _ _ (TrackNodeMesh _)) _ trans = trans

interpolateTransformStep :: (Additive f, UV.Unbox (f a), Num a) => UV.Vector Int64 -> UV.Vector (f a) -> Timespan -> f a
interpolateTransformStep timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  -> values UV.! (n - 1)

interpolateTransformLinear :: (Additive f, Fractional a, UV.Unbox (f a)) => UV.Vector Int64 -> UV.Vector (f a) -> Timespan -> f a
interpolateTransformLinear timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = lerp (fromIntegral (t - t0) / fromIntegral (t1 - t0)) v1 v0
            in v

interpolateQuaternion :: UV.Vector Int64 -> UV.Vector Quaternion -> Timespan -> Quaternion
interpolateQuaternion timepoints values (Timespan t) =
    case lowerBound timepoints t of
        Nothing -> if UV.null values then zero else UV.last values
        Just 0  -> if UV.null values then zero else UV.head values
        Just n  ->
            let t0 = timepoints UV.! (n - 1)
                t1 = timepoints UV.! n
                v0 = values UV.! (n - 1)
                v1 = values UV.! n
                v = slerp v0 v1 (fromIntegral (t - t0) / fromIntegral (t1 - t0))
            in v

interpolateNode :: KeyFrames -> Timespan -> Node -> Node
interpolateNode (KeyFrames _ _ (TrackNodeTranslation _)) _ node = node
interpolateNode (KeyFrames _ _ (TrackNodeRotation _)) _ node = node
interpolateNode (KeyFrames _ _ (TrackNodeScale _)) _ node = node
interpolateNode (KeyFrames _ timepoints (TrackNodeMesh values)) (Timespan t) node =
    case lowerBound timepoints t of
        Nothing -> if SV.null values then node else node { nodeMesh = Just (SV.last values) }
        Just 0  -> if SV.null values then node else node { nodeMesh = Just (SV.head values) }
        Just n  -> if SV.length values < n then node else node { nodeMesh = Just (values SV.! (n - 1)) }

applyChannel :: Scene -> Channel -> Timespan -> IO ()
applyChannel scene (Channel nodeId keys) t = do
    applyTransformToNode scene nodeId f
    void $ updateNode scene nodeId g
    where
    f = BV.foldl (\a k -> interpolateTransform k t . a) id keys
    g = BV.foldl (\a k -> interpolateNode k t . a) id keys

applyAnimation :: Scene -> Animation -> Timespan -> IO ()
applyAnimation scene (Animation channels _) t =
    BV.mapM_ (flip (applyChannel scene) t) channels
