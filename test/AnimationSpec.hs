module AnimationSpec
    ( spec
    ) where

import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import Graphics.Hree.Animation
import Graphics.Hree.Math (Transform(..))
import qualified Graphics.Hree.Scene as Hree
import Linear (Epsilon(..), V3(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "lowerBound" $ do
        it "search index of a element which value is greater than a specified argument" $ do
            let vec = UV.fromList [0.0, 1.0, 1.5, 1.8, 2.2, 2.0, 3.0] :: UV.Vector Float
            lowerBound vec 2.0 `shouldBe` Just 4
            lowerBound vec (-1) `shouldBe` Just 0
            lowerBound vec 0.1 `shouldBe` Just 1
            lowerBound vec 3.0 `shouldBe` Just 6

        it "will return Nothing if all elements are less than a specifierd argument" $ do
            let vec = UV.fromList [0.0, 1.0, 2.0] :: UV.Vector Float
                result = lowerBound vec 3.0
            result `shouldBe` Nothing

    describe "interpolate Step" $ do
        it "will return key frame value" $ do
            let timepoints = UV.fromList [0.0, 1.0, 2.0]
                v1 = V3 0 0 0
                v2 = V3 10 10 10
                v3 = V3 20 20 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
                keyFrames = KeyFrames InterpolationStep timepoints values
            interpolate keyFrames 1.5 `shouldBe` v2
            interpolate keyFrames (-1) `shouldBe` v1
            interpolate keyFrames 0.1 `shouldBe` v1
            interpolate keyFrames 2.5 `shouldBe` v3

    describe "interpolate Linear" $ do
        it "will return linear interpolated key frame value" $ do
            let timepoints = UV.fromList [0.0, 1.0, 2.0]
                v1 = V3 0 0 0
                v2 = V3 10 10 10
                v3 = V3 20 20 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
                keyFrames = KeyFrames InterpolationLinear timepoints values
            interpolate keyFrames 1.5 - (V3 15 15 15) `shouldSatisfy` nearZero
            interpolate keyFrames (-1) - v1 `shouldSatisfy` nearZero
            interpolate keyFrames 0.1 - (V3 1 1 1) `shouldSatisfy` nearZero
            interpolate keyFrames 2.5 - v3 `shouldSatisfy` nearZero

    describe "applyAnimation" $ do
        it "transform a node" $ do
            scene <- Hree.newScene
            nodeId <- Hree.addNode scene Hree.newNode True
            let timepoints = UV.fromList [0.0, 1.0, 2.0]
                v1 = V3 0 0 0
                v2 = V3 10 10 0
                v3 = V3 10 10 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
                keyFrames = KeyFrames InterpolationLinear timepoints values
                track = TrackNodeTranslation keyFrames
                channels = BV.singleton $ Channel nodeId (BV.singleton track)
                animation = Animation channels 2.0
            applyAnimation scene animation 0
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just v1) . fmap transformTranslation
            applyAnimation scene animation 0.5
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 5 5 0)) . fmap transformTranslation
            applyAnimation scene animation 1.0
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 10 10 0)) . fmap transformTranslation
            applyAnimation scene animation 2.1
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 10 10 20)) . fmap transformTranslation
