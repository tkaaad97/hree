module AnimationSpec
    ( spec
    ) where

import Chronos (Timespan(..))
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
            let ms = 1000000
                vec = UV.fromList . map (* ms) $ [0, 1000, 1500, 1800, 2200, 2000, 3000]
            lowerBound vec (2000 * ms) `shouldBe` Just 4
            lowerBound vec (- 1000 * ms) `shouldBe` Just 0
            lowerBound vec (100 * ms) `shouldBe` Just 1
            lowerBound vec (3000 * ms) `shouldBe` Just 6

        it "will return Nothing if all elements are less than a specifierd argument" $ do
            let vec = UV.fromList [0, 1, 2]
                result = lowerBound vec 3
            result `shouldBe` Nothing

    describe "interpolate Step" $ do
        it "will return key frame value" $ do
            let timepoints = UV.fromList [0, 10, 20]
                v1 = V3 0 0 0
                v2 = V3 10 10 10
                v3 = V3 20 20 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
            interpolateTransformStep timepoints values (Timespan 15) `shouldBe` v2
            interpolateTransformStep timepoints values (Timespan (-1)) `shouldBe` v1
            interpolateTransformStep timepoints values (Timespan 1) `shouldBe` v1
            interpolateTransformStep timepoints values (Timespan 25) `shouldBe` v3

    describe "interpolate Linear" $ do
        it "will return linear interpolated key frame value" $ do
            let timepoints = UV.fromList [0, 10, 20]
                v1 = V3 0 0 0
                v2 = V3 10 10 10
                v3 = V3 20 20 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
            interpolateTransformLinear timepoints values (Timespan 15) - (V3 15 15 15) `shouldSatisfy` nearZero
            interpolateTransformLinear timepoints values (Timespan (-10)) - v1 `shouldSatisfy` nearZero
            interpolateTransformLinear timepoints values (Timespan 1) - (V3 1 1 1) `shouldSatisfy` nearZero
            interpolateTransformLinear timepoints values (Timespan 25) - v3 `shouldSatisfy` nearZero

    describe "applyAnimation" $ do
        it "transform a node" $ do
            scene <- Hree.newScene
            nodeId <- Hree.addNode scene Hree.newNode True
            let timepoints = UV.fromList [0, 10, 20]
                v1 = V3 0 0 0
                v2 = V3 10 10 0
                v3 = V3 10 10 20
                values = UV.fromList [v1, v2, v3] :: UV.Vector (V3 Float)
                ani = singleAnimation nodeId (linearTranslation timepoints values)
            applyAnimation scene ani (Timespan 0)
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just v1) . fmap transformTranslation
            applyAnimation scene ani (Timespan 5)
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 5 5 0)) . fmap transformTranslation
            applyAnimation scene ani (Timespan 10)
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 10 10 0)) . fmap transformTranslation
            applyAnimation scene ani (Timespan 21)
            Hree.readNodeTransform scene nodeId >>= (`shouldBe` Just (V3 10 10 20)) . fmap transformTranslation
