module MathSpec
    ( spec
    ) where

import qualified Graphics.Hree as Hree
import Linear (Quaternion(..), V3(..), fromQuaternion, nearZero, normalize,
               (^-^))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "matrixToTransform" $ do
        it "should decompose matrix to transform" . property $
            \((tx, ty, tz), (q0, q1, q2, q3), (sx, sy, sz)) -> do
                let translate = V3 tx ty tz
                    quaternion = normalize (Quaternion q0 (V3 (correctNotZero q1) q2 q3))
                    scale = V3 (abs $ correctNotZero sx) (abs $ correctNotZero sy) (abs $ correctNotZero sz)
                    trans = Hree.Transform translate quaternion scale
                    mat = Hree.transformMatrix trans
                    result = Hree.matrixToTransform mat
                    Hree.Transform translate' quaternion' scale' = result

                ((trans, result), translate ^-^ translate') `shouldSatisfy` (nearZero . snd)
                ((trans, result), Linear.fromQuaternion quaternion ^-^ Linear.fromQuaternion quaternion') `shouldSatisfy` (nearZero . snd)
                ((trans, result), scale ^-^ scale') `shouldSatisfy` (nearZero . snd)
    where
    correctNotZero x
        | x == 0 = 1.0E-4
        | x > 0 = max 1.0E-4 x
        | otherwise = - max 1.0E-4 (-x)

