module Hree.Math
    ( Vec2
    , Vec3
    , Vec4
    , Mat4
    , ColorW8
    , Quaternion
    , Transform(..)
    , matrixToTransform
    , rotateMatrixToQuaternion
    , transformMatrix
    , zeroTransform
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as UV (fromList, maxIndexBy, (!))
import Data.Word (Word8)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Hree.GL.Types (Mat3, Mat4, Vec2, Vec3, Vec4)
import Linear ((!*!))
import qualified Linear (Epsilon(nearZero), Metric(norm), Quaternion(..),
                         V3(..), V4(..), fromQuaternion, mkTransformationMat,
                         normalize, transpose)

type ColorW8 = Linear.V4 Word8

type Quaternion = Linear.Quaternion Float

data Transform = Transform
    { transformTranslation :: !Vec3
    , transformQuaternion  :: !Quaternion
    , transformScale       :: !Vec3
    } deriving (Show, Eq)

zeroTransform :: Transform
zeroTransform =
    let t = Linear.V3 0 0 0
        q = Linear.Quaternion 1 (Linear.V3 0 0 0)
        s = Linear.V3 1 1 1
    in Transform t q s

instance Storable Transform where
    sizeOf _ = 40

    alignment _ = 4

    peek ptr = do
        t <- peek $ castPtr ptr
        q <- peek $ castPtr ptr `plusPtr` 12
        s <- peek $ castPtr ptr `plusPtr` 28
        return $ Transform t q s

    poke ptr (Transform t q s) = do
        poke (castPtr ptr) t
        poke (castPtr ptr `plusPtr` 12) q
        poke (castPtr ptr `plusPtr` 28) s

transformMatrix :: Transform -> Mat4
transformMatrix (Transform t q (Linear.V3 sx sy sz)) =
    let m = Linear.fromQuaternion q
    in Linear.mkTransformationMat m t !*! Linear.V4 (Linear.V4 sx 0 0 0) (Linear.V4 0 sy 0 0) (Linear.V4 0 0 sz 0) (Linear.V4 0 0 0 1)

matrixToTransform :: Mat4 -> Transform
matrixToTransform mat = Transform t q s
    where
    Linear.V4
        (Linear.V4 m00 m01 m02 m03)
        (Linear.V4 m10 m11 m12 m13)
        (Linear.V4 m20 m21 m22 m23)
        _ = mat
    v0 = Linear.V3 m00 m10 m20
    v1 = Linear.V3 m01 m11 m21
    v2 = Linear.V3 m02 m12 m22
    sx = Linear.norm v0
    sy = Linear.norm v1
    sz = Linear.norm v2
    rmat = Linear.transpose $ Linear.V3 (Linear.normalize v0) (Linear.normalize v1) (Linear.normalize v2)
    q = fromMaybe (Linear.Quaternion 1 (Linear.V3 0 0 0)) . rotateMatrixToQuaternion $ rmat
    s = Linear.V3 sx sy sz
    t = Linear.V3 m03 m13 m23

-- http://marupeke296.sakura.ne.jp/DXG_No58_RotQuaternionTrans.html
rotateMatrixToQuaternion :: Mat3 -> Maybe Quaternion
rotateMatrixToQuaternion mat
    | Linear.nearZero maxElem = Nothing
    | otherwise = q maxElemIndex
    where
    Linear.V3
        (Linear.V3 m00 m01 m02)
        (Linear.V3 m10 m11 m12)
        (Linear.V3 m20 m21 m22) = mat
    e0 = m00 - m11 - m22 + 1
    e1 = - m00 + m11 - m22 + 1
    e2 = - m00 - m11 + m22 + 1
    e3 = m00 + m11 + m22 + 1
    elems = UV.fromList [e0, e1, e2, e3]
    maxElemIndex = UV.maxIndexBy (\a b -> abs a `compare` abs b) elems
    maxElem = elems UV.! maxElemIndex
    v = signum maxElem * sqrt (abs maxElem) * 0.5
    s = 0.25 / v
    q 0 =
        let qx = v
            qy = (m10 + m01) * s
            qz = (m02 + m20) * s
            qw = (m21 - m12) * s
        in Just (Linear.Quaternion qw (Linear.V3 qx qy qz))
    q 1 =
        let qx = (m10 + m01) * s
            qy = v
            qz = (m21 + m12) * s
            qw = (m02 - m20) * s
        in Just (Linear.Quaternion qw (Linear.V3 qx qy qz))
    q 2 =
        let qx = (m02 + m20) * s
            qy = (m21 + m12) * s
            qz = v
            qw = (m10 - m01) * s
        in Just (Linear.Quaternion qw (Linear.V3 qx qy qz))
    q 3 =
        let qx = (m21 - m12) * s
            qy = (m02 - m20) * s
            qz = (m10 - m01) * s
            qw = v
        in Just (Linear.Quaternion qw (Linear.V3 qx qy qz))
    q _ = Nothing
