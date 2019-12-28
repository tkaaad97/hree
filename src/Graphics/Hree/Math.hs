module Graphics.Hree.Math
    ( Vec2
    , Vec3
    , Vec4
    , Mat4
    , ColorW8
    , Quaternion
    , Transform(..)
    , transformMatrix
    , zeroTransform
    ) where

import Data.Word (Word8)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Graphics.Hree.GL.Types (Mat4, Vec2, Vec3, Vec4)
import Linear ((!*!))
import qualified Linear (Quaternion(..), V3(..), V4(..), fromQuaternion,
                         mkTransformationMat)

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
