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
import Linear ((^*))
import qualified Linear (M44, Quaternion(..), V2, V3(..), V4, fromQuaternion,
                         mkTransformationMat)

type Vec2 = Linear.V2 Float

type Vec3 = Linear.V3 Float

type Vec4 = Linear.V4 Float

type ColorW8 = Linear.V4 Word8

type Quaternion = Linear.Quaternion Float

type Mat4 = Linear.M44 Float

data Transform = Transform
    { transformTranslation :: !Vec3
    , transformQuaternion  :: !Quaternion
    , transformScale       :: !Vec3
    , transformUpdated     :: !Bool
    } deriving (Show, Eq)

zeroTransform :: Transform
zeroTransform =
    let t = Linear.V3 0 0 0
        q = Linear.Quaternion 1 (Linear.V3 0 0 0)
        s = Linear.V3 1 1 1
        u = False
    in Transform t q s u

instance Storable Transform where
    sizeOf _ = 44

    alignment _ = 4

    peek ptr = do
        t <- peek $ castPtr ptr
        q <- peek $ castPtr ptr `plusPtr` 12
        s <- peek $ castPtr ptr `plusPtr` 28
        u <- peek $ castPtr ptr `plusPtr` 40
        return $ Transform t q s u

    poke ptr (Transform t q s u) = do
        poke (castPtr ptr) t
        poke (castPtr ptr `plusPtr` 12) q
        poke (castPtr ptr `plusPtr` 28) s
        poke (castPtr ptr `plusPtr` 40) u

transformMatrix :: Transform -> Mat4
transformMatrix (Transform t q (Linear.V3 sx sy sz) _) =
    let Linear.V3 v0 v1 v2 = Linear.fromQuaternion q
        m = Linear.V3 (v0 ^* sx) (v1 ^* sy) (v2 ^* sz)
    in Linear.mkTransformationMat m t
