module Graphics.Hree.Math
    ( Vec2
    , Vec3
    , Vec4
    , ColorW8
    ) where

import Data.Text (Text)
import Data.Word (Word8)
import qualified Linear as Linear (V2, V3, V4(..))

type Vec2 = Linear.V2 Float

type Vec3 = Linear.V3 Float

type Vec4 = Linear.V4 Float

type ColorW8 = Linear.V4 Word8
