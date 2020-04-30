{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Skin
    ( Skin
    , maxJointCount
    ) where

import Data.Proxy (Proxy(..))
import GHC.TypeNats (KnownNat, natVal)
import Graphics.Hree.GL.Types (LimitedVector(..))
import Graphics.Hree.Types (MatricesBlockBinder(..), Skin(..))

limitSize :: forall a n p. (KnownNat n) => p (LimitedVector n a) -> Int
limitSize _ = fromIntegral . natVal $ (Proxy :: Proxy n)

maxJointCount :: Skin -> Int
maxJointCount = go . skinJointMatricesBinder
    where
    go (MatricesBlockBinder a) = limitSize a
