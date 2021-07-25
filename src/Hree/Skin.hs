{-# LANGUAGE ScopedTypeVariables #-}
module Hree.Skin
    ( Skin
    , maxJointCount
    ) where

import Data.Proxy (Proxy(..))
import GHC.TypeNats (KnownNat, natVal)
import Hree.GL.Types (LimitedVector(..))
import Hree.Types (MatricesBlockBinder(..), Skin(..))

limitSize :: forall a n p. (KnownNat n) => p (LimitedVector n a) -> Int
limitSize _ = fromIntegral . natVal $ (Proxy :: Proxy n)

maxJointCount :: Skin -> Int
maxJointCount = go . skinJointMatricesBinder
    where
    go (MatricesBlockBinder a) = limitSize a
