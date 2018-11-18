module Graphics.Hree.Texture
    ( Texture(..)
    ) where

import Data.ByteString (ByteString)

data Texture =
    Texture !ByteString
    deriving (Show, Eq)
