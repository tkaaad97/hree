{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Graphics.Hree.Material
    ( Material(..)
    , TextureMappingType(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as BV (Vector)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (Texture)
import Graphics.Hree.Program (Options, ProgramSpec)

data TextureMappingType =
    BaseColorMapping |
    NormalMapping |
    MetallicRoughnessMapping
    deriving (Show, Eq, Enum)

class Block (MaterialUniformBlock a) => Material a where
    type MaterialUniformBlock a
    materialUniformBlock      :: a -> MaterialUniformBlock a
    materialTextures          :: a -> BV.Vector (ByteString, Texture)
    materialHasTextureMapping :: a -> TextureMappingType -> Bool
    materialProgramSpec       :: a -> Options -> ProgramSpec
