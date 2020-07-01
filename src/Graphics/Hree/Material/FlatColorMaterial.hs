{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.FlatColorMaterial
    ( FlatColorMaterial
    , FlatColorMaterialBlock(..)
    , flatColorMaterial
    ) where

import qualified Graphics.GL as GL (GLfloat)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Graphics.Hree.Types (Material(..))
import Linear (V4)

type FlatColorMaterial = Material FlatColorMaterialBlock

newtype FlatColorMaterialBlock = FlatColorMaterialBlock
    { color :: V4 GL.GLfloat
    } deriving (Show, Eq)

instance Block FlatColorMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16

    peekByteOffStd140 ptr off = do
        c <- peekByteOffStd140 ptr off
        return $ FlatColorMaterialBlock c

    pokeByteOffStd140 ptr off (FlatColorMaterialBlock c) =
        pokeByteOffStd140 ptr off c

flatColorMaterial :: V4 GL.GLfloat -> Material FlatColorMaterialBlock
flatColorMaterial c = Material
    { materialUniformBlock = FlatColorMaterialBlock c
    , materialTextures = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = EmbeddedProgram FlatColorProgram
    }
