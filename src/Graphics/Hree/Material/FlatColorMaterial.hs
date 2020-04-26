{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.FlatColorMaterial
    ( FlatColorMaterial(..)
    , flatColorMaterial
    ) where

import qualified Graphics.GL as GL (GLfloat)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.Material (Material(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Linear (V4)

newtype FlatColorMaterial = FlatColorMaterial
    { color :: V4 GL.GLfloat
    } deriving (Show, Eq)

instance Block FlatColorMaterial where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16

    peekByteOffStd140 ptr off = do
        c <- peekByteOffStd140 ptr off
        return $ FlatColorMaterial c

    pokeByteOffStd140 ptr off (FlatColorMaterial c) =
        pokeByteOffStd140 ptr off c

instance Material FlatColorMaterial where
    type MaterialUniformBlock FlatColorMaterial = FlatColorMaterial
    materialUniformBlock = id
    materialTextures = mempty
    materialHasTextureMapping _ _ = False
    materialProgramSpec _ = EmbeddedProgram FlatColorProgram

flatColorMaterial :: V4 GL.GLfloat -> FlatColorMaterial
flatColorMaterial = FlatColorMaterial
