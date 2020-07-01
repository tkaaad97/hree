{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.StandardMaterial
    ( StandardMaterial
    , StandardMaterialBlock(..)
    , standardMaterial
    , standardMaterialBlock
    ) where

import qualified Graphics.GL as GL (GLfloat)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Graphics.Hree.Types (Material(..))
import Linear (V3(..), V4(..))

type StandardMaterial = Material StandardMaterialBlock

data StandardMaterialBlock = StandardMaterialBlock
    { baseColorFactor    :: !(V4 GL.GLfloat)
    , emissivenessFactor :: !(V3 GL.GLfloat)
    , metallicFactor     :: !GL.GLfloat
    , roughnessFactor    :: !GL.GLfloat
    , normalScale        :: !GL.GLfloat
    } deriving (Show, Eq)

instance Block StandardMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 48

    peekByteOffStd140 ptr off = do
        baseColor <- peekByteOffStd140 ptr off
        emissive <- peekByteOffStd140 ptr (off + 16)
        metallic <- peekByteOffStd140 ptr (off + 28)
        roughness <- peekByteOffStd140 ptr (off + 32)
        nscale <- peekByteOffStd140 ptr (off + 36)
        return $ StandardMaterialBlock baseColor emissive metallic roughness nscale

    pokeByteOffStd140 ptr off (StandardMaterialBlock baseColor emissive metallic roughness nscale) = do
        pokeByteOffStd140 ptr off baseColor
        pokeByteOffStd140 ptr (off + 16) emissive
        pokeByteOffStd140 ptr (off + 28) metallic
        pokeByteOffStd140 ptr (off + 32) roughness
        pokeByteOffStd140 ptr (off + 36) nscale

standardMaterial :: StandardMaterialBlock -> Material StandardMaterialBlock
standardMaterial block = Material
    { materialUniformBlock = block
    , materialTextures = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = EmbeddedProgram StandardProgram
    }

standardMaterialBlock :: StandardMaterialBlock
standardMaterialBlock = StandardMaterialBlock
    { baseColorFactor = V4 1 1 1 1
    , emissivenessFactor = V3 0 0 0
    , metallicFactor = 1
    , roughnessFactor = 1
    , normalScale = 1
    }
