{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hree.Material.StandardMaterial
    ( StandardMaterial
    , StandardMaterialBlock(..)
    , standardMaterial
    , standardMaterialBlock
    ) where

import qualified Graphics.GL as GL (GLfloat)
import Hree.GL.Block (Block(..))
import Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Hree.Types (Material(..))
import Linear (V3(..), V4(..))

type StandardMaterial = Material StandardMaterialBlock

data StandardMaterialBlock = StandardMaterialBlock
    { baseColorFactor   :: !(V4 GL.GLfloat)
    , emissiveFactor    :: !(V3 GL.GLfloat)
    , metallicFactor    :: !GL.GLfloat
    , roughnessFactor   :: !GL.GLfloat
    , normalScale       :: !GL.GLfloat
    , occlusionStrength :: !GL.GLfloat
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
        occlusion <- peekByteOffStd140 ptr (off + 40)
        return $ StandardMaterialBlock baseColor emissive metallic roughness nscale occlusion

    pokeByteOffStd140 ptr off (StandardMaterialBlock baseColor emissive metallic roughness nscale occlusion) = do
        pokeByteOffStd140 ptr off baseColor
        pokeByteOffStd140 ptr (off + 16) emissive
        pokeByteOffStd140 ptr (off + 28) metallic
        pokeByteOffStd140 ptr (off + 32) roughness
        pokeByteOffStd140 ptr (off + 36) nscale
        pokeByteOffStd140 ptr (off + 40) occlusion

standardMaterial :: StandardMaterialBlock -> Material StandardMaterialBlock
standardMaterial block = Material
    { materialUniformBlock = block
    , materialMappings = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = EmbeddedProgram StandardProgram
    }

standardMaterialBlock :: StandardMaterialBlock
standardMaterialBlock = StandardMaterialBlock
    { baseColorFactor = V4 1 1 1 1
    , emissiveFactor = V3 0 0 0
    , metallicFactor = 1
    , roughnessFactor = 1
    , normalScale = 1
    , occlusionStrength = 1
    }
