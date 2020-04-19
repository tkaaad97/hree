{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.StandardMaterial
    ( StandardMaterial(..)
    , StandardMaterialBlock(..)
    , standardMaterial
    , standardMaterialBlock
    ) where

import Data.Maybe (catMaybes)
import qualified Data.Vector as BV (fromList)
import qualified Graphics.GL as GL (GLfloat)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (Texture)
import Graphics.Hree.Material (Material(..), TextureMappingType(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Linear (V3(..), V4(..))

data StandardMaterialBlock = StandardMaterialBlock
    { baseColorFactor    :: !(V4 GL.GLfloat)
    , emissivenessFactor :: !(V3 GL.GLfloat)
    , metallicFactor     :: !GL.GLfloat
    , roughnessFactor    :: !GL.GLfloat
    , normalScale        :: !GL.GLfloat
    } deriving (Show, Eq)

data StandardMaterial = StandardMaterial
    { uniformBlock             :: !StandardMaterialBlock
    , baseColorTexture         :: !(Maybe Texture)
    , normalTexture            :: !(Maybe Texture)
    , metallicRoughnessTexture :: !(Maybe Texture)
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

instance Material StandardMaterial where
    type MaterialUniformBlock StandardMaterial = StandardMaterialBlock
    materialUniformBlock = uniformBlock
    materialTextures a = BV.fromList . catMaybes $
        [ texture (baseColorTexture a) "baseColorTexture"
        , texture (normalTexture a) "normalTexture"
        , texture (metallicRoughnessTexture a) "metallicRoughnessTexture"
        ]
        where
        texture Nothing _  = Nothing
        texture (Just t) n = Just (n, t)
    materialHasTextureMapping a mappingType = hasColorMapping mappingType (baseColorTexture a) (normalTexture a) (metallicRoughnessTexture a)
        where
        hasColorMapping BaseColorMapping (Just _) _ _         = True
        hasColorMapping NormalMapping _ (Just _) _            = True
        hasColorMapping MetallicRoughnessMapping _ _ (Just _) = True
        hasColorMapping _ _ _ _                               = False
    materialProgramSpec _ = EmbeddedProgram StandardProgram

standardMaterial :: StandardMaterialBlock -> StandardMaterial
standardMaterial block = StandardMaterial
    { uniformBlock = block
    , baseColorTexture = Nothing
    , normalTexture = Nothing
    , metallicRoughnessTexture = Nothing
    }

standardMaterialBlock :: StandardMaterialBlock
standardMaterialBlock = StandardMaterialBlock
    { baseColorFactor = V4 1 1 1 1
    , emissivenessFactor = V3 0 0 0
    , metallicFactor = 1
    , roughnessFactor = 1
    , normalScale = 1
    }
