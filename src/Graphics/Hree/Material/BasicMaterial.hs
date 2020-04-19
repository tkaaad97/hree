{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.BasicMaterial
    ( BasicMaterial(..)
    , BasicMaterialBlock(..)
    , basicMaterial
    ) where

import Data.Maybe (maybe)
import qualified Data.Vector as BV (singleton)
import qualified Graphics.GL as GL (GLfloat)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (Texture)
import Graphics.Hree.Material (Material(..), TextureMappingType(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Linear (V3(..))

data BasicMaterialBlock = BasicMaterialBlock
    { directionalLight :: !(V3 GL.GLfloat)
    } deriving (Show, Eq)

data BasicMaterial = BasicMaterial
    { uniformBlock     :: !BasicMaterialBlock
    , baseColorTexture :: !(Maybe Texture)
    } deriving (Show, Eq)

instance Block BasicMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12

    peekByteOffStd140 ptr off = do
        l <- peekByteOffStd140 ptr off
        return $ BasicMaterialBlock l

    pokeByteOffStd140 ptr off (BasicMaterialBlock l) = do
        pokeByteOffStd140 ptr off l

instance Material BasicMaterial where
    type MaterialUniformBlock BasicMaterial = BasicMaterialBlock
    materialUniformBlock = uniformBlock
    materialTextures = maybe mempty (BV.singleton . (,) "baseColorTexture") . baseColorTexture
    materialHasTextureMapping a mappingType = hasColorMapping mappingType (baseColorTexture a)
        where
        hasColorMapping BaseColorMapping (Just _) = True
        hasColorMapping _ _                       = False
    materialProgramSpec _ = EmbeddedProgram BasicProgram

basicMaterial :: V3 GL.GLfloat -> BasicMaterial
basicMaterial l = BasicMaterial (BasicMaterialBlock l) Nothing
