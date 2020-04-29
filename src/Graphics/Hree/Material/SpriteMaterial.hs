{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.SpriteMaterial
    ( SpriteMaterial(..)
    , SpriteMaterialBlock(..)
    , spriteMaterial
    ) where

import Data.Maybe (maybe)
import qualified Data.Vector as BV (singleton)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (Texture)
import Graphics.Hree.Material (Material(..), TextureMappingType(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Linear (V2(..), V3(..))

data SpriteMaterialBlock = SpriteMaterialBlock
    { rotateAxis :: !(V3 Float)
    , uvOffset   :: !(V2 Float)
    , uvScale    :: !(V2 Float)
    } deriving (Show, Eq)

data SpriteMaterial = SpriteMaterial
    { uniformBlock     :: !SpriteMaterialBlock
    , baseColorTexture :: !(Maybe Texture)
    } deriving (Show, Eq)

instance Block SpriteMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 32

    peekByteOffStd140 ptr off = do
        axis <- peekByteOffStd140 ptr off
        uvo <- peekByteOffStd140 ptr (off + 16)
        uvs <- peekByteOffStd140 ptr (off + 24)
        return $ SpriteMaterialBlock axis uvo uvs

    pokeByteOffStd140 ptr off (SpriteMaterialBlock axis uvo uvs) = do
        pokeByteOffStd140 ptr off axis
        pokeByteOffStd140 ptr (off + 16) uvo
        pokeByteOffStd140 ptr (off + 24) uvs

instance Material SpriteMaterial where
    type MaterialUniformBlock SpriteMaterial = SpriteMaterialBlock
    materialUniformBlock = uniformBlock
    materialTextures = maybe mempty (BV.singleton . (,) "baseColorMaterial") . baseColorTexture
    materialHasTextureMapping a mappingType = hasColorMapping mappingType (baseColorTexture a)
        where
        hasColorMapping BaseColorMapping (Just _) = True
        hasColorMapping _ _                       = False
    materialProgramSpec _ = EmbeddedProgram SpriteProgram

spriteMaterial :: SpriteMaterial
spriteMaterial = SpriteMaterial (SpriteMaterialBlock (V3 0 0 1) (V2 0 0) (V2 1 1)) Nothing
