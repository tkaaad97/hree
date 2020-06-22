{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.SpriteMaterial
    ( SpriteMaterial(..)
    , SpriteMaterialBlock(..)
    , spriteMaterial
    ) where

import qualified Data.Vector as BV (singleton)
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                               DepthOption(..), RenderOption(..), Texture)
import Graphics.Hree.Material (Material(..), TextureMappingType(..),
                               defaultRenderOption)
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Linear (V2(..), V3(..))

data SpriteMaterialBlock = SpriteMaterialBlock
    { rotateAxis            :: !(V3 Float)
    , uvFlippedHorizontally :: !GL.GLboolean
    , uvFlippedVertically   :: !GL.GLboolean
    , uvOffset              :: !(V2 Float)
    , uvScale               :: !(V2 Float)
    } deriving (Show, Eq)

data SpriteMaterial = SpriteMaterial
    { uniformBlock     :: !SpriteMaterialBlock
    , baseColorTexture :: !(Maybe Texture)
    } deriving (Show, Eq)

instance Block SpriteMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 36

    peekByteOffStd140 ptr off = do
        axis <- peekByteOffStd140 ptr off
        flipH <- peekByteOffStd140 ptr (off + 12)
        uvo <- peekByteOffStd140 ptr (off + 16)
        uvs <- peekByteOffStd140 ptr (off + 24)
        flipV <- peekByteOffStd140 ptr (off + 32)
        return $ SpriteMaterialBlock axis flipH flipV uvo uvs

    pokeByteOffStd140 ptr off (SpriteMaterialBlock axis flipH flipV uvo uvs) = do
        pokeByteOffStd140 ptr off axis
        pokeByteOffStd140 ptr (off + 12) flipH
        pokeByteOffStd140 ptr (off + 16) uvo
        pokeByteOffStd140 ptr (off + 24) uvs
        pokeByteOffStd140 ptr (off + 32) flipV

instance Material SpriteMaterial where
    type MaterialUniformBlock SpriteMaterial = SpriteMaterialBlock
    materialUniformBlock = uniformBlock
    materialTextures = maybe mempty (BV.singleton . (,) "baseColorMaterial") . baseColorTexture
    materialHasTextureMapping a mappingType = hasColorMapping mappingType (baseColorTexture a)
        where
        hasColorMapping BaseColorMapping (Just _) = True
        hasColorMapping _ _                       = False
    materialProgramSpec _ = EmbeddedProgram SpriteProgram
    materialRenderOption _ = defaultRenderOption
        { renderOptionDepth = DepthOption
            { depthOptionDepthTest = False
            , depthOptionDepthMask = True
            , depthOptionDepthFunction = DepthFunction.glLequal
            }
        , renderOptionBlending = BlendingOption
            { blendingOptionEnabled = True
            , blendingOptionRGB = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA)
            , blendingOptionAlpha = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA)
            }
        }

spriteMaterial :: SpriteMaterial
spriteMaterial = SpriteMaterial (SpriteMaterialBlock (V3 0 0 1) GL.GL_FALSE GL.GL_FALSE (V2 0 0) (V2 1 1)) Nothing
