{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.SpriteMaterial
    ( MaxSpriteTileCount
    , SpriteMaterial(..)
    , SpriteMaterialBlock(..)
    , SpriteTile(..)
    , maxSpriteTileCount
    , spriteMaterial
    ) where

import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV (singleton)
import qualified Data.Vector.Storable as SV (singleton)
import GHC.TypeNats (Nat, natVal)
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Block (Block(..), Elem(..), Element(..))
import Graphics.Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                               DepthOption(..), LimitedVector(..),
                               RenderOption(..), Texture)
import Graphics.Hree.Material (Material(..), TextureMappingType(..),
                               defaultRenderOption)
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..),
                              programOptionMaxSpriteTileCount)
import Linear (V2(..), V3(..))

type MaxSpriteTileCount = (64 :: Nat)

maxSpriteTileCount :: Int
maxSpriteTileCount = fromIntegral . natVal $ (Proxy :: Proxy MaxSpriteTileCount)

data SpriteTile = SpriteTile
    { spriteTileFlippedHorizontally :: !GL.GLboolean
    , spriteTIleFlippedVertically   :: !GL.GLboolean
    , spriteTileUv                  :: !(V2 Float)
    , spriteTileUvSize              :: !(V2 Float)
    } deriving (Show, Eq)

data SpriteMaterialBlock = SpriteMaterialBlock
    { rotateAxis            :: !(V3 Float)
    , opacityFactor         :: !Float
    , uvOffset              :: !(V2 Float)
    , uvFlippedHorizontally :: !GL.GLboolean
    , uvFlippedVertically   :: !GL.GLboolean
    , spriteTiles           :: !(LimitedVector MaxSpriteTileCount (Elem SpriteTile))
    } deriving (Show, Eq)

data SpriteMaterial = SpriteMaterial
    { uniformBlock     :: !SpriteMaterialBlock
    , baseColorTexture :: !(Maybe Texture)
    } deriving (Show, Eq)

instance Block SpriteTile where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 24

    peekByteOffStd140 ptr off = do
        flipH <- peekByteOffStd140 ptr off
        flipV <- peekByteOffStd140 ptr (off + 4)
        uv <- peekByteOffStd140 ptr (off + 8)
        uvSize <- peekByteOffStd140 ptr (off + 16)
        return (SpriteTile flipH flipV uv uvSize)

    pokeByteOffStd140 ptr off (SpriteTile flipH flipV uv uvSize) = do
        pokeByteOffStd140 ptr off flipH
        pokeByteOffStd140 ptr (off + 4) flipV
        pokeByteOffStd140 ptr (off + 8) uv
        pokeByteOffStd140 ptr (off + 16) uvSize

instance Element SpriteTile where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 32

instance Block SpriteMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = spriteMaterialBlockByteSize

    peekByteOffStd140 ptr off = do
        axis <- peekByteOffStd140 ptr off
        opacity <- peekByteOffStd140 ptr (off + 12)
        uvo <- peekByteOffStd140 ptr (off + 16)
        flipH <- peekByteOffStd140 ptr (off + 24)
        flipV <- peekByteOffStd140 ptr (off + 28)
        tiles <- peekByteOffStd140 ptr (off + 32)
        return $ SpriteMaterialBlock axis opacity uvo flipH flipV tiles

    pokeByteOffStd140 ptr off (SpriteMaterialBlock axis opacity uvo flipH flipV tiles) = do
        pokeByteOffStd140 ptr off axis
        pokeByteOffStd140 ptr (off + 12) opacity
        pokeByteOffStd140 ptr (off + 16) uvo
        pokeByteOffStd140 ptr (off + 24) flipH
        pokeByteOffStd140 ptr (off + 28) flipV
        pokeByteOffStd140 ptr (off + 32) tiles

spriteMaterialBlockByteSize :: Int
spriteMaterialBlockByteSize = 32 + sizeOfStd140 (Proxy :: Proxy (LimitedVector MaxSpriteTileCount (Elem SpriteTile)))

instance Material SpriteMaterial where
    type MaterialUniformBlock SpriteMaterial = SpriteMaterialBlock
    materialUniformBlock = uniformBlock
    materialTextures = maybe mempty (BV.singleton . (,) "baseColorMaterial") . baseColorTexture
    materialHasTextureMapping a mappingType = hasColorMapping mappingType (baseColorTexture a)
        where
        hasColorMapping BaseColorMapping (Just _) = True
        hasColorMapping _ _                       = False
    materialProgramSpec _ programOption =
        EmbeddedProgram SpriteProgram programOption { programOptionMaxSpriteTileCount = pure maxSpriteTileCount }
    materialRenderOption _ = defaultRenderOption
        { renderOptionCullFace = Nothing
        , renderOptionDepth = DepthOption
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
spriteMaterial = SpriteMaterial (SpriteMaterialBlock (V3 0 0 1) 1 (V2 0 0) GL.GL_FALSE GL.GL_FALSE tiles) Nothing
    where
    tiles = LimitedVector . SV.singleton . Elem $ SpriteTile GL.GL_FALSE GL.GL_FALSE (V2 0 0) (V2 0 0)
