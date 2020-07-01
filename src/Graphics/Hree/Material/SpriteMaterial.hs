{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material.SpriteMaterial
    ( MaxSpriteTileCount
    , SpriteMaterial
    , SpriteMaterialBlock(..)
    , SpriteTile(..)
    , maxSpriteTileCount
    , spriteMaterial
    ) where

import Data.Function ((&))
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as SV (singleton)
import GHC.TypeNats (Nat, natVal)
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Block (Block(..), Elem(..), Element(..))
import Graphics.Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                               DepthOption(..), LimitedVector(..),
                               setPartialRenderOptionBlending,
                               setPartialRenderOptionCullFace,
                               setPartialRenderOptionDepth)
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..),
                              setPartialMaxSpriteTileCount)
import Graphics.Hree.Types (Material(..))
import Linear (V2(..), V3(..))

type MaxSpriteTileCount = (64 :: Nat)

maxSpriteTileCount :: Int
maxSpriteTileCount = fromIntegral . natVal $ (Proxy :: Proxy MaxSpriteTileCount)

type SpriteMaterial = Material SpriteMaterialBlock

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

spriteMaterial :: Material SpriteMaterialBlock
spriteMaterial = Material
    { materialUniformBlock = block
    , materialTextures = mempty
    , materialRenderOption = renderOption
    , materialProgramOption = programOption
    , materialProgramSpec = EmbeddedProgram SpriteProgram
    }
    where
    tiles = LimitedVector . SV.singleton . Elem $ SpriteTile GL.GL_FALSE GL.GL_FALSE (V2 0 0) (V2 0 0)
    block = SpriteMaterialBlock (V3 0 0 1) 1 (V2 0 0) GL.GL_FALSE GL.GL_FALSE tiles
    renderOption = mempty
        & flip setPartialRenderOptionCullFace (Just Nothing)
        & flip setPartialRenderOptionDepth (Just DepthOption
            { depthOptionDepthTest = False
            , depthOptionDepthMask = True
            , depthOptionDepthFunction = DepthFunction.glLequal
            })
        & flip setPartialRenderOptionBlending (Just BlendingOption
            { blendingOptionEnabled = True
            , blendingOptionRGB = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA)
            , blendingOptionAlpha = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA)
            })
    programOption = mempty & flip setPartialMaxSpriteTileCount (Just maxSpriteTileCount)
