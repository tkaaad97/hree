{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Graphics.Hree.Material
    ( Material(..)
    , TextureMappingType(..)
    , defaultRenderOption
    ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as BV (Vector)
import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                               DepthOption(..), FaceStencilOption(..),
                               RenderOption, RenderOption_(..),
                               StencilFuncArgs(..), StencilOpArgs(..),
                               StencilOption(..), Texture)
import Graphics.Hree.Program (ProgramOption, ProgramSpec)
import Linear (V4(..))

data TextureMappingType =
    BaseColorMapping |
    NormalMapping |
    MetallicRoughnessMapping
    deriving (Show, Eq, Enum)

class Block (MaterialUniformBlock a) => Material a where
    type MaterialUniformBlock a
    materialUniformBlock      :: a -> MaterialUniformBlock a
    materialTextures          :: a -> BV.Vector (ByteString, Texture)
    materialHasTextureMapping :: a -> TextureMappingType -> Bool
    materialRenderOption      :: a -> RenderOption
    materialProgramSpec       :: a -> ProgramOption -> ProgramSpec

    materialRenderOption _ = defaultRenderOption

defaultRenderOption :: RenderOption
defaultRenderOption = RenderOption
    { renderOptionCullFace = pure $ Just CullFaceMode.glBack
    , renderOptionFlipSided = pure $ False
    , renderOptionDepth = pure $ defaultDepthOption
    , renderOptionBlending = pure $ defaultBlendingOption
    , renderOptionStencil = pure $ defaultStencilOption
    , renderOptionColorMask = pure $ V4 GL.GL_TRUE GL.GL_TRUE GL.GL_TRUE GL.GL_TRUE
    }

defaultDepthOption :: DepthOption
defaultDepthOption = DepthOption
    { depthOptionDepthTest = True
    , depthOptionDepthMask = True
    , depthOptionDepthFunction = DepthFunction.glLequal
    }

defaultBlendingOption :: BlendingOption
defaultBlendingOption = BlendingOption
    { blendingOptionEnabled = True
    , blendingOptionRGB = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ONE_MINUS_SRC_ALPHA)
    , blendingOptionAlpha = BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ONE_MINUS_SRC_ALPHA)
    }

defaultStencilOption :: StencilOption
defaultStencilOption = StencilOption
    { stencilOptionStencilTest = False
    , stencilOptionFront = defaultFaceStencilOption
    , stencilOptionBack = defaultFaceStencilOption
    }

defaultFaceStencilOption :: FaceStencilOption
defaultFaceStencilOption = FaceStencilOption
    { faceStencilOptionStencilFunc = StencilFuncArgs StencilFunction.glAlways 0 0xFF
    , faceStencilOptionStencilOp = StencilOpArgs StencilOp.glKeep StencilOp.glKeep StencilOp.glKeep
    , faceStencilOptionStencilMask = 0xFF
    }
