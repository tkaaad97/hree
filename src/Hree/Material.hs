{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hree.Material
    ( module Hree.Material.BasicMaterial
    , module Hree.Material.FlatColorMaterial
    , module Hree.Material.SpriteMaterial
    , module Hree.Material.StandardMaterial
    , module Hree.Material.UserMaterial
    , defaultRenderOption
    , textureMappingUniformName
    ) where

import Data.ByteString (ByteString)
import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                      DepthOption(..), FaceStencilOption(..), RenderOption,
                      RenderOption_(..), StencilFuncArgs(..), StencilOpArgs(..),
                      StencilOption(..))
import Hree.Types (TextureMappingType(..))
import Linear (V4(..))

import Hree.Material.BasicMaterial (BasicMaterial, BasicMaterialBlock,
                                    basicMaterial)
import Hree.Material.FlatColorMaterial (FlatColorMaterial,
                                        FlatColorMaterialBlock,
                                        flatColorMaterial)
import Hree.Material.SpriteMaterial (SpriteMaterial, SpriteMaterialBlock,
                                     spriteMaterial)
import Hree.Material.StandardMaterial (StandardMaterial, StandardMaterialBlock,
                                       standardMaterial, standardMaterialBlock)
import Hree.Material.UserMaterial (userMaterial)

defaultRenderOption :: RenderOption
defaultRenderOption = RenderOption
    { renderOptionCullFace = pure $ Just CullFaceMode.glBack
    , renderOptionFlipSided = pure False
    , renderOptionDepth = pure defaultDepthOption
    , renderOptionBlending = pure defaultBlendingOption
    , renderOptionStencil = pure defaultStencilOption
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

textureMappingUniformName :: TextureMappingType -> ByteString
textureMappingUniformName BaseColorMapping         = "baseColorMapping"
textureMappingUniformName NormalMapping            = "normalMapping"
textureMappingUniformName EmissiveMapping          = "emissiveMapping"
textureMappingUniformName MetallicRoughnessMapping = "metallicRoughnessMapping"
textureMappingUniformName OcclusionMapping         = "occlusionMapping"
