{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hree.Material
    ( module Graphics.Hree.Material.BasicMaterial
    , module Graphics.Hree.Material.FlatColorMaterial
    , module Graphics.Hree.Material.SpriteMaterial
    , module Graphics.Hree.Material.StandardMaterial
    , module Graphics.Hree.Material.UserMaterial
    , defaultRenderOption
    , materialHasTextureMapping
    , textureMappingUniformName
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import qualified Data.Vector as BV (find)
import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types (BlendingOption(..), BlendingSeparateOption(..),
                               DepthOption(..), FaceStencilOption(..),
                               RenderOption, RenderOption_(..),
                               StencilFuncArgs(..), StencilOpArgs(..),
                               StencilOption(..))
import Graphics.Hree.Types (Material(..), TextureMappingType(..))
import Linear (V4(..))

import Graphics.Hree.Material.BasicMaterial (BasicMaterial, BasicMaterialBlock,
                                             basicMaterial)
import Graphics.Hree.Material.FlatColorMaterial (FlatColorMaterial,
                                                 FlatColorMaterialBlock,
                                                 flatColorMaterial)
import Graphics.Hree.Material.SpriteMaterial (SpriteMaterial,
                                              SpriteMaterialBlock,
                                              spriteMaterial)
import Graphics.Hree.Material.StandardMaterial (StandardMaterial,
                                                StandardMaterialBlock,
                                                standardMaterial,
                                                standardMaterialBlock)
import Graphics.Hree.Material.UserMaterial (userMaterial)

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

materialHasTextureMapping :: Material b -> TextureMappingType -> Bool
materialHasTextureMapping material textureMappingType = isJust . BV.find ((== textureMappingType) . fst) $ materialMappings material

textureMappingUniformName :: TextureMappingType -> ByteString
textureMappingUniformName BaseColorMapping         = "baseColorMapping"
textureMappingUniformName NormalMapping            = "normalMapping"
textureMappingUniformName EmissiveMapping          = "emissiveMapping"
textureMappingUniformName MetallicRoughnessMapping = "metallicRoughnessMapping"
textureMappingUniformName OcclusionMapping         = "occlusionMapping"
