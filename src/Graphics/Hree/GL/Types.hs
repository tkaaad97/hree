{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
module Graphics.Hree.GL.Types
    ( AttribBinding(..)
    , AttributeFormat(..)
    , AttribFormat(..)
    , AttribIFormat(..)
    , AttribLFormat(..)
    , AttribInfo(..)
    , BufferSource(..)
    , BufferBindingIndex(..)
    , BindBufferSetting(..)
    , DrawMethod(..)
    , IndexBuffer(..)
    , ProgramInfo(..)
    , RenderInfo(..)
    , RenderOption_(..)
    , RenderOption
    , PartialRenderOption
    , DepthOption(..)
    , BlendingOption(..)
    , BlendingSeparateOption(..)
    , StencilOption(..)
    , StencilFuncArgs(..)
    , StencilOpArgs(..)
    , FaceStencilOption(..)
    , Uniform(..)
    , UniformInfo(..)
    , UniformBlockInfo(..)
    , Texture(..)
    , BVec2
    , BVec3
    , BVec4
    , DMat2
    , DMat2x3
    , DMat2x4
    , DMat3
    , DMat3x2
    , DMat3x4
    , DMat4
    , DMat4x2
    , DMat4x3
    , DVec2
    , DVec3
    , DVec4
    , IVec2
    , IVec3
    , IVec4
    , Mat2
    , Mat2x3
    , Mat2x4
    , Mat3
    , Mat3x2
    , Mat3x4
    , Mat4
    , Mat4x2
    , Mat4x3
    , UVec2
    , UVec3
    , UVec4
    , Vec2
    , Vec3
    , Vec4
    , LimitedVector(..)
    , partialRenderOptionCullFace
    , partialRenderOptionFlipSided
    , partialRenderOptionDepth
    , partialRenderOptionBlending
    , partialRenderOptionStencil
    , partialRenderOptionColorMask
    , setPartialRenderOptionCullFace
    , setPartialRenderOptionFlipSided
    , setPartialRenderOptionDepth
    , setPartialRenderOptionBlending
    , setPartialRenderOptionStencil
    , setPartialRenderOptionColorMask
    ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Last(..), Semigroup(..))
import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Storable as SV (Vector)
import Foreign (Ptr, Storable)
import GHC.TypeNats (Nat)
import qualified GLW
import qualified Graphics.GL as GL
import Linear (M22, M23, M24, M32, M33, M34, M42, M43, M44, V2, V3, V4)

data Uniform = forall a. (GLW.Uniform a, Show a) => Uniform !a
deriving instance Show Uniform

data UniformInfo = UniformInfo
    { uiUniformName         :: !ByteString
    , uiUniformDataType     :: !GL.GLuint
    , uiUniformSize         :: !GL.GLuint
    , uiUniformOffset       :: !GL.GLint
    , uiUniformArrayStride  :: !GL.GLint
    , uiUniformMatrixStride :: !GL.GLint
    , uiUniformIsRowMajor   :: !Bool
    } deriving (Show, Eq)

data UniformBlockInfo = UniformBlockInfo
    { ubiUniformBlockName  :: !ByteString
    , ubiUniformBlockIndex :: !GL.GLuint
    } deriving (Show, Eq)

data AttributeFormat =
    AttribFormat' AttribFormat |
    AttribIFormat' AttribIFormat |
    AttribLFormat' AttribLFormat
    deriving (Show, Eq)

data AttribFormat = AttribFormat
    { attribFormatSize           :: !Int
    , attribFormatComponentType  :: !GL.GLenum
    , attribFormatNormalized     :: !Bool
    , attribFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribIFormat = AttribIFormat
    { attribIFormatSize           :: !Int
    , attribIFormatComponentType  :: !GL.GLenum
    , attribIFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribLFormat = AttribLFormat
    { attribLFormatSize           :: !Int
    , attribLFormatComponentType  :: !GL.GLenum
    , attribLFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribInfo = AttribInfo
    { aiAttribName     :: !ByteString
    , aiAttribLocation :: !GLW.AttribLocation
    , aiAttribSize     :: !GL.GLuint
    , aiAttribDataType :: !GL.GLuint
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo
    { programInfoProgram             :: !GLW.Program
    , programInfoAttribs             :: !(Map ByteString AttribInfo)
    , programInfoUniforms            :: !(Map ByteString UniformInfo)
    , programInfoUniformLocations    :: !(Map ByteString GLW.UniformLocation)
    , programInfoUniformBlocks       :: !(Map ByteString UniformBlockInfo)
    , programInfoBufferBindingPoints :: !(BV.Vector (ByteString, BufferBindingIndex))
    } deriving (Show)

data DepthOption = DepthOption
    { depthOptionDepthTest     :: !Bool
    , depthOptionDepthMask     :: !Bool
    , depthOptionDepthFunction :: !GLW.DepthFunction
    } deriving (Show, Eq)

data BlendingSeparateOption = BlendingSeparateOption
    { blendingSeparateOptionBlendEquation :: !GL.GLenum
    , blendingSeparateOptionFunction      :: !(GL.GLenum, GL.GLenum)
    } deriving (Show, Eq)

data BlendingOption = BlendingOption
    { blendingOptionEnabled :: !Bool
    , blendingOptionRGB     :: !BlendingSeparateOption
    , blendingOptionAlpha   :: !BlendingSeparateOption
    } deriving (Show, Eq)

data StencilFuncArgs = StencilFuncArgs
    { stencilFuncArgsFunc :: !GLW.StencilFunction
    , stencilFuncArgsRef  :: !GL.GLint
    , stencilFuncArgsMask :: !GL.GLuint
    } deriving (Show, Eq)

data StencilOpArgs = StencilOpArgs
    { stencilOpArgsSFail  :: !GLW.StencilOp
    , stencilOpArgsDpFail :: !GLW.StencilOp
    , stencilOpArgsDpPass :: !GLW.StencilOp
    } deriving (Show, Eq)

data FaceStencilOption = FaceStencilOption
    { faceStencilOptionStencilFunc :: !StencilFuncArgs
    , faceStencilOptionStencilOp   :: !StencilOpArgs
    , faceStencilOptionStencilMask :: !GL.GLuint
    } deriving (Show, Eq)

data StencilOption = StencilOption
    { stencilOptionStencilTest :: !Bool
    , stencilOptionFront       :: !FaceStencilOption
    , stencilOptionBack        :: !FaceStencilOption
    } deriving (Show, Eq)

data RenderOption_ f = RenderOption
    { renderOptionCullFace  :: !(f (Maybe GLW.CullFaceMode))
    , renderOptionFlipSided :: !(f Bool)
    , renderOptionDepth     :: !(f DepthOption)
    , renderOptionBlending  :: !(f BlendingOption)
    , renderOptionStencil   :: !(f StencilOption)
    , renderOptionColorMask :: !(f BVec4)
    }

newtype LastMaybe a = LastMaybe
    { unLastMaybe :: Last (Maybe a)
    } deriving (Show, Eq, Semigroup)

type RenderOption = RenderOption_ Identity
type PartialRenderOption = RenderOption_ LastMaybe

deriving instance Eq RenderOption
deriving instance Eq PartialRenderOption
deriving instance Show RenderOption
deriving instance Show PartialRenderOption

instance Monoid (LastMaybe a) where
    mempty = LastMaybe (Last Nothing)

instance Semigroup PartialRenderOption where
    a <> b = RenderOption
        { renderOptionCullFace  = renderOptionCullFace a <> renderOptionCullFace b
        , renderOptionFlipSided = renderOptionFlipSided a <> renderOptionFlipSided b
        , renderOptionDepth     = renderOptionDepth a <> renderOptionDepth b
        , renderOptionBlending  = renderOptionBlending a <> renderOptionBlending b
        , renderOptionStencil   = renderOptionStencil a <> renderOptionStencil b
        , renderOptionColorMask = renderOptionColorMask a <> renderOptionColorMask b
        }

instance Monoid PartialRenderOption where
    mempty = RenderOption
        { renderOptionCullFace  = mempty
        , renderOptionFlipSided = mempty
        , renderOptionDepth     = mempty
        , renderOptionBlending  = mempty
        , renderOptionStencil   = mempty
        , renderOptionColorMask = mempty
        }

partialRenderOptionCullFace :: PartialRenderOption -> Maybe (Maybe GLW.CullFaceMode)
partialRenderOptionCullFace = coerce . renderOptionCullFace

partialRenderOptionFlipSided :: PartialRenderOption -> Maybe Bool
partialRenderOptionFlipSided = coerce . renderOptionFlipSided

partialRenderOptionDepth :: PartialRenderOption -> Maybe DepthOption
partialRenderOptionDepth = coerce . renderOptionDepth

partialRenderOptionBlending :: PartialRenderOption -> Maybe BlendingOption
partialRenderOptionBlending = coerce . renderOptionBlending

partialRenderOptionStencil :: PartialRenderOption -> Maybe StencilOption
partialRenderOptionStencil = coerce . renderOptionStencil

partialRenderOptionColorMask :: PartialRenderOption -> Maybe BVec4
partialRenderOptionColorMask = coerce . renderOptionColorMask

setPartialRenderOptionCullFace :: PartialRenderOption -> Maybe (Maybe GLW.CullFaceMode) -> PartialRenderOption
setPartialRenderOptionCullFace option a = option { renderOptionCullFace = coerce a }

setPartialRenderOptionFlipSided :: PartialRenderOption -> Maybe Bool -> PartialRenderOption
setPartialRenderOptionFlipSided option a = option { renderOptionFlipSided = coerce a }

setPartialRenderOptionDepth :: PartialRenderOption -> Maybe DepthOption -> PartialRenderOption
setPartialRenderOptionDepth option a = option { renderOptionDepth = coerce a }

setPartialRenderOptionBlending :: PartialRenderOption -> Maybe BlendingOption -> PartialRenderOption
setPartialRenderOptionBlending option a = option { renderOptionBlending = coerce a }

setPartialRenderOptionStencil :: PartialRenderOption -> Maybe StencilOption -> PartialRenderOption
setPartialRenderOptionStencil option a = option { renderOptionStencil = coerce a }

setPartialRenderOptionColorMask :: PartialRenderOption -> Maybe BVec4 -> PartialRenderOption
setPartialRenderOptionColorMask option a = option { renderOptionColorMask = coerce a }

data RenderInfo = RenderInfo
    { riProgram       :: !ProgramInfo
    , riDrawMethod    :: !DrawMethod
    , riVertexArray   :: !GLW.VertexArray
    , riUniforms      :: !(BV.Vector (GLW.UniformLocation, Uniform))
    , riUniformBlocks :: !(BV.Vector (BufferBindingIndex, GLW.Buffer))
    , riTextures      :: !(BV.Vector (GL.GLuint, Texture))
    , riRenderOption  :: !RenderOption
    }

data DrawMethod =
    DrawArrays !GLW.PrimitiveType !GL.GLint !GL.GLsizei |
    DrawElements !GLW.PrimitiveType !GL.GLsizei !GL.GLenum !(Ptr ()) |
    DrawArraysInstanced !GLW.PrimitiveType !GL.GLint !GL.GLsizei !GL.GLsizei |
    DrawElementsInstanced !GLW.PrimitiveType !GL.GLsizei !GL.GLenum !(Ptr ()) !GL.GLsizei
    deriving (Show, Eq)

data BufferSource =
    forall a. Storable a => BufferSourcePtr !(Ptr a) !GL.GLenum |
    forall a. Storable a => BufferSourceVector !(SV.Vector a) !GL.GLenum |
    BufferSourceByteString !ByteString !GL.GLenum

data BindBufferSetting = BindBufferSetting
    { bindBufferSettingOffset  :: !Int
    , bindBufferSettingStride  :: !Int
    , bindBufferSettingDivisor :: !Int
    } deriving (Show, Eq)

data AttribBinding = AttribBinding
    { attribBindingIndex           :: !GLW.BindingIndex
    , attribBindingAttributeFormat :: !AttributeFormat
    } deriving (Show, Eq)

newtype Texture = Texture
    { unTexture :: (GLW.Texture 'GLW.GL_TEXTURE_2D, GLW.Sampler)
    } deriving (Show, Eq)

data IndexBuffer = IndexBuffer
    { ibBuffer     :: !GLW.Buffer
    , ibDataType   :: !GL.GLenum
    , ibCount      :: !GL.GLsizei
    , ibByteOffset :: !Int
    } deriving (Show, Eq)

newtype LimitedVector (n :: Nat) a = LimitedVector
    { unLimitedVector :: SV.Vector a
    } deriving (Show, Eq)

newtype BufferBindingIndex = BufferBindingIndex
    { unBufferBindingIndex :: GL.GLuint
    } deriving (Show, Eq)

type BVec2 = V2 GL.GLboolean
type BVec3 = V3 GL.GLboolean
type BVec4 = V4 GL.GLboolean
type Vec2 = V2 GL.GLfloat
type Vec3 = V3 GL.GLfloat
type Vec4 = V4 GL.GLfloat
type IVec2 = V2 GL.GLint
type IVec3 = V3 GL.GLint
type IVec4 = V4 GL.GLint
type UVec2 = V2 GL.GLuint
type UVec3 = V3 GL.GLuint
type UVec4 = V4 GL.GLuint
type DVec2 = V2 GL.GLdouble
type DVec3 = V3 GL.GLdouble
type DVec4 = V4 GL.GLdouble

-- glsl's mat{C}x{R} is C-column by R-row
-- linear's M{R}{C} is R-row by C-column
type Mat2 = M22 Float
type Mat2x3 = M32 Float
type Mat2x4 = M42 Float
type Mat3 = M33 Float
type Mat3x2 = M23 Float
type Mat3x4 = M43 Float
type Mat4 = M44 Float
type Mat4x2 = M24 Float
type Mat4x3 = M34 Float
type DMat2 = M22 Double
type DMat2x3 = M32 Double
type DMat2x4 = M42 Double
type DMat3 = M33 Double
type DMat3x2 = M23 Double
type DMat3x4 = M43 Double
type DMat4 = M44 Double
type DMat4x2 = M24 Double
type DMat4x3 = M34 Double
