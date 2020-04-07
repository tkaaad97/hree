{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
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
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Storable as SV (Vector)
import Data.Word (Word32)
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

data RenderInfo = RenderInfo
    { riProgram       :: !ProgramInfo
    , riDrawMethod    :: !DrawMethod
    , riVertexArray   :: !GLW.VertexArray
    , riUniforms      :: !(BV.Vector (GLW.UniformLocation, Uniform))
    , riUniformBlocks :: !(BV.Vector (BufferBindingIndex, GLW.Buffer))
    , riTextures      :: !(BV.Vector Texture)
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

type BVec2 = V2 Bool
type BVec3 = V3 Bool
type BVec4 = V4 Bool
type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
type IVec2 = V2 Int32
type IVec3 = V3 Int32
type IVec4 = V4 Int32
type UVec2 = V2 Word32
type UVec3 = V3 Word32
type UVec4 = V4 Word32
type DVec2 = V2 Double
type DVec3 = V3 Double
type DVec4 = V4 Double

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
