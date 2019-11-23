{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
module Graphics.Hree.GL.Types
    ( AttribBinding(..)
    , AttribFormat(..)
    , AttribInfo(..)
    , BindingIndex
    , BufferSource(..)
    , BindBufferSetting(..)
    , DrawMethod(..)
    , IndexBuffer(..)
    , ProgramInfo(..)
    , RenderInfo(..)
    , Uniform(..)
    , UniformInfo(..)
    , UniformBlockInfo(..)
    , Texture(..)
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Storable as SV (Vector)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified GLW
import qualified Graphics.GL as GL

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
    { ubiUniformBlockName     :: !ByteString
    , ubiUniformBlockIndex    :: !GL.GLuint
    , ubiUniformBlockDataSize :: !GL.GLsizei
    , ubiUniformBlockUniforms :: !(BV.Vector UniformInfo)
    } deriving (Show, Eq)

data AttribFormat = AttribFormat
    { attribFormatSize           :: !Int
    , attribFormatComponentType  :: !GL.GLenum
    , attribFormatNormalized     :: !Bool
    , attribFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribInfo = AttribInfo
    { aiAttribName     :: !ByteString
    , aiAttribLocation :: !GLW.AttribLocation
    , aiAttribSize     :: !GL.GLuint
    , aiAttribDataType :: !GL.GLuint
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo
    { programInfoProgram          :: !GLW.Program
    , programInfoAttribs          :: !(Map ByteString AttribInfo)
    , programInfoUniforms         :: !(Map ByteString UniformInfo)
    , programInfoUniformLocations :: !(Map ByteString GLW.UniformLocation)
    } deriving (Show)

data RenderInfo = RenderInfo
    { riProgram     :: !ProgramInfo
    , riDrawMethod  :: !DrawMethod
    , riVertexArray :: !GLW.VertexArray
    , riUniforms    :: ![(GLW.UniformLocation, Uniform)]
    , riTextures    :: ![Texture]
    }

data DrawMethod =
    DrawArrays !GLW.PrimitiveType !GL.GLint !GL.GLsizei |
    DrawElements !GLW.PrimitiveType !GL.GLsizei !GL.GLenum !(Ptr ()) |
    DrawArraysInstanced !GLW.PrimitiveType !GL.GLint !GL.GLsizei !GL.GLsizei |
    DrawElementsInstanced !GLW.PrimitiveType !GL.GLsizei !GL.GLenum !(Ptr ()) !GL.GLsizei
    deriving (Show, Eq)

data BufferSource =
    forall a. Storable a => BufferSourceVector !(SV.Vector a) !GL.GLenum |
    BufferSourceByteString !ByteString !GL.GLenum

type BindingIndex = Int

data BindBufferSetting = BindBufferSetting
    { bindBufferSettingOffset  :: !Int
    , bindBufferSettingStride  :: !Int
    , bindBufferSettingDivisor :: !Int
    } deriving (Show, Eq)

data AttribBinding = AttribBinding
    { attribBindingIndex        :: !GLW.BindingIndex
    , attribBindingAttribFormat :: !AttribFormat
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
