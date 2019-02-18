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
    , ProgramInfo(..)
    , RenderInfo(..)
    , Uniform(..)
    , UniformInfo(..)
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified GLW
import qualified Graphics.GL as GL

data Uniform = forall a. (GLW.Uniform a, Show a) => Uniform !a
deriving instance Show Uniform

data UniformInfo = UniformInfo
    { uiUniformName     :: !ByteString
    , uiUniformLocation :: !GL.GLuint
    , uiUniformSize     :: !GL.GLuint
    , uiUniformDataType :: !GL.GLuint
    }
    deriving (Show)

data AttribFormat = AttribFormat
    { attribFormatSize           :: !Int
    , attribFormatDataType       :: !GL.GLuint
    , attribFormatNormalized     :: !Bool
    , attribFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribInfo = AttribInfo
    { aiAttribName     :: !ByteString
    , aiAttribLocation :: !GL.GLuint
    , aiAttribSize     :: !GL.GLuint
    , aiAttribDataType :: !GL.GLuint
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo
    { programInfoProgram  :: !GLW.Program
    , programInfoAttribs  :: !(Map ByteString AttribInfo)
    , programInfoUniforms :: !(Map ByteString UniformInfo)
    } deriving (Show)

data RenderInfo = RenderInfo
    { riProgram     :: !ProgramInfo
    , riDrawMethod  :: !DrawMethod
    , riVertexArray :: !GLW.VertexArray
    , riUniforms    :: ![(UniformInfo, Uniform)]
    , riTexture     :: !(Maybe GLW.Texture)
    }

data DrawMethod =
    DrawArrays !GLW.PrimitiveType !GL.GLint !GL.GLsizei |
    DrawElements !GLW.PrimitiveType !GL.GLsizei !GL.GLenum !(Ptr ())
    deriving (Show, Eq)

data BufferSource = forall a. Storable a => BufferSource !(Vector a) !GL.GLenum

type BindingIndex = Int

data BindBufferSetting = BindBufferSetting
    { bindBufferSettingOffset :: !Int
    , bindBufferSettingStride :: !Int
    } deriving (Show, Eq)

data AttribBinding = AttribBinding
    { attribBindingIndex        :: !Int
    , attribBindingAttribFormat :: !AttribFormat
    } deriving (Show, Eq)
