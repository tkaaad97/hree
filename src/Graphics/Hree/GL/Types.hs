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
    , Uniform(..)
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified Graphics.Rendering.OpenGL as GL

data Uniform = forall a. (GL.Uniform a, Show a) => Uniform !a
deriving instance Show Uniform

data UniformInfo = UniformInfo
    { uiUniformName     :: !ByteString
    , uiUniformLocation :: !GL.UniformLocation
    }
    deriving (Show)

data AttribFormat = AttribFormat
    { attribFormatSize           :: !Int
    , attribFormatDataType       :: !GL.DataType
    , attribFormatNormalized     :: !Bool
    , attribFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribInfo = AttribInfo
    { aiAttribName   :: !ByteString
    , aiAttribFormat :: !AttribFormat
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo !GL.Program !(Map ByteString AttribInfo) !(Map ByteString UniformInfo)
    deriving (Show)

data RenderInfo = RenderInfo
    { riProgram     :: !ProgramInfo
    , riDrawMethod  :: !DrawMethod
    , riVertexArray :: !GL.VertexArrayObject
    , riUniforms    :: ![(UniformInfo, Uniform)]
    , riTexture     :: !GL.TextureObject
    }

data DrawMethod =
    DrawArrays !GL.PrimitiveMode !GL.ArrayIndex !GL.NumArrayIndices |
    DrawElements !GL.PrimitiveMode !GL.NumArrayIndices !GL.DataType !(Ptr GL.NumArrayIndices)
    deriving (Show, Eq)

data BufferSource = forall a. Storable a => BufferSource !(Vector a) !GL.BufferUsage

type BindingIndex = Int

data BindBufferSetting = BindBufferSetting
    { bindBufferSettingOffset :: !Int
    , bindBufferSettingStride :: !Int
    } deriving (Show, Eq)

data AttribBinding = AttribBinding
    { attribBindingIndex         :: !Int
    , attribBindingAttribInfo    :: !AttribInfo
    , attribBindingBufferSetting :: !BindBufferSetting
    } deriving (Show, Eq)
