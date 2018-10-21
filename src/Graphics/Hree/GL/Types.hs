{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
module Graphics.Hree.GL.Types
    (
    ) where

import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import qualified Graphics.Rendering.OpenGL as GL

data Uniform = forall a. (GL.Uniform a, Show a) => Uniform !a
deriving instance Show Uniform

data UniformInfo = UniformInfo !GL.UniformLocation !Uniform
    deriving (Show)

data AttribInfo = AttribInfo
    { aiAttribLocation  :: !GL.AttribLocation
    , aiDataType        :: !GL.DataType
    , aiNumArrayIndices :: !Int
    , aiIntegerHandling :: !GL.IntegerHandling
    , aiStride          :: !Int
    , aiOffset          :: !Int
    } deriving (Show, Eq)

data ProgramInfo = ProgramInfo !GL.Program ![AttribInfo] ![GL.UniformLocation]
    deriving (Show)

data RenderInfo = RenderInfo
    { riProgram      :: !ProgramInfo
    , riMode         :: !GL.PrimitiveMode
    , riVertexBuffer :: !GL.BufferObject
    , riIndex        :: !GL.ArrayIndex
    , riNum          :: !GL.NumArrayIndices
    , riUniformInfos :: ![UniformInfo]
    , riTexture      :: !GL.TextureObject
    }

data RenderProgramInfos = RenderProgramInfos
    { rpiTriangleProgramInfo :: !ProgramInfo
    , rpiArcProgramInfo      :: !ProgramInfo
    , rpiLineProgramInfo     :: !ProgramInfo
    } deriving (Show)

data RenderResource = RenderResource
    { rrRenderProgramInfos :: !RenderProgramInfos
    , rrTextures           :: !(Map Text GL.TextureObject)
    } deriving (Show)
