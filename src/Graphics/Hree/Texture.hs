module Graphics.Hree.Texture
    ( Texture(..)
    ) where

import Data.ByteString (ByteString)
import qualified Graphics.GL as GLRaw
import qualified Graphics.Rendering.OpenGL as GL

data TextureConfig = TextureConfig
    { target         :: !GL.TextureTarget2D
    , level          :: !GLRaw.GLuint
    , internalFormat :: !GL.PixelInternalFormat
    , size           :: !GL.TextureSize2D
    , border         :: !GLRaw.GLuint
    , format         :: !GL.PixelFormat
    , dataType       :: !GL.DataType
    , magFilter      :: !GL.MagnificationFilter
    , minFilter      :: !GL.MinificationFilter
    , wrapS          :: !GLRaw.GLuint
    , wrapT          :: !GLRaw.GLuint
    } deriving (Show, Eq)

data Texture = Texture
    { texturePath   :: !ByteString
    , textureConfig :: !TextureConfig
    } deriving (Show, Eq)
