module Graphics.Hree.Texture
    ( Texture(..)
    ) where

import Data.ByteString (ByteString)
import qualified GLW
import qualified Graphics.GL as GL

data TextureConfig = TextureConfig
    { target         :: !GLW.TextureTarget
    , level          :: !GL.GLint
    , internalFormat :: !GL.GLint
    , width          :: !GL.GLsizei
    , height         :: !GL.GLsizei
    , border         :: !GL.GLint
    , format         :: !GLW.PixelFormat
    , dataType       :: !GL.GLenum
    , magFilter      :: !GLW.TextureMagFilter
    , minFilter      :: !GLW.TextureMinFilter
    , wrapS          :: !GL.GLint
    , wrapT          :: !GL.GLint
    } deriving (Show, Eq)

data Texture = Texture
    { texturePath   :: !ByteString
    , textureConfig :: !TextureConfig
    } deriving (Show, Eq)
