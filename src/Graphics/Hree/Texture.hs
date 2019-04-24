module Graphics.Hree.Texture
    ( TextureSettings(..)
    ) where

import Data.ByteString (ByteString)
import qualified GLW
import qualified Graphics.GL as GL

data TextureSettings = TextureSettings
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
