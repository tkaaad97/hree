module Graphics.Hree.Sampler
    (
    ) where

import qualified GLW
import qualified Graphics.GL as GL
import Linear (V4)

data SamplerSettings = SamplerSettings
    { minFilter   :: !GL.GLint
    , magFilter   :: !GL.GLint
    , minLod      :: !GL.GLfloat
    , maxLod      :: !GL.GLfloat
    , wrapS       :: !GL.GLint
    , wrapT       :: !GL.GLint
    , wrapR       :: !GL.GLint
    , borderColor :: !(V4 GL.GLfloat)
    , compareMode :: !GL.GLint
    , compareFunc :: !GL.GLint
    } deriving (Show, Eq)

defaultSamplerSettings :: SamplerSettings
defaultSamplerSettings = SamplerSettings
    { minFilter = GL.GL_NEAREST_MIPMAP_LINEAR
    , magFilter = GL.GL_LINEAR
    , minLod = -1000.0
    , maxLod = 1000.0
    , wrapS = GL.GL_REPEAT
    , wrapT = GL.GL_REPEAT
    , wrapR = GL.GL_REPEAT
    , borderColor = V4 0 0 0 0
    , compareMode = GL.GL_NONE
    , compareFunc = GL.GL_LEQUAL
    }

data Sampler2D = Sampler2D
    { samplerTexture :: !GLW.Texture
    , samplerSetting :: !SamplerSetting
    } deriving (Show, Eq)
