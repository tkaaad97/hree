{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Graphics.Hree.Texture
    ( TextureSettings(..)
    , TextureSourceData(..)
    , TextureParam(..)
    , glDepthStencilTextureMode
    , glTextureBaseLevel
    , glTextureCompareFunc
    , glTextureCompareMode
    , glTextureLodBias
    , glTextureMinFilter
    , glTextureMagFilter
    , glTextureMinLod
    , glTextureMaxLod
    , glTextureMaxLevel
    , glTextureSwizzleR
    , glTextureSwizzleG
    , glTextureSwizzleB
    , glTextureSwizzleA
    , glTextureSwizzleRGBA
    , glTextureWrapS
    , glTextureWrapT
    , glTextureWrapR
    , glTextureBorderColor
    , glTextureBorderColori
    , glTextureBorderColorIi
    , glTextureBorderColorIui
    , setTextureParameter
    , getTextureParameter
    ) where

import Data.Coerce (coerce)
import Foreign (Ptr)
import qualified Foreign (alloca, castPtr, peek, with)
import qualified GLW
import qualified GLW.Internal.Objects (Texture(..))
import qualified Graphics.GL as GL
import Linear (V4)

data TextureSettings = TextureSettings
    { textureLevels         :: !GL.GLint
    , textureInternalFormat :: !GL.GLenum
    , textureWidth          :: !GL.GLsizei
    , textureHeight         :: !GL.GLsizei
    , textureGenerateMipmap :: !Bool
    } deriving (Show, Eq)

data TextureSourceData = TextureSourceData
    { sourceWidth    :: !GL.GLsizei
    , sourceHeight   :: !GL.GLsizei
    , sourceFormat   :: !GLW.PixelFormat
    , sourceDataType :: !GL.GLenum
    , sourcePixels   :: !(Ptr ())
    }

data TextureParam a where
    TextureParamGLint :: GL.GLenum -> TextureParam GL.GLint
    TextureParamGLfloat :: GL.GLenum -> TextureParam GL.GLfloat
    TextureParamGLintv4 :: GL.GLenum -> TextureParam (V4 GL.GLint)
    TextureParamGLfloatv4 :: GL.GLenum -> TextureParam (V4 GL.GLfloat)
    TextureParamInternalGLintv4 :: GL.GLenum -> TextureParam (V4 GL.GLint)
    TextureParamInternalGLuintv4 :: GL.GLenum -> TextureParam (V4 GL.GLuint)

glDepthStencilTextureMode :: TextureParam GL.GLint
glDepthStencilTextureMode = TextureParamGLint GL.GL_DEPTH_STENCIL_TEXTURE_MODE

glTextureBaseLevel :: TextureParam GL.GLint
glTextureBaseLevel = TextureParamGLint GL.GL_TEXTURE_BASE_LEVEL

glTextureCompareFunc :: TextureParam GL.GLint
glTextureCompareFunc = TextureParamGLint GL.GL_TEXTURE_COMPARE_FUNC

glTextureCompareMode :: TextureParam GL.GLint
glTextureCompareMode = TextureParamGLint GL.GL_TEXTURE_COMPARE_MODE

glTextureLodBias :: TextureParam GL.GLfloat
glTextureLodBias =  TextureParamGLfloat GL.GL_TEXTURE_LOD_BIAS

glTextureMinFilter :: TextureParam GL.GLint
glTextureMinFilter = TextureParamGLint GL.GL_TEXTURE_MIN_FILTER

glTextureMagFilter :: TextureParam GL.GLint
glTextureMagFilter = TextureParamGLint GL.GL_TEXTURE_MAG_FILTER

glTextureMinLod :: TextureParam GL.GLfloat
glTextureMinLod = TextureParamGLfloat GL.GL_TEXTURE_MIN_LOD

glTextureMaxLod :: TextureParam GL.GLfloat
glTextureMaxLod = TextureParamGLfloat GL.GL_TEXTURE_MAX_LOD

glTextureMaxLevel :: TextureParam GL.GLint
glTextureMaxLevel = TextureParamGLint GL.GL_TEXTURE_MAX_LEVEL

glTextureSwizzleR :: TextureParam GL.GLint
glTextureSwizzleR = TextureParamGLint GL.GL_TEXTURE_SWIZZLE_R

glTextureSwizzleG :: TextureParam GL.GLint
glTextureSwizzleG = TextureParamGLint GL.GL_TEXTURE_SWIZZLE_G

glTextureSwizzleB :: TextureParam GL.GLint
glTextureSwizzleB = TextureParamGLint GL.GL_TEXTURE_SWIZZLE_B

glTextureSwizzleA :: TextureParam GL.GLint
glTextureSwizzleA = TextureParamGLint GL.GL_TEXTURE_SWIZZLE_A

glTextureSwizzleRGBA :: TextureParam (V4 GL.GLint)
glTextureSwizzleRGBA = TextureParamGLintv4 GL.GL_TEXTURE_SWIZZLE_RGBA

glTextureWrapS :: TextureParam GL.GLint
glTextureWrapS = TextureParamGLint GL.GL_TEXTURE_WRAP_S

glTextureWrapT :: TextureParam GL.GLint
glTextureWrapT = TextureParamGLint GL.GL_TEXTURE_WRAP_T

glTextureWrapR :: TextureParam GL.GLint
glTextureWrapR = TextureParamGLint GL.GL_TEXTURE_WRAP_R

glTextureBorderColor :: TextureParam (V4 GL.GLfloat)
glTextureBorderColor = TextureParamGLfloatv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColori :: TextureParam (V4 GL.GLint)
glTextureBorderColori = TextureParamGLintv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColorIi :: TextureParam (V4 GL.GLint)
glTextureBorderColorIi = TextureParamInternalGLintv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColorIui :: TextureParam (V4 GL.GLuint)
glTextureBorderColorIui = TextureParamInternalGLuintv4 GL.GL_TEXTURE_BORDER_COLOR

setTextureParameter :: GLW.Texture 'GLW.GL_TEXTURE_2D -> TextureParam a -> a -> IO ()
setTextureParameter texture (TextureParamGLint pname) a = GL.glTextureParameteri (coerce texture) pname a
setTextureParameter texture (TextureParamGLfloat pname) a = GL.glTextureParameterf (coerce texture) pname a
setTextureParameter texture (TextureParamGLintv4 pname) a =
    Foreign.with a (GL.glTextureParameteriv (coerce texture) pname . Foreign.castPtr)
setTextureParameter texture (TextureParamGLfloatv4 pname) a =
    Foreign.with a (GL.glTextureParameterfv (coerce texture) pname . Foreign.castPtr)
setTextureParameter texture (TextureParamInternalGLintv4 pname) a =
    Foreign.with a (GL.glTextureParameterIiv (coerce texture) pname . Foreign.castPtr)
setTextureParameter texture (TextureParamInternalGLuintv4 pname) a =
    Foreign.with a (GL.glTextureParameterIuiv (coerce texture) pname . Foreign.castPtr)

getTextureParameter :: GLW.Texture 'GLW.GL_TEXTURE_2D -> TextureParam a -> IO a
getTextureParameter texture (TextureParamGLint pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameteriv (coerce texture) pname p
        Foreign.peek p
getTextureParameter texture (TextureParamGLfloat pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameterfv (coerce texture) pname p
        Foreign.peek p
getTextureParameter texture (TextureParamGLintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameteriv (coerce texture) pname (Foreign.castPtr p)
        Foreign.peek p
getTextureParameter texture (TextureParamGLfloatv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameterfv (coerce texture) pname (Foreign.castPtr p)
        Foreign.peek p
getTextureParameter texture (TextureParamInternalGLintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameterIiv (coerce texture) pname (Foreign.castPtr p)
        Foreign.peek p
getTextureParameter texture (TextureParamInternalGLuintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetTextureParameterIuiv (coerce texture) pname (Foreign.castPtr p)
        Foreign.peek p
