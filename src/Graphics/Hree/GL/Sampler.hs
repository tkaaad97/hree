{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
module Graphics.Hree.GL.Sampler
    ( SamplerParam(..)
    , SamplerParamValue(..)
    , glTextureBorderColor
    , glTextureBorderColorIi
    , glTextureBorderColorIui
    , glTextureBorderColori
    , glTextureCompareFunc
    , glTextureCompareMode
    , glTextureLodBias
    , glTextureMagFilter
    , glTextureMaxLod
    , glTextureMinFilter
    , glTextureMinLod
    , glTextureWrapR
    , glTextureWrapS
    , glTextureWrapT
    , setSamplerParameter
    , setSamplerParamValue
    , getSamplerParameter
    ) where

import Data.Coerce (coerce)
import qualified Foreign (alloca, castPtr, peek, with)
import qualified GLW
import qualified GLW.Internal.Objects (Sampler(..))
import qualified Graphics.GL as GL
import Linear (V4(..))

data SamplerParam a where
    SamplerParamGLint :: GL.GLenum -> SamplerParam GL.GLint
    SamplerParamGLfloat :: GL.GLenum -> SamplerParam GL.GLfloat
    SamplerParamGLintv4 :: GL.GLenum -> SamplerParam (V4 GL.GLint)
    SamplerParamGLfloatv4 :: GL.GLenum -> SamplerParam (V4 GL.GLfloat)
    SamplerParamInternalGLintv4 :: GL.GLenum -> SamplerParam (V4 GL.GLint)
    SamplerParamInternalGLuintv4 :: GL.GLenum -> SamplerParam (V4 GL.GLuint)

data SamplerParamValue = forall a. SamplerParamValue !(SamplerParam a) !a

glTextureWrapS :: SamplerParam GL.GLint
glTextureWrapS = SamplerParamGLint GL.GL_TEXTURE_WRAP_S

glTextureWrapT :: SamplerParam GL.GLint
glTextureWrapT = SamplerParamGLint GL.GL_TEXTURE_WRAP_T

glTextureWrapR :: SamplerParam GL.GLint
glTextureWrapR = SamplerParamGLint GL.GL_TEXTURE_WRAP_R

glTextureMinFilter :: SamplerParam GL.GLint
glTextureMinFilter = SamplerParamGLint GL.GL_TEXTURE_MIN_FILTER

glTextureMagFilter :: SamplerParam GL.GLint
glTextureMagFilter = SamplerParamGLint GL.GL_TEXTURE_MAG_FILTER

glTextureBorderColor :: SamplerParam (V4 GL.GLfloat)
glTextureBorderColor = SamplerParamGLfloatv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColori :: SamplerParam (V4 GL.GLint)
glTextureBorderColori = SamplerParamGLintv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColorIi :: SamplerParam (V4 GL.GLint)
glTextureBorderColorIi = SamplerParamInternalGLintv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureBorderColorIui :: SamplerParam (V4 GL.GLuint)
glTextureBorderColorIui = SamplerParamInternalGLuintv4 GL.GL_TEXTURE_BORDER_COLOR

glTextureMinLod :: SamplerParam GL.GLfloat
glTextureMinLod = SamplerParamGLfloat GL.GL_TEXTURE_MIN_LOD

glTextureMaxLod :: SamplerParam GL.GLfloat
glTextureMaxLod = SamplerParamGLfloat GL.GL_TEXTURE_MAX_LOD

glTextureLodBias :: SamplerParam GL.GLfloat
glTextureLodBias =  SamplerParamGLfloat GL.GL_TEXTURE_LOD_BIAS

glTextureCompareMode :: SamplerParam GL.GLint
glTextureCompareMode = SamplerParamGLint GL.GL_TEXTURE_COMPARE_MODE

glTextureCompareFunc :: SamplerParam GL.GLint
glTextureCompareFunc = SamplerParamGLint GL.GL_TEXTURE_COMPARE_FUNC

setSamplerParameter :: GLW.Sampler -> SamplerParam a -> a -> IO ()
setSamplerParameter sampler (SamplerParamGLint pname) a = GL.glSamplerParameteri (coerce sampler) pname a
setSamplerParameter sampler (SamplerParamGLfloat pname) a = GL.glSamplerParameterf (coerce sampler) pname a
setSamplerParameter sampler (SamplerParamGLintv4 pname) a =
    Foreign.with a (GL.glSamplerParameteriv (coerce sampler) pname . Foreign.castPtr)
setSamplerParameter sampler (SamplerParamGLfloatv4 pname) a =
    Foreign.with a (GL.glSamplerParameterfv (coerce sampler) pname . Foreign.castPtr)
setSamplerParameter sampler (SamplerParamInternalGLintv4 pname) a =
    Foreign.with a (GL.glSamplerParameterIiv (coerce sampler) pname . Foreign.castPtr)
setSamplerParameter sampler (SamplerParamInternalGLuintv4 pname) a =
    Foreign.with a (GL.glSamplerParameterIuiv (coerce sampler) pname . Foreign.castPtr)

setSamplerParamValue :: GLW.Sampler -> SamplerParamValue -> IO ()
setSamplerParamValue sampler (SamplerParamValue p a) = setSamplerParameter sampler p a

getSamplerParameter :: GLW.Sampler -> SamplerParam a -> IO a
getSamplerParameter sampler (SamplerParamGLint pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameteriv (coerce sampler) pname p
        Foreign.peek p
getSamplerParameter sampler (SamplerParamGLfloat pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameterfv (coerce sampler) pname p
        Foreign.peek p
getSamplerParameter sampler (SamplerParamGLintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameteriv (coerce sampler) pname (Foreign.castPtr p)
        Foreign.peek p
getSamplerParameter sampler (SamplerParamGLfloatv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameterfv (coerce sampler) pname (Foreign.castPtr p)
        Foreign.peek p
getSamplerParameter sampler (SamplerParamInternalGLintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameterIiv (coerce sampler) pname (Foreign.castPtr p)
        Foreign.peek p
getSamplerParameter sampler (SamplerParamInternalGLuintv4 pname) =
    Foreign.alloca $ \p -> do
        GL.glGetSamplerParameterIuiv (coerce sampler) pname (Foreign.castPtr p)
        Foreign.peek p
