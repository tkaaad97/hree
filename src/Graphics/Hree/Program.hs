{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Graphics.Hree.Program
    ( VertexShaderSpec(..)
    , FragmentShaderSpec(..)
    , ProgramSpec(..)
    , basicProgramSpec
    , mkProgram
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString (pack, useAsCStringLen)
import Data.Coerce (Coercible(..))
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Foreign (Ptr)
import qualified Foreign (alloca, allocaArray, peek, peekArray, with)
import qualified Foreign.C.String as Foreign (peekCStringLen)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import System.IO.Error (userError)

data VertexShaderSpec = VertexShaderSpec
    deriving (Show, Eq, Ord)

data FragmentShaderSpec = FragmentShaderSpec
    deriving (Show, Eq, Ord)

data ProgramSpec = ProgramSpec
    { vertexShaderSpec   :: !VertexShaderSpec
    , fragmentShaderSpec :: !FragmentShaderSpec
    } deriving (Show, Eq, Ord)

basicProgramSpec :: ProgramSpec
basicProgramSpec = ProgramSpec VertexShaderSpec FragmentShaderSpec

instance Hashable VertexShaderSpec where
    hash _ = 0
    hashWithSalt = const

instance Hashable FragmentShaderSpec where
    hash _ = 0
    hashWithSalt = const

instance Hashable ProgramSpec where
    hashWithSalt s (ProgramSpec a b) = s `hashWithSalt` a `hashWithSalt` b

mkProgram :: ProgramSpec -> IO ProgramInfo
mkProgram (ProgramSpec vspec fspec) = do
    vshader <- mkVertexShader vspec
    fshader <- mkFragmentShader fspec
    program <- GLW.createObject (Proxy :: Proxy GLW.Program)
    GLW.glAttachShader program vshader
    GLW.glAttachShader program fshader
    GLW.glLinkProgram program
    throwIfProgramErrorStatus program GL.GL_LINK_STATUS "program link error"
    GLW.deleteObject vshader
    GLW.deleteObject fshader

    attribs <- getActiveAttribs program
    uniforms <- getActiveUniforms program

    let attribMap = Map.fromList $ zip (map aiAttribName attribs) attribs
        uniformMap = Map.fromList $ zip (map uiUniformName uniforms) uniforms

    return $ ProgramInfo program attribMap uniformMap

mkVertexShader :: VertexShaderSpec -> IO (GLW.Shader 'GLW.GL_VERTEX_SHADER)
mkVertexShader _ = mkShader (Proxy :: Proxy GLW.GL_VERTEX_SHADER) shaderSource
    where
    shaderSource = $(embedFile "shader/basic-vertex.glsl")

mkFragmentShader :: FragmentShaderSpec -> IO (GLW.Shader 'GLW.GL_FRAGMENT_SHADER)
mkFragmentShader _ = mkShader (Proxy :: Proxy GLW.GL_FRAGMENT_SHADER) shaderSource
    where
    shaderSource = $(embedFile "shader/basic-fragment.glsl")

mkShader :: forall (k :: GLW.ShaderType). GLW.SingShaderType k => Proxy k -> ByteString -> IO (GLW.Shader k)
mkShader _ source =
    ByteString.useAsCStringLen source $ \(source', len) ->
    Foreign.with source' $ \sp ->
    Foreign.with (fromIntegral len) $ \lp -> do
        shader <- GLW.createObject (Proxy :: Proxy (GLW.Shader k))
        GLW.glShaderSource shader 1 sp lp
        GLW.glCompileShader shader
        throwIfShaderErrorStatus shader GL.GL_COMPILE_STATUS "shader compile error"
        return shader

getActiveAttribs :: GLW.Program -> IO [AttribInfo]
getActiveAttribs = getActivePorts AttribInfo (GLW.AttribLocation . fromIntegral) GL.GL_ACTIVE_ATTRIBUTES GLW.glGetActiveAttrib

getActiveUniforms :: GLW.Program -> IO [UniformInfo]
getActiveUniforms = getActivePorts UniformInfo GLW.UniformLocation GL.GL_ACTIVE_UNIFORMS GLW.glGetActiveUniform

getActivePorts ::
    (ByteString -> location -> GL.GLuint -> GL.GLuint -> a)
    -> (GL.GLint -> location)
    -> GL.GLenum
    -> (GLW.Program -> GL.GLuint -> GL.GLsizei -> Ptr GL.GLsizei -> Ptr GL.GLint -> Ptr GL.GLenum -> Ptr GL.GLchar -> IO ())
    -> GLW.Program
    -> IO [a]
getActivePorts construct toLocation pname f program = do
    len  <- Foreign.alloca $ \p -> GLW.glGetProgramiv program pname p >> Foreign.peek p
    mapM g [0..(len - 1)]

    where
    maxNameBytes = 128
    g i =
        Foreign.alloca $ \lp ->
        Foreign.alloca $ \sp ->
        Foreign.alloca $ \tp ->
        Foreign.allocaArray maxNameBytes $ \np -> do
            f program (fromIntegral i) (fromIntegral maxNameBytes) lp sp tp np
            length <- Foreign.peek lp
            size <- Foreign.peek sp
            dataType <- Foreign.peek tp
            name <- ByteString.pack <$> Foreign.peekCStringLen (np, fromIntegral length)
            return $ construct name (toLocation i) (fromIntegral size) dataType

throwIfProgramErrorStatus
    :: GLW.Program
    -> GL.GLenum
    -> String
    -> IO ()
throwIfProgramErrorStatus program statusName messagePrefix = Foreign.alloca $ \p -> do
    GLW.glGetProgramiv program statusName p
    status <- Foreign.peek p
    unless (status == GL.GL_TRUE) $
        Foreign.alloca $ \sizePtr ->
        Foreign.allocaArray bufSize $ \buf -> do
            log' <- GLW.glGetProgramInfoLog program (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log
    where
    bufSize = 256

throwIfShaderErrorStatus
    :: GLW.Shader t
    -> GL.GLenum
    -> String
    -> IO ()
throwIfShaderErrorStatus shader statusName messagePrefix = Foreign.alloca $ \p -> do
    GLW.glGetShaderiv shader statusName p
    status <- Foreign.peek p
    unless (status == GL.GL_TRUE) $
        Foreign.alloca $ \sizePtr ->
        Foreign.allocaArray bufSize $ \buf -> do
            log' <- GLW.glGetShaderInfoLog shader (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log
    where
    bufSize = 256
