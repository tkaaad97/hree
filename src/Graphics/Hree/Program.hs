{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Foreign (Ptr, alloca, allocaArray, peek, peekArray, with)
import Foreign.C.String (peekCStringLen)
import qualified Graphics.GL as GLRaw
import Graphics.Hree.GL.Types
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Error (userError)
import Unsafe.Coerce (unsafeCoerce)

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
    program <- GL.createProgram
    mapM_ (GL.attachShader program) [vshader, fshader]
    GL.linkProgram program
    checkStatus GL.linkStatus GL.programInfoLog "program link error" program
    GL.deleteObjectNames [vshader, fshader]

    attribs <- getActiveAttribs (unsafeCoerce program)
    uniforms <- getActiveUniforms (unsafeCoerce program)

    let attribMap = Map.fromList $ zip (map aiAttribName attribs) attribs
        uniformMap = Map.fromList $ zip (map uiUniformName uniforms) uniforms

    return $ ProgramInfo program attribMap uniformMap

mkVertexShader :: VertexShaderSpec -> IO GL.Shader
mkVertexShader _ = mkShader "vertexShader" GL.VertexShader shaderSource
    where
    shaderSource = $(embedFile "shader/basic-vertex.glsl")

mkFragmentShader :: FragmentShaderSpec -> IO GL.Shader
mkFragmentShader _ = mkShader "vertexShader" GL.FragmentShader shaderSource
    where
    shaderSource = $(embedFile "shader/basic-fragment.glsl")

mkShader :: String -> GL.ShaderType -> ByteString -> IO GL.Shader
mkShader name shaderType src = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader GL.$= src
    GL.compileShader shader
    checkStatus GL.compileStatus GL.shaderInfoLog ("shader " ++ name ++ " compile error") shader
    return shader

checkStatus
    :: (a -> GL.GettableStateVar Bool)
    -> (a -> GL.GettableStateVar String)
    -> String
    -> a
    -> IO ()
checkStatus getStatus getInfoLog message object = do
    ok <- GL.get . getStatus $ object
    unless ok $ do
        log' <- GL.get . getInfoLog $ object
        throwIO . userError $ message ++ ": " ++ log'

getActiveAttribs :: GL.GLuint -> IO [AttribInfo]
getActiveAttribs = getActivePorts AttribInfo GLRaw.GL_ACTIVE_ATTRIBUTES GLRaw.glGetActiveAttrib

getActiveUniforms :: GL.GLuint -> IO [UniformInfo]
getActiveUniforms = getActivePorts UniformInfo GLRaw.GL_ACTIVE_UNIFORMS GLRaw.glGetActiveUniform

getActivePorts ::
    (ByteString -> GLRaw.GLuint -> GLRaw.GLuint -> GL.GLuint -> a)
    -> GLRaw.GLenum
    -> (GLRaw.GLuint -> GLRaw.GLuint -> GLRaw.GLsizei -> Ptr GLRaw.GLsizei -> Ptr GLRaw.GLint -> Ptr GLRaw.GLenum -> Ptr GLRaw.GLchar -> IO ())
    -> GL.GLuint
    -> IO [a]
getActivePorts construct pname f program = do
    len  <- alloca $ \p -> GLRaw.glGetProgramiv program pname p >> peek p
    mapM g [0..(len - 1)]

    where
    maxNameBytes = 64
    g i =
        alloca $ \lp ->
        alloca $ \sp ->
        alloca $ \tp ->
        allocaArray maxNameBytes $ \np -> do
            f program (fromIntegral i) (fromIntegral maxNameBytes) lp sp tp np
            length <- peek lp
            size <- peek sp
            dataType <- peek tp
            name <- ByteString.pack <$> peekCStringLen (np, fromIntegral length)
            return $ construct name (fromIntegral i) (fromIntegral size) dataType
