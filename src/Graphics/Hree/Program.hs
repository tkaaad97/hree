{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Graphics.Hree.Program
    ( VertexShaderSpec(..)
    , FragmentShaderSpec(..)
    , ProgramSpec(..)
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable(..))
import qualified Graphics.Rendering.OpenGL as GL
import System.IO.Error (userError)

data VertexShaderSpec = VertexShaderSpec
    deriving (Show, Eq)

data FragmentShaderSpec = FragmentShaderSpec
    deriving (Show, Eq)

data ProgramSpec = ProgramSpec
    { vertexShaderSpec   :: !VertexShaderSpec
    , fragmentShaderSpec :: !FragmentShaderSpec
    } deriving (Show, Eq)

instance Hashable VertexShaderSpec where
    hash _ = 0
    hashWithSalt = const

instance Hashable FragmentShaderSpec where
    hash _ = 0
    hashWithSalt = const

instance Hashable ProgramSpec where
    hashWithSalt s (ProgramSpec a b) = s `hashWithSalt` a `hashWithSalt` b

mkProgram :: ProgramSpec -> IO GL.Program
mkProgram (ProgramSpec vspec fspec) = do
    vshader <- mkVertexShader vspec
    fshader <- mkFragmentShader fspec
    program <- GL.createProgram
    mapM_ (GL.attachShader program) [vshader, fshader]
    GL.linkProgram program
    checkStatus GL.linkStatus GL.programInfoLog "program link error" program
    return program

mkVertexShader :: VertexShaderSpec -> IO GL.Shader
mkVertexShader _ = mkShader "vertexShader" GL.VertexShader shaderSource
    where
    shaderSource = $(embedFile "shader/basic-vertex.glsl")

mkFragmentShader :: FragmentShaderSpec -> IO GL.Shader
mkFragmentShader _ = mkShader "vertexShader" GL.VertexShader shaderSource
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
