{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Graphics.Hree.Program
    ( Options(..)
    , ShaderSource(..)
    , ProgramName
    , ProgramSpec(..)
    , basicProgramSpec
    , flatColorProgramSpec
    , getProgramName
    , spriteProgramSpec
    , standardProgramSpec
    , testProgramSpec
    , unProgramName
    , mkProgram
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (intercalate)
import qualified Data.ByteString.Char8 as ByteString (pack, useAsCStringLen)
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text (replace)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text.Builder (fromText, singleton,
                                                         toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Text.Builder (decimal)
import Foreign (Ptr)
import qualified Foreign (alloca, allocaArray, peek, with)
import qualified Foreign.C.String as Foreign (peekCStringLen)
import GHC.Generics (Generic)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import System.IO.Error (userError)

data EmbeddedProgramType =
    BasicProgram |
    FlatColorProgram |
    SpriteProgram |
    StandardProgram |
    TestProgram
    deriving (Show, Eq, Enum, Generic)

data Options = Options
    { optionsGlslVersion    :: !(Maybe Int)
    , optionsHasNormalMap   :: !Bool
    , optionsHasVertexColor :: !Bool
    , optionsMaxLightCount  :: !Int
    } deriving (Show, Eq, Generic)

data ShaderSource = ShaderSource
    { shaderSourceName :: !Text
    , shaderSourceBody :: !ByteString
    } deriving (Show, Eq)

data ProgramSpec =
    EmbeddedProgram
    { embeddedProgramType    :: !EmbeddedProgramType
    , embeddedProgramOptions :: !Options
    } |
    UserProgram
    { vertexShaderSource   :: !ShaderSource
    , fragmentShaderSource :: !ShaderSource
    } deriving (Show, Eq)

newtype ProgramName = ProgramName
    { unProgramName :: Text
    } deriving (Show, Eq, Ord, Hashable)

defaultOptions :: Options
defaultOptions = Options
    { optionsGlslVersion = Just 450
    , optionsHasNormalMap = False
    , optionsHasVertexColor = True
    , optionsMaxLightCount = 10
    }

basicProgramSpec :: ProgramSpec
basicProgramSpec = EmbeddedProgram BasicProgram defaultOptions

flatColorProgramSpec :: ProgramSpec
flatColorProgramSpec = EmbeddedProgram FlatColorProgram defaultOptions

spriteProgramSpec :: ProgramSpec
spriteProgramSpec = EmbeddedProgram SpriteProgram defaultOptions

standardProgramSpec :: ProgramSpec
standardProgramSpec = EmbeddedProgram StandardProgram defaultOptions

testProgramSpec :: ProgramSpec
testProgramSpec = EmbeddedProgram TestProgram options
    where
    options = defaultOptions { optionsGlslVersion = Nothing }

instance Hashable Options where

getProgramName :: ProgramSpec -> ProgramName
getProgramName (EmbeddedProgram progType options) = ProgramName . Text.toStrict . Text.Builder.toLazyText $
    Text.Builder.fromText "embedded:" `mappend`
    Text.Builder.fromText (embeddedProgramName progType) `mappend`
    Text.Builder.singleton ':' `mappend`
    (Text.Builder.decimal . hash $ options)
getProgramName (UserProgram (ShaderSource vname _) (ShaderSource fname _)) = ProgramName . Text.toStrict . Text.Builder.toLazyText $
    Text.Builder.fromText "user:" `mappend`
    Text.Builder.fromText vname' `mappend`
    Text.Builder.singleton ':' `mappend`
    Text.Builder.fromText fname'
    where
    vname' = Text.replace ":" "::" vname
    fname' = Text.replace ":" "::" fname

embeddedProgramName :: EmbeddedProgramType -> Text
embeddedProgramName BasicProgram     = "basic"
embeddedProgramName FlatColorProgram = "flatcolor"
embeddedProgramName SpriteProgram    = "sprite"
embeddedProgramName StandardProgram  = "standard"
embeddedProgramName TestProgram      = "test"

mkProgram :: ProgramSpec -> IO ProgramInfo
mkProgram (EmbeddedProgram progType options) = do
    let header = renderOptions options
        vsource = header `mappend` getEmbeddedVertexShaderSource progType
        fsource = header `mappend` getEmbeddedFragmentShaderSource progType
    mkProgram' vsource fsource
mkProgram (UserProgram (ShaderSource _ vsource) (ShaderSource _ fsource)) =
    mkProgram' vsource fsource

mkProgram' :: ByteString -> ByteString -> IO ProgramInfo
mkProgram' vsource fsource = do
    vshader <- mkShader (Proxy :: Proxy 'GLW.GL_VERTEX_SHADER) vsource
    fshader <- mkShader (Proxy :: Proxy 'GLW.GL_FRAGMENT_SHADER) fsource
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

getEmbeddedVertexShaderSource :: EmbeddedProgramType -> ByteString
getEmbeddedVertexShaderSource BasicProgram = $(embedFile "shader/basic-vertex.glsl")
getEmbeddedVertexShaderSource FlatColorProgram = $(embedFile "shader/flat-color-vertex.glsl")
getEmbeddedVertexShaderSource SpriteProgram = $(embedFile "shader/sprite-vertex.glsl")
getEmbeddedVertexShaderSource StandardProgram = $(embedFile "shader/standard-vertex.glsl")
getEmbeddedVertexShaderSource TestProgram = $(embedFile "shader/test-vertex.glsl")

getEmbeddedFragmentShaderSource :: EmbeddedProgramType -> ByteString
getEmbeddedFragmentShaderSource BasicProgram = $(embedFile "shader/basic-fragment.glsl")
getEmbeddedFragmentShaderSource FlatColorProgram = $(embedFile "shader/flat-color-fragment.glsl")
getEmbeddedFragmentShaderSource SpriteProgram = $(embedFile "shader/sprite-fragment.glsl")
getEmbeddedFragmentShaderSource StandardProgram = $(embedFile "shader/standard-fragment.glsl")
getEmbeddedFragmentShaderSource TestProgram = $(embedFile "shader/test-fragment.glsl")

renderOptions :: Options -> ByteString
renderOptions options = ByteString.intercalate "\n" . catMaybes $
    [ mappend "#version " . ByteString.pack . show <$> optionsGlslVersion options
    , if optionsHasNormalMap options
        then Just "#define HAS_NORMAL_MAP"
        else Nothing
    , if optionsHasVertexColor options
        then Just "#define HAS_VERTEX_COLOR"
        else Nothing
    , Just $ "#define MAX_LIGHT_COUNT " `mappend` (ByteString.pack . show . optionsMaxLightCount $ options)
    , Just ""
    ]

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
getActiveAttribs = getActivePorts AttribInfo GLW.glGetAttribLocation GL.GL_ACTIVE_ATTRIBUTES GLW.glGetActiveAttrib

getActiveUniforms :: GLW.Program -> IO [UniformInfo]
getActiveUniforms = getActivePorts UniformInfo GLW.glGetUniformLocation GL.GL_ACTIVE_UNIFORMS GLW.glGetActiveUniform

getActivePorts ::
    (ByteString -> location -> GL.GLuint -> GL.GLuint -> a)
    -> (GLW.Program -> Ptr GL.GLchar -> IO (Maybe location))
    -> GL.GLenum
    -> (GLW.Program -> GL.GLuint -> GL.GLsizei -> Ptr GL.GLsizei -> Ptr GL.GLint -> Ptr GL.GLenum -> Ptr GL.GLchar -> IO ())
    -> GLW.Program
    -> IO [a]
getActivePorts construct getLocation pname f program = do
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
            len <- Foreign.peek lp
            size <- Foreign.peek sp
            dataType <- Foreign.peek tp
            name <- ByteString.pack <$> Foreign.peekCStringLen (np, fromIntegral len)
            l <- fromMaybe (error "getLocation failed") <$> getLocation program np
            return $ construct name l (fromIntegral size) dataType

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
            GLW.glGetProgramInfoLog program (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log' <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log'
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
            GLW.glGetShaderInfoLog shader (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log' <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log'
    where
    bufSize = 256
