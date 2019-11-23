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
    , ProgramName
    , ProgramSpec(..)
    , ShaderSource(..)
    , basicProgramSpec
    , defaultOptions
    , flatColorProgramSpec
    , getProgramName
    , mkProgram
    , setGlslVersion
    , setHasMetallicRoughnessMap
    , setHasNormalMap
    , setHasVertexNormal
    , setHasVertexTangent
    , setHasVertexColor
    , setMaxLightCount
    , spriteProgramSpec
    , standardProgramSpec
    , testProgramSpec
    , unProgramName
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (intercalate)
import qualified Data.ByteString.Char8 as ByteString (pack, packCStringLen,
                                                      useAsCString,
                                                      useAsCStringLen)
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text (replace)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text.Builder (fromText, singleton,
                                                         toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Text.Builder (decimal)
import qualified Data.Vector as BV (Vector, fromList, generateM, map, mapM,
                                    mapMaybe, toList, zip, zipWith, (!))
import qualified Foreign (alloca, allocaArray, peek, peekArray, poke, with)
import qualified Foreign.C.String as Foreign (peekCStringLen)
import GHC.Generics (Generic)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import System.IO.Error (userError)

data Options = Options
    { optionsGlslVersion             :: !(Maybe Int)
    , optionsHasNormalMap            :: !Bool
    , optionsHasMetallicRoughnessMap :: !Bool
    , optionsHasVertexNormal         :: !Bool
    , optionsHasVertexTangent        :: !Bool
    , optionsHasVertexColor          :: !Bool
    , optionsMaxLightCount           :: !Int
    } deriving (Show, Eq, Generic)

data EmbeddedProgramType =
    BasicProgram |
    FlatColorProgram |
    SpriteProgram |
    StandardProgram |
    TestProgram
    deriving (Show, Eq, Enum, Generic)

data ShaderSource = ShaderSource
    { shaderSourceName :: !Text
    , shaderSourceBody :: !ByteString
    } deriving (Show, Eq)

data ProgramSpec =
    EmbeddedProgram
        !EmbeddedProgramType
        !Options
    |
    UserProgram
        !ShaderSource -- vertexShaderSource
        !ShaderSource -- fragmentShaderSource
    deriving (Show, Eq)

newtype ProgramName = ProgramName
    { unProgramName :: Text
    } deriving (Show, Eq, Ord, Hashable)

defaultOptions :: Options
defaultOptions = Options
    { optionsGlslVersion = Just 450
    , optionsHasNormalMap = False
    , optionsHasMetallicRoughnessMap = False
    , optionsHasVertexNormal = False
    , optionsHasVertexTangent = False
    , optionsHasVertexColor = False
    , optionsMaxLightCount = 10
    }

setGlslVersion :: Options -> Maybe Int -> Options
setGlslVersion options a = options { optionsGlslVersion = a }

setHasNormalMap :: Options -> Bool -> Options
setHasNormalMap options a = options { optionsHasNormalMap = a }

setHasMetallicRoughnessMap :: Options -> Bool -> Options
setHasMetallicRoughnessMap options a = options { optionsHasMetallicRoughnessMap = a }

setHasVertexNormal :: Options -> Bool -> Options
setHasVertexNormal options a = options { optionsHasVertexNormal = a }

setHasVertexTangent :: Options -> Bool -> Options
setHasVertexTangent options a = options { optionsHasVertexTangent = a }

setHasVertexColor :: Options -> Bool -> Options
setHasVertexColor options a = options { optionsHasVertexColor = a }

setMaxLightCount :: Options -> Int -> Options
setMaxLightCount options a = options { optionsMaxLightCount = a }

basicProgramSpec :: Options -> ProgramSpec
basicProgramSpec = EmbeddedProgram BasicProgram

flatColorProgramSpec :: Options -> ProgramSpec
flatColorProgramSpec = EmbeddedProgram FlatColorProgram

spriteProgramSpec :: Options -> ProgramSpec
spriteProgramSpec = EmbeddedProgram SpriteProgram

standardProgramSpec :: Options -> ProgramSpec
standardProgramSpec = EmbeddedProgram StandardProgram

testProgramSpec :: Options -> ProgramSpec
testProgramSpec options = EmbeddedProgram TestProgram options { optionsGlslVersion = Nothing }

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
    uniforms <- getActiveUniformInfos program

    let uniformNames = BV.map uiUniformName uniforms
        attribMap = Map.fromList . BV.toList $ BV.zip (BV.map aiAttribName attribs) attribs
        uniformMap = Map.fromList . BV.toList $ BV.zip uniformNames uniforms

    uniformLocations <- BV.mapM (getUniformLocation program) uniformNames
    let uniformLocationMap = Map.fromList . BV.toList . BV.mapMaybe id $ BV.zipWith (fmap . (,)) uniformNames uniformLocations

    uniformBlocks <- getActiveUniformBlockInfos program uniforms
    let uniformBlockMap = Map.fromList . BV.toList $ BV.zip (BV.map ubiUniformBlockName uniformBlocks) uniformBlocks

    return $ ProgramInfo program attribMap uniformMap uniformLocationMap uniformBlockMap

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
    , if optionsHasMetallicRoughnessMap options
        then Just "#define HAS_METALLIC_ROUGHNESS_MAP"
        else Nothing
    , if optionsHasVertexNormal options
        then Just "#define HAS_VERTEX_NORMAL"
        else Nothing
    , if optionsHasVertexTangent options
        then Just "#define HAS_VERTEX_TANGENT"
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

getActiveAttribs :: GLW.Program -> IO (BV.Vector AttribInfo)
getActiveAttribs program =
    Foreign.alloca $ \p ->
    Foreign.alloca $ \lp ->
    Foreign.alloca $ \sp ->
    Foreign.alloca $ \tp ->
    Foreign.allocaArray maxNameBytes $ \np -> do
        GLW.glGetProgramiv program GL.GL_ACTIVE_ATTRIBUTES p
        len <- Foreign.peek p
        BV.mapMaybe id <$> BV.generateM (fromIntegral len) (getActiveAttrib lp sp tp np)

    where
    maxNameBytes = 128
    getActiveAttrib lp sp tp np index = do
        GLW.glGetActiveAttrib program (fromIntegral index) (fromIntegral maxNameBytes) lp sp tp np
        len <- Foreign.peek lp
        size <- Foreign.peek sp
        dataType <- Foreign.peek tp
        name <- ByteString.packCStringLen (np, fromIntegral len)
        maybeLocation <- GLW.glGetAttribLocation program np
        return $ maybeLocation >>= \l -> return $ AttribInfo name l (fromIntegral size) dataType

getActiveUniformInfos :: GLW.Program -> IO (BV.Vector UniformInfo)
getActiveUniformInfos program =
    Foreign.alloca $ \p ->
    Foreign.alloca $ \lp ->
    Foreign.alloca $ \sp ->
    Foreign.alloca $ \tp ->
    Foreign.allocaArray maxNameBytes $ \np ->
    Foreign.alloca $ \ip ->
    Foreign.alloca $ \op -> do
        GLW.glGetProgramiv program GL.GL_ACTIVE_UNIFORMS p
        len <- Foreign.peek p
        BV.generateM (fromIntegral len) (getUniformInfo lp sp tp np ip op)

    where
    maxNameBytes = 128

    getUniformInfo lp sp tp np ip op index = do
        GLW.glGetActiveUniform program (fromIntegral index) (fromIntegral maxNameBytes) lp sp tp np
        nameLen <- Foreign.peek lp
        size <- Foreign.peek sp
        dataType <- Foreign.peek tp
        name <- ByteString.packCStringLen (np, fromIntegral nameLen)

        Foreign.poke ip (fromIntegral index)
        GLW.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_OFFSET op
        offset <- Foreign.peek op

        GLW.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_ARRAY_STRIDE op
        astride <- Foreign.peek op

        GLW.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_MATRIX_STRIDE op
        mstride <- Foreign.peek op

        GLW.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_IS_ROW_MAJOR op
        isRowMajor <- Foreign.peek op

        let uniformInfo = UniformInfo name dataType (fromIntegral size) offset astride mstride (isRowMajor /= 0)

        return uniformInfo

getUniformLocation :: GLW.Program -> ByteString -> IO (Maybe GLW.UniformLocation)
getUniformLocation program =
    flip ByteString.useAsCString $ GLW.glGetUniformLocation program

getActiveUniformBlockInfos :: GLW.Program -> BV.Vector UniformInfo -> IO (BV.Vector UniformBlockInfo)
getActiveUniformBlockInfos program uniformInfos =
    Foreign.alloca $ \p ->
    Foreign.alloca $ \lp ->
    Foreign.allocaArray maxNameBytes $ \np ->
    Foreign.alloca $ \op -> do
        GLW.glGetProgramiv program GL.GL_ACTIVE_UNIFORM_BLOCKS p
        len <- Foreign.peek p
        BV.generateM (fromIntegral len) (getUniformBlockInfo lp np op)

    where
    maxNameBytes = 128

    getUniformBlockInfo lp np op index = do
        GLW.glGetActiveUniformBlockName program (fromIntegral index) (fromIntegral maxNameBytes) lp np
        nameLen <- Foreign.peek lp
        name <- ByteString.packCStringLen (np, fromIntegral nameLen)

        GLW.glGetActiveUniformBlockiv program (fromIntegral index) GL.GL_UNIFORM_BLOCK_DATA_SIZE op
        dataSize <- Foreign.peek op

        GLW.glGetActiveUniformBlockiv program (fromIntegral index) GL.GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS op
        num <- Foreign.peek op

        indices <- Foreign.allocaArray (fromIntegral num) $ \ids -> do
            GLW.glGetActiveUniformBlockiv program (fromIntegral index) GL.GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES ids
            BV.fromList <$> Foreign.peekArray (fromIntegral num) ids

        let uniforms = BV.map ((uniformInfos BV.!) . fromIntegral) indices
            uniformBlockInfo = UniformBlockInfo name (fromIntegral index) dataSize uniforms

        return uniformBlockInfo



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
