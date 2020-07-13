{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Graphics.Hree.Program
    ( ProgramOption_(..)
    , ProgramOption
    , PartialProgramOption
    , EmbeddedProgramType(..)
    , ProgramName
    , ProgramSpec(..)
    , ShaderSource(..)
    , applyPartialProgramOption
    , basicProgramSpec
    , defaultProgramOption
    , flatColorProgramSpec
    , getProgramName
    , mkProgram
    , setGlslVersion
    , setHasEmissiveMap
    , setHasJointIndices
    , setHasJointWeights
    , setHasMetallicRoughnessMap
    , setHasNormalMap
    , setHasOcclusionMap
    , setHasVertexNormal
    , setHasVertexTangent
    , setHasVertexColor
    , setMaxJointCount
    , setMaxLightCount
    , setMaxSpriteTileCount
    , setUseVertexSkinning
    , setPartialGlslVersion
    , setPartialHasEmissiveMap
    , setPartialHasJointIndices
    , setPartialHasJointWeights
    , setPartialHasMetallicRoughnessMap
    , setPartialHasNormalMap
    , setPartialHasOcclusionMap
    , setPartialHasVertexNormal
    , setPartialHasVertexTangent
    , setPartialHasVertexColor
    , setPartialMaxJointCount
    , setPartialMaxLightCount
    , setPartialMaxSpriteTileCount
    , setPartialUseVertexSkinning
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
import Data.Coerce (coerce)
#ifdef EMBED_SHADERS
import Data.FilePreprocess (preprocessFile)
#else
import Data.FilePreprocess (preprocessFileIO)
#endif
import Data.Functor.Identity (Identity(..))
import Data.Hashable (Hashable(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Last(..), Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text (replace)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text.Builder (fromText, singleton,
                                                         toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Text.Builder (decimal)
import qualified Data.Vector as BV (Vector, generateM, map, mapM, mapMaybe,
                                    toList, zip, zipWith, (!))
import qualified Data.Vector.Storable as SV (generateM, imapMaybe, length,
                                             unsafeWith, (!))
import qualified Foreign (alloca, allocaArray, copyArray, peek, peekElemOff,
                          pokeArray, with)
import qualified Foreign.C.String as Foreign (peekCStringLen)
import GHC.Generics (Generic)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Light (maxLightCount)
#ifndef EMBED_SHADERS
import qualified Paths_hree as Paths
#endif
import System.FilePath ((</>))

data ProgramOption_ f = ProgramOption
    { programOptionGlslVersion             :: !(f (Maybe Int))
    , programOptionHasEmissiveMap          :: !(f Bool)
    , programOptionHasJointIndices         :: !(f Bool)
    , programOptionHasJointWeights         :: !(f Bool)
    , programOptionHasMetallicRoughnessMap :: !(f Bool)
    , programOptionHasNormalMap            :: !(f Bool)
    , programOptionHasOcclusionMap         :: !(f Bool)
    , programOptionHasVertexColor          :: !(f Bool)
    , programOptionHasVertexNormal         :: !(f Bool)
    , programOptionHasVertexTangent        :: !(f Bool)
    , programOptionMaxJointCount           :: !(f Int)
    , programOptionMaxLightCount           :: !(f Int)
    , programOptionMaxSpriteTileCount      :: !(f Int)
    , programOptionUseVertexSkinning       :: !(f Bool)
    } deriving (Generic)

newtype LastMaybe a = LastMaybe
    { unLastMaybe :: Last (Maybe a)
    } deriving (Show, Eq, Semigroup)
type ProgramOption = ProgramOption_ Identity
type PartialProgramOption = ProgramOption_ LastMaybe

deriving instance Eq ProgramOption
deriving instance Eq PartialProgramOption
deriving instance Show ProgramOption
deriving instance Show PartialProgramOption

instance Monoid (LastMaybe a) where
    mempty = LastMaybe (Last Nothing)

instance Hashable ProgramOption where

instance Semigroup PartialProgramOption where
    a <> b = ProgramOption
        { programOptionGlslVersion = programOptionGlslVersion a <> programOptionGlslVersion b
        , programOptionHasEmissiveMap = programOptionHasEmissiveMap a <> programOptionHasEmissiveMap b
        , programOptionHasJointIndices = programOptionHasJointIndices a <> programOptionHasJointIndices b
        , programOptionHasJointWeights = programOptionHasJointWeights a <> programOptionHasJointWeights b
        , programOptionHasMetallicRoughnessMap = programOptionHasMetallicRoughnessMap a <> programOptionHasMetallicRoughnessMap b
        , programOptionHasNormalMap = programOptionHasNormalMap a <> programOptionHasNormalMap b
        , programOptionHasOcclusionMap = programOptionHasOcclusionMap a <> programOptionHasOcclusionMap b
        , programOptionHasVertexColor = programOptionHasVertexColor a <> programOptionHasVertexColor b
        , programOptionHasVertexNormal = programOptionHasVertexNormal a <> programOptionHasVertexNormal b
        , programOptionHasVertexTangent = programOptionHasVertexTangent a <> programOptionHasVertexTangent b
        , programOptionMaxJointCount = programOptionMaxJointCount a <> programOptionMaxJointCount b
        , programOptionMaxLightCount = programOptionMaxLightCount a <> programOptionMaxLightCount b
        , programOptionMaxSpriteTileCount = programOptionMaxSpriteTileCount a <> programOptionMaxSpriteTileCount b
        , programOptionUseVertexSkinning = programOptionUseVertexSkinning a <> programOptionUseVertexSkinning b
        }

instance Monoid PartialProgramOption where
    mempty =ProgramOption
        { programOptionGlslVersion = mempty
        , programOptionHasEmissiveMap = mempty
        , programOptionHasJointIndices = mempty
        , programOptionHasJointWeights = mempty
        , programOptionHasMetallicRoughnessMap = mempty
        , programOptionHasNormalMap = mempty
        , programOptionHasOcclusionMap = mempty
        , programOptionHasVertexColor = mempty
        , programOptionHasVertexNormal = mempty
        , programOptionHasVertexTangent = mempty
        , programOptionMaxJointCount = mempty
        , programOptionMaxLightCount = mempty
        , programOptionMaxSpriteTileCount = mempty
        , programOptionUseVertexSkinning = mempty
        }

applyPartialProgramOption :: ProgramOption -> PartialProgramOption -> ProgramOption
applyPartialProgramOption a b = ProgramOption
    { programOptionGlslVersion = maybe (programOptionGlslVersion a) pure . getLast . coerce $ programOptionGlslVersion b
    , programOptionHasEmissiveMap = maybe (programOptionHasEmissiveMap a) pure . coerce $ programOptionHasEmissiveMap b
    , programOptionHasJointIndices = maybe (programOptionHasJointIndices a) pure . coerce $ programOptionHasJointIndices b
    , programOptionHasJointWeights = maybe (programOptionHasJointWeights a) pure . coerce $ programOptionHasJointWeights b
    , programOptionHasMetallicRoughnessMap = maybe (programOptionHasMetallicRoughnessMap a) pure . coerce $ programOptionHasMetallicRoughnessMap b
    , programOptionHasNormalMap = maybe (programOptionHasNormalMap a) pure . coerce $ programOptionHasNormalMap b
    , programOptionHasOcclusionMap = maybe (programOptionHasOcclusionMap a) pure . coerce $ programOptionHasOcclusionMap b
    , programOptionHasVertexColor = maybe (programOptionHasVertexColor a) pure . coerce $ programOptionHasVertexColor b
    , programOptionHasVertexNormal = maybe (programOptionHasVertexNormal a) pure . coerce $ programOptionHasVertexNormal b
    , programOptionHasVertexTangent = maybe (programOptionHasVertexTangent a) pure . coerce $ programOptionHasVertexTangent b
    , programOptionMaxJointCount = maybe (programOptionMaxJointCount a) pure . coerce $ programOptionMaxJointCount b
    , programOptionMaxLightCount = maybe (programOptionMaxLightCount a) pure . coerce $ programOptionMaxLightCount b
    , programOptionMaxSpriteTileCount = maybe (programOptionMaxSpriteTileCount a) pure . coerce $ programOptionMaxSpriteTileCount b
    , programOptionUseVertexSkinning = maybe (programOptionUseVertexSkinning a) pure . coerce $ programOptionUseVertexSkinning b
    }

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
    EmbeddedProgram !EmbeddedProgramType |
    UserProgram
        !ShaderSource -- vertexShaderSource
        !ShaderSource -- fragmentShaderSource
        !(BV.Vector (ByteString, BufferBindingIndex)) -- binding points
    deriving (Show, Eq)

newtype ProgramName = ProgramName
    { unProgramName :: Text
    } deriving (Show, Eq, Ord, Hashable)

defaultProgramOption :: ProgramOption
defaultProgramOption = ProgramOption
    { programOptionGlslVersion = pure $ Just 450
    , programOptionHasEmissiveMap = pure False
    , programOptionHasJointIndices = pure False
    , programOptionHasJointWeights = pure False
    , programOptionHasMetallicRoughnessMap = pure False
    , programOptionHasNormalMap = pure False
    , programOptionHasOcclusionMap = pure False
    , programOptionHasVertexNormal = pure False
    , programOptionHasVertexTangent = pure False
    , programOptionHasVertexColor = pure False
    , programOptionMaxJointCount = pure 128
    , programOptionMaxLightCount = pure maxLightCount
    , programOptionMaxSpriteTileCount = pure 0
    , programOptionUseVertexSkinning = pure False
    }

setGlslVersion :: ProgramOption -> Maybe Int -> ProgramOption
setGlslVersion programOption a = programOption { programOptionGlslVersion = pure a }

setHasEmissiveMap :: ProgramOption -> Bool -> ProgramOption
setHasEmissiveMap programOption a = programOption { programOptionHasEmissiveMap = pure a }

setHasJointIndices :: ProgramOption -> Bool -> ProgramOption
setHasJointIndices programOption a = programOption { programOptionHasJointIndices = pure a }

setHasJointWeights :: ProgramOption -> Bool -> ProgramOption
setHasJointWeights programOption a = programOption { programOptionHasJointWeights = pure a }

setHasMetallicRoughnessMap :: ProgramOption -> Bool -> ProgramOption
setHasMetallicRoughnessMap programOption a = programOption { programOptionHasMetallicRoughnessMap = pure a }

setHasNormalMap :: ProgramOption -> Bool -> ProgramOption
setHasNormalMap programOption a = programOption { programOptionHasNormalMap = pure a }

setHasOcclusionMap :: ProgramOption -> Bool -> ProgramOption
setHasOcclusionMap programOption a = programOption { programOptionHasOcclusionMap = pure a }

setHasVertexNormal :: ProgramOption -> Bool -> ProgramOption
setHasVertexNormal programOption a = programOption { programOptionHasVertexNormal = pure a }

setHasVertexTangent :: ProgramOption -> Bool -> ProgramOption
setHasVertexTangent programOption a = programOption { programOptionHasVertexTangent = pure a }

setHasVertexColor :: ProgramOption -> Bool -> ProgramOption
setHasVertexColor programOption a = programOption { programOptionHasVertexColor = pure a }

setMaxJointCount :: ProgramOption -> Int -> ProgramOption
setMaxJointCount programOption a = programOption { programOptionMaxJointCount = pure a }

setMaxLightCount :: ProgramOption -> Int -> ProgramOption
setMaxLightCount programOption a = programOption { programOptionMaxLightCount = pure a }

setMaxSpriteTileCount :: ProgramOption -> Int -> ProgramOption
setMaxSpriteTileCount programOption a = programOption { programOptionMaxSpriteTileCount = pure a }

setUseVertexSkinning :: ProgramOption -> Bool -> ProgramOption
setUseVertexSkinning programOption a = programOption { programOptionUseVertexSkinning = pure a }

setPartialGlslVersion :: PartialProgramOption -> Maybe (Maybe Int) -> PartialProgramOption
setPartialGlslVersion programOption a = programOption { programOptionGlslVersion = coerce a }

setPartialHasEmissiveMap :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasEmissiveMap programOption a = programOption { programOptionHasEmissiveMap = coerce a }

setPartialHasJointIndices :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasJointIndices programOption a = programOption { programOptionHasJointIndices = coerce a }

setPartialHasJointWeights :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasJointWeights programOption a = programOption { programOptionHasJointWeights = coerce a }

setPartialHasMetallicRoughnessMap :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasMetallicRoughnessMap programOption a = programOption { programOptionHasMetallicRoughnessMap = coerce a }

setPartialHasNormalMap :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasNormalMap programOption a = programOption { programOptionHasNormalMap = coerce a }

setPartialHasOcclusionMap :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasOcclusionMap programOption a = programOption { programOptionHasOcclusionMap = coerce a }

setPartialHasVertexNormal :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasVertexNormal programOption a = programOption { programOptionHasVertexNormal = coerce a }

setPartialHasVertexTangent :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasVertexTangent programOption a = programOption { programOptionHasVertexTangent = coerce a }

setPartialHasVertexColor :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialHasVertexColor programOption a = programOption { programOptionHasVertexColor = coerce a }

setPartialMaxJointCount :: PartialProgramOption -> Maybe Int -> PartialProgramOption
setPartialMaxJointCount programOption a = programOption { programOptionMaxJointCount = coerce a }

setPartialMaxLightCount :: PartialProgramOption -> Maybe Int -> PartialProgramOption
setPartialMaxLightCount programOption a = programOption { programOptionMaxLightCount = coerce a }

setPartialMaxSpriteTileCount :: PartialProgramOption -> Maybe Int -> PartialProgramOption
setPartialMaxSpriteTileCount programOption a = programOption { programOptionMaxSpriteTileCount = coerce a }

setPartialUseVertexSkinning :: PartialProgramOption -> Maybe Bool -> PartialProgramOption
setPartialUseVertexSkinning programOption a = programOption { programOptionUseVertexSkinning = coerce a }

basicProgramSpec :: ProgramSpec
basicProgramSpec = EmbeddedProgram BasicProgram

flatColorProgramSpec :: ProgramSpec
flatColorProgramSpec = EmbeddedProgram FlatColorProgram

spriteProgramSpec :: ProgramSpec
spriteProgramSpec = EmbeddedProgram SpriteProgram

standardProgramSpec :: ProgramSpec
standardProgramSpec = EmbeddedProgram StandardProgram

testProgramSpec :: ProgramSpec
testProgramSpec = EmbeddedProgram TestProgram

getProgramName :: ProgramSpec -> ProgramOption -> ProgramName
getProgramName (EmbeddedProgram progType) programOption = ProgramName . Text.toStrict . Text.Builder.toLazyText $
    Text.Builder.fromText "embedded:" `mappend`
    Text.Builder.fromText (embeddedProgramName progType) `mappend`
    Text.Builder.singleton ':' `mappend`
    (Text.Builder.decimal . hash $ programOption)
getProgramName (UserProgram (ShaderSource vname _) (ShaderSource fname _) _) _ = ProgramName . Text.toStrict . Text.Builder.toLazyText $
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

embeddedProgramBindingPoints :: EmbeddedProgramType -> BV.Vector (ByteString, BufferBindingIndex)
embeddedProgramBindingPoints _ = mempty

mkProgram :: ProgramSpec -> ProgramOption -> IO ProgramInfo
mkProgram (EmbeddedProgram progType) programOption = do
    let header = renderProgramOption programOption
        bindingPoints = embeddedProgramBindingPoints progType
    vsource <- mappend header <$> loadEmbeddedVertexShaderSource progType
    fsource <- mappend header <$> loadEmbeddedFragmentShaderSource progType
    mkProgram' vsource fsource bindingPoints
mkProgram (UserProgram (ShaderSource _ vsource) (ShaderSource _ fsource) bindingPoints) _ =
    mkProgram' vsource fsource bindingPoints

mkProgram' :: ByteString -> ByteString -> BV.Vector (ByteString, BufferBindingIndex) -> IO ProgramInfo
mkProgram' vsource fsource bindingPoints = do
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

    uniformBlocks <- getActiveUniformBlockInfos program
    let uniformBlockMap = Map.fromList . BV.toList $ BV.zip (BV.map ubiUniformBlockName uniformBlocks) uniformBlocks

    return $ ProgramInfo program attribMap uniformMap uniformLocationMap uniformBlockMap bindingPoints

loadEmbeddedVertexShaderSource, loadEmbeddedFragmentShaderSource :: EmbeddedProgramType -> IO ByteString
#ifdef EMBED_SHADERS
loadEmbeddedVertexShaderSource BasicProgram = return $(preprocessFile "shader/basic-vertex.glsl" "shader/lib")
loadEmbeddedVertexShaderSource FlatColorProgram = return $(preprocessFile "shader/flat-color-vertex.glsl" "shader/lib")
loadEmbeddedVertexShaderSource SpriteProgram = return $(preprocessFile "shader/sprite-vertex.glsl" "shader/lib")
loadEmbeddedVertexShaderSource StandardProgram = return $(preprocessFile "shader/standard-vertex.glsl" "shader/lib")
loadEmbeddedVertexShaderSource TestProgram = return $(preprocessFile "shader/test-vertex.glsl" "shader/lib")

loadEmbeddedFragmentShaderSource BasicProgram = return $(preprocessFile "shader/basic-fragment.glsl" "shader/lib")
loadEmbeddedFragmentShaderSource FlatColorProgram = return $(preprocessFile "shader/flat-color-fragment.glsl" "shader/lib")
loadEmbeddedFragmentShaderSource SpriteProgram = return $(preprocessFile "shader/sprite-fragment.glsl" "shader/lib")
loadEmbeddedFragmentShaderSource StandardProgram = return $(preprocessFile "shader/standard-fragment.glsl" "shader/lib")
loadEmbeddedFragmentShaderSource TestProgram = return $(preprocessFile "shader/test-fragment.glsl" "shader/lib")
#else
loadEmbeddedVertexShaderSource programType = loadEmbeddedShaderSource programType "vertex"
loadEmbeddedFragmentShaderSource programType = loadEmbeddedShaderSource programType "fragment"

loadEmbeddedShaderSource :: EmbeddedProgramType -> String -> IO ByteString
loadEmbeddedShaderSource programType shaderType = do
    filepath <- getShaderFileName programType shaderType
    dataDir <- Paths.getDataDir
    let includeDir = dataDir </> "shader" </> "lib"
    preprocessFileIO filepath includeDir

getShaderFileName :: EmbeddedProgramType -> String -> IO FilePath
getShaderFileName BasicProgram shaderType     = Paths.getDataFileName $ "shader/basic-" ++ shaderType ++ ".glsl"
getShaderFileName FlatColorProgram shaderType = Paths.getDataFileName $ "shader/flat-color-" ++ shaderType ++ ".glsl"
getShaderFileName SpriteProgram shaderType    = Paths.getDataFileName $ "shader/sprite-" ++ shaderType ++ ".glsl"
getShaderFileName StandardProgram shaderType  = Paths.getDataFileName $ "shader/standard-" ++ shaderType ++ ".glsl"
getShaderFileName TestProgram shaderType      = Paths.getDataFileName $ "shader/test-" ++ shaderType ++ ".glsl"
#endif

renderProgramOption :: ProgramOption -> ByteString
renderProgramOption programOption = ByteString.intercalate "\n" . catMaybes $
    [ mappend "#version " . ByteString.pack . show <$> runIdentity (programOptionGlslVersion programOption)
    , if runIdentity $ programOptionHasEmissiveMap programOption
        then Just "#define HAS_EMISSIVE_MAP"
        else Nothing
    , if runIdentity $ programOptionHasJointIndices programOption
        then Just "#define HAS_JOINT_INDICES"
        else Nothing
    , if runIdentity $ programOptionHasJointWeights programOption
        then Just "#define HAS_JOINT_WEIGHTS"
        else Nothing
    , if runIdentity $ programOptionHasMetallicRoughnessMap programOption
        then Just "#define HAS_METALLIC_ROUGHNESS_MAP"
        else Nothing
    , if runIdentity $ programOptionHasNormalMap programOption
        then Just "#define HAS_NORMAL_MAP"
        else Nothing
    , if runIdentity $ programOptionHasOcclusionMap programOption
        then Just "#define HAS_OCCLUSION_MAP"
        else Nothing
    , if runIdentity $ programOptionHasVertexNormal programOption
        then Just "#define HAS_VERTEX_NORMAL"
        else Nothing
    , if runIdentity $ programOptionHasVertexTangent programOption
        then Just "#define HAS_VERTEX_TANGENT"
        else Nothing
    , if runIdentity $ programOptionHasVertexColor programOption
        then Just "#define HAS_VERTEX_COLOR"
        else Nothing
    , Just $ "#define MAX_JOINT_COUNT " `mappend` (ByteString.pack . show . runIdentity . programOptionMaxJointCount $ programOption)
    , Just $ "#define MAX_LIGHT_COUNT " `mappend` (ByteString.pack . show . runIdentity . programOptionMaxLightCount $ programOption)
    , Just $ "#define MAX_SPRITE_TILE_COUNT " `mappend` (ByteString.pack . show . runIdentity . programOptionMaxSpriteTileCount $ programOption)
    , if runIdentity $ programOptionUseVertexSkinning programOption
        then Just "#define USE_VERTEX_SKINNING"
        else Nothing
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
    Foreign.alloca $ \p -> do
        GLW.glGetProgramiv program GL.GL_ACTIVE_UNIFORMS p
        Foreign.peek p >>= getUniformInfos . fromIntegral

    where
    maxNameBytes = 128

    getUniformInfos uniformCount =
        Foreign.allocaArray uniformCount $ \indices ->
        Foreign.allocaArray uniformCount $ \params -> do
            -- get uniform block indices
            Foreign.pokeArray indices [0..(fromIntegral uniformCount - 1)]
            GLW.glGetActiveUniformsiv program (fromIntegral uniformCount) indices GL.GL_UNIFORM_BLOCK_INDEX params
            blockIndices <- SV.generateM uniformCount (Foreign.peekElemOff params)

            -- get uniform informations (other than inside uniform blocks)
            let targetIndices = SV.imapMaybe (\i a -> if a < 0 then Just (fromIntegral i :: GL.GLuint) else Nothing) blockIndices
                targetCount = SV.length targetIndices
            uniformInfos <-
                Foreign.alloca $ \lp ->
                Foreign.alloca $ \sp ->
                Foreign.alloca $ \tp ->
                Foreign.allocaArray maxNameBytes $ \np ->
                BV.generateM targetCount $ \i -> do
                    GLW.glGetActiveUniform program (targetIndices SV.! i) (fromIntegral maxNameBytes) lp sp tp np
                    nameLen <- Foreign.peek lp
                    size <- Foreign.peek sp
                    dataType <- Foreign.peek tp
                    name <- ByteString.packCStringLen (np, fromIntegral nameLen)
                    return $ UniformInfo name dataType (fromIntegral size) (-1) (-1) (-1) False

            SV.unsafeWith targetIndices $ \source -> Foreign.copyArray source indices targetCount

            GLW.glGetActiveUniformsiv program (fromIntegral targetCount) indices GL.GL_UNIFORM_OFFSET params
            offsets <- SV.generateM targetCount (Foreign.peekElemOff params)

            GLW.glGetActiveUniformsiv program (fromIntegral targetCount) indices GL.GL_UNIFORM_ARRAY_STRIDE params
            astrides <- SV.generateM targetCount (Foreign.peekElemOff params)

            GLW.glGetActiveUniformsiv program (fromIntegral targetCount) indices GL.GL_UNIFORM_MATRIX_STRIDE params
            mstrides <- SV.generateM targetCount (Foreign.peekElemOff params)

            GLW.glGetActiveUniformsiv program (fromIntegral targetCount) indices GL.GL_UNIFORM_IS_ROW_MAJOR params
            isRowMajors <- SV.generateM targetCount (Foreign.peekElemOff params)

            BV.generateM targetCount $ \i -> do
                let uinfo = uniformInfos BV.! i
                    uinfo' = uinfo
                        { uiUniformOffset = offsets SV.! i
                        , uiUniformArrayStride = astrides SV.! i
                        , uiUniformMatrixStride = mstrides SV.! i
                        , uiUniformIsRowMajor = (isRowMajors SV.! i) /= 0
                        }
                return uinfo'

getUniformLocation :: GLW.Program -> ByteString -> IO (Maybe GLW.UniformLocation)
getUniformLocation program =
    flip ByteString.useAsCString $ GLW.glGetUniformLocation program

getActiveUniformBlockInfos :: GLW.Program -> IO (BV.Vector UniformBlockInfo)
getActiveUniformBlockInfos program =
    Foreign.alloca $ \p ->
    Foreign.alloca $ \lp ->
    Foreign.allocaArray maxNameBytes $ \np -> do
        GLW.glGetProgramiv program GL.GL_ACTIVE_UNIFORM_BLOCKS p
        len <- Foreign.peek p
        BV.generateM (fromIntegral len) (getUniformBlockInfo lp np)

    where
    maxNameBytes = 128

    getUniformBlockInfo lp np index = do
        GLW.glGetActiveUniformBlockName program (fromIntegral index) (fromIntegral maxNameBytes) lp np
        nameLen <- Foreign.peek lp
        name <- ByteString.packCStringLen (np, fromIntegral nameLen)
        return $ UniformBlockInfo name (fromIntegral index)

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
