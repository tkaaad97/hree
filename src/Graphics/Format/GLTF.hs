{-# LANGUAGE OverloadedStrings #-}
module Graphics.Format.GLTF
    ( Buffer(..)
    , BufferView(..)
    , Accessor(..)
    , Mesh(..)
    , Primitive(..)
    , Node(..)
    , Scene(..)
    , GLTF(..)
    , loadGLTFFile
    ) where

import Control.Exception (throwIO)
import qualified Data.Aeson as Aeson (FromJSON(..), eitherDecodeFileStrict',
                                      withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (readFile, stripPrefix, take)
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.ByteString.Char8 as ByteString (break, drop, dropWhile,
                                                      unpack)
import Data.Either (either)
import Data.Map.Strict (Map)
import Data.Maybe (maybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, empty, mapM)
import qualified Data.Vector as UV (Vector, length, (!))
import qualified GLW (Buffer)
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL.Types as Hree (BufferSource(..))
import Graphics.Hree.Math
import qualified Graphics.Hree.Scene as Hree (Scene, addBuffer)
import qualified Linear (Quaternion(..), V3(..), V4(..))
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data Buffer = Buffer
    { bufferByteLength :: !Int
    , bufferUri        :: !Text
    } deriving (Show, Eq)

data BufferView = BufferView
    { bufferViewBuffer     :: !Int
    , bufferViewByteLength :: !Int
    , bufferViewByteOffset :: !Int
    , bufferViewTarget     :: !(Maybe Int)
    , bufferViewName       :: !(Maybe Text)
    } deriving (Show, Eq)

data Accessor = Accessor
    { accessorBufferView    :: !Int
    , accessorByteOffset    :: !Int
    , accessorComponentType :: !Int
    , accessorNormalized    :: !Bool
    , accessorCount         :: !Int
    , accessorType          :: !Text
    , accessorName          :: !(Maybe Text)
    } deriving (Show, Eq)

data Mesh = Mesh
    { meshPrimitives :: !(BV.Vector Primitive)
    , meshName       :: !(Maybe Text)
    } deriving (Show, Eq)

data Primitive = Primitive
    { primitiveAttributes :: !(Map Text Int)
    , primitiveIndices    :: !(Maybe Int)
    , primitiveMaterial   :: !(Maybe Int)
    , primitiveMode       :: !(Maybe Int)
    } deriving (Show, Eq)

data Scene = Scene
    { sceneNodes :: !(BV.Vector Int)
    , sceneName  :: !(Maybe Text)
    } deriving (Show, Eq)

data Node = Node
    { nodeCamera      :: !(Maybe Int)
    , nodeChildren    :: !(BV.Vector Int)
    , nodeSkin        :: !(Maybe Int)
    , nodeMatrix      :: !(Maybe Mat4)
    , nodeMesh        :: !(Maybe Int)
    , nodeRotation    :: !(Maybe Quaternion)
    , nodeScale       :: !(Maybe Vec3)
    , nodeTranslation :: !(Maybe Vec3)
    , nodeName        :: !(Maybe Text)
    } deriving (Show, Eq)

data GLTF = GLTF
    { gltfScenes      :: !(BV.Vector Scene)
    , gltfNodes       :: !(BV.Vector Node)
    , gltfMeshes      :: !(BV.Vector Mesh)
    , gltfBuffers     :: !(BV.Vector Buffer)
    , gltfBufferViews :: !(BV.Vector BufferView)
    , gltfAccessors   :: !(BV.Vector Accessor)
    } deriving (Show, Eq)

instance Aeson.FromJSON Buffer where
    parseJSON = Aeson.withObject "Buffer" $ \v -> do
        len <- v Aeson..: "byteLength"
        uri <- v Aeson..: "uri"
        return $ Buffer len uri

instance Aeson.FromJSON BufferView where
    parseJSON = Aeson.withObject "BufferView" $ \v -> do
        buffer <- v Aeson..: "buffer"
        offset <- v Aeson..:? "byteOffset" Aeson..!= 0
        len <- v Aeson..: "byteLength"
        target <- v Aeson..:? "target"
        name <- v Aeson..:? "name"
        return $ BufferView buffer len offset target name

instance Aeson.FromJSON Accessor where
    parseJSON = Aeson.withObject "Accessor" $ \v -> do
        bufferView <- v Aeson..: "bufferView"
        offset <- v Aeson..:? "byteOffset" Aeson..!= 0
        componentType <- v Aeson..: "componentType"
        normalized <- v Aeson..:? "normalized" Aeson..!= False
        count <- v Aeson..: "count"
        type' <- v Aeson..: "type"
        name <- v Aeson..:? "name"
        return $ Accessor bufferView offset componentType normalized count type' name

instance Aeson.FromJSON Mesh where
    parseJSON = Aeson.withObject "Mesh" $ \v -> do
        primitives <- v Aeson..: "primitives"
        name <- v Aeson..:? "name"
        return $ Mesh primitives name

instance Aeson.FromJSON Primitive where
    parseJSON = Aeson.withObject "Primitive" $ \v -> do
        attributes <- v Aeson..: "attributes"
        indices <- v Aeson..:? "indices"
        material <- v Aeson..:? "material"
        mode <- v Aeson..:? "mode"
        return $ Primitive attributes indices material mode

instance Aeson.FromJSON Scene where
    parseJSON = Aeson.withObject "Scene" $ \v -> do
        nodes <- v Aeson..:? "nodes" Aeson..!= BV.empty
        name <- v Aeson..:? "name"
        return $ Scene nodes name

instance Aeson.FromJSON Node where
    parseJSON = Aeson.withObject "Node" $ \v -> do
        camera <- v Aeson..:? "camera"
        children <- v Aeson..:? "children" Aeson..!= BV.empty
        skin <- v Aeson..:? "skin"
        matrix <- mapM fromVectorToMat4 =<< v Aeson..:? "matrix"
        mesh <- v Aeson..:? "mesh"
        rotation <- mapM fromVectorToQuaternion =<< v Aeson..:? "rotation"
        scale <- mapM fromVectorToVec3 =<< v Aeson..:? "scale"
        translation <- mapM fromVectorToVec3 =<< v Aeson..:? "translation"
        name <- v Aeson..:? "name"
        return $ Node camera children skin matrix mesh rotation scale translation name

instance Aeson.FromJSON GLTF where
    parseJSON = Aeson.withObject "GLTF" $ \v -> do
        scenes <- v Aeson..:? "scenes" Aeson..!= BV.empty
        nodes <- v Aeson..:? "nodes" Aeson..!= BV.empty
        meshes <- v Aeson..: "meshes"
        buffers <- v Aeson..: "buffers"
        bufferViews <- v Aeson..: "bufferViews"
        accessors <- v Aeson..: "accessors"
        return $ GLTF scenes nodes meshes buffers bufferViews accessors

fromVectorToVec3 :: UV.Vector Float -> Aeson.Parser Vec3
fromVectorToVec3 v
    | UV.length v == 3 =
        let a0 = v UV.! 0
            a1 = v UV.! 1
            a2 = v UV.! 2
        in return $ Linear.V3 a0 a1 a2
    | otherwise = fail "bad array size"

fromVectorToQuaternion :: UV.Vector Float -> Aeson.Parser Quaternion
fromVectorToQuaternion v
    | UV.length v == 4 =
        let a0 = v UV.! 0
            a1 = v UV.! 1
            a2 = v UV.! 2
            a3 = v UV.! 3
        in return $ Linear.Quaternion a0 (Linear.V3 a1 a2 a3)
    | otherwise = fail "bad array size"

fromVectorToMat4 :: UV.Vector Float -> Aeson.Parser Mat4
fromVectorToMat4 v
    | UV.length v == 16 =
        let a00 = v UV.! 0
            a01 = v UV.! 1
            a02 = v UV.! 2
            a03 = v UV.! 3
            a10 = v UV.! 4
            a11 = v UV.! 5
            a12 = v UV.! 6
            a13 = v UV.! 7
            a20 = v UV.! 8
            a21 = v UV.! 9
            a22 = v UV.! 10
            a23 = v UV.! 11
            a30 = v UV.! 12
            a31 = v UV.! 13
            a32 = v UV.! 14
            a33 = v UV.! 15
            r0 = Linear.V4 a00 a01 a02 a03
            r1 = Linear.V4 a10 a11 a12 a13
            r2 = Linear.V4 a20 a21 a22 a23
            r3 = Linear.V4 a30 a31 a32 a33
        in return $ Linear.V4 r0 r1 r2 r3
    | otherwise = fail "bad array size"

loadGLTFFile :: FilePath -> IO GLTF
loadGLTFFile filepath = either error return =<< Aeson.eitherDecodeFileStrict' filepath

parseUri :: FilePath -> Int -> ByteString -> IO ByteString
parseUri cd byteLength uri =
    case scheme of
        "data" -> do
            bs <- either (throwIO . userError) return . Base64.decode . ByteString.drop 1 . ByteString.dropWhile (/= ',') $ remainder
            return $ ByteString.take byteLength bs
        "file" -> do
            relativePath <- maybe (throwIO . userError $ "file uri parse error") return $ ByteString.stripPrefix "://" remainder
            path <- canonicalizePath $ cd </> ByteString.unpack relativePath
            bs <- ByteString.readFile path
            return $ ByteString.take byteLength bs
        _ -> throwIO . userError $ "unknown scheme: " ++ ByteString.unpack scheme
    where
    (scheme, remainder) = ByteString.break (/= ':') uri

createBuffer :: FilePath -> Hree.Scene -> Buffer -> IO GLW.Buffer
createBuffer cd scene (Buffer byteLength uri) = do
    bs <- parseUri cd byteLength (Text.encodeUtf8 uri)
    Hree.addBuffer scene (Hree.BufferSourceByteString bs GL.GL_STATIC_READ)

createBuffers :: FilePath -> Hree.Scene -> BV.Vector Buffer -> IO (BV.Vector GLW.Buffer)
createBuffers cd scene = BV.mapM (createBuffer cd scene)
