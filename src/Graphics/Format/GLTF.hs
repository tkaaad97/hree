{-# LANGUAGE OverloadedStrings #-}
module Graphics.Format.GLTF
    ( ComponentType(..)
    , ValueType(..)
    , Buffer(..)
    , BufferView(..)
    , Accessor(..)
    , Mesh(..)
    , Primitive(..)
    , Node(..)
    , Scene(..)
    , GLTF(..)
    , loadGLTFFile
    , marshalComponentType
    , unmarshalComponentType
    , componentByteSize
    , componentTypeToGLenum
    , marshalValueType
    , unmarshalValueType
    , numberOfComponent
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
import qualified Data.List as List (groupBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, mapWithKey, toList)
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, empty, mapM, (!?))
import qualified Data.Vector as UV (Vector, length, (!))
import qualified GLW (Buffer)
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree (addAttribBindings, newGeometry)
import qualified Graphics.Hree.GL.Types as Hree (AttribFormat(..),
                                                 BindBufferSetting(..),
                                                 BufferSource(..))
import qualified Graphics.Hree.Material as Hree (basicMaterial)
import Graphics.Hree.Math
import qualified Graphics.Hree.Scene as Hree (Scene, addBuffer, addMesh)
import qualified Graphics.Hree.Types as Hree (Geometry(..), Mesh(..), MeshId)
import qualified Linear (Quaternion(..), V3(..), V4(..))
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

data ComponentType =
    Byte' |
    UnsignedByte' |
    Short' |
    UnsignedShort' |
    UnsignedInt' |
    Float'
    deriving (Show, Eq)

data ValueType =
    Scalar |
    Vec2 |
    Vec3 |
    Vec4 |
    Mat2 |
    Mat3 |
    Mat4
    deriving (Show, Eq)

data Buffer = Buffer
    { bufferByteLength :: !Int
    , bufferUri        :: !Text
    } deriving (Show, Eq)

data BufferView = BufferView
    { bufferViewBuffer     :: !Int
    , bufferViewByteLength :: !Int
    , bufferViewByteOffset :: !Int
    , bufferViewByteStride :: !(Maybe Int)
    , bufferViewTarget     :: !(Maybe Int)
    , bufferViewName       :: !(Maybe Text)
    } deriving (Show, Eq)

data Accessor = Accessor
    { accessorBufferView    :: !Int
    , accessorByteOffset    :: !Int
    , accessorComponentType :: !ComponentType
    , accessorNormalized    :: !Bool
    , accessorCount         :: !Int
    , accessorType          :: !ValueType
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
        stride <- v Aeson..:? "byteStride"
        target <- v Aeson..:? "target"
        name <- v Aeson..:? "name"
        return $ BufferView buffer len offset stride target name

instance Aeson.FromJSON Accessor where
    parseJSON = Aeson.withObject "Accessor" $ \v -> do
        bufferView <- v Aeson..: "bufferView"
        offset <- v Aeson..:? "byteOffset" Aeson..!= 0
        componentType' <- v Aeson..: "componentType"
        normalized <- v Aeson..:? "normalized" Aeson..!= False
        count <- v Aeson..: "count"
        type' <- v Aeson..: "type"
        name <- v Aeson..:? "name"
        componentType <- maybe (fail $ "invalid component type:" ++ show componentType') return $ unmarshalComponentType componentType'
        valueType <- maybe (fail $ "invalid type:" ++ Text.unpack type') return $ unmarshalValueType type'
        return $ Accessor bufferView offset componentType normalized count valueType name

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

marshalComponentType :: ComponentType -> Int
marshalComponentType Byte'          = 5120
marshalComponentType UnsignedByte'  = 5121
marshalComponentType Short'         = 5122
marshalComponentType UnsignedShort' = 5123
marshalComponentType UnsignedInt'   = 5125
marshalComponentType Float'         = 5126

unmarshalComponentType :: Int -> Maybe ComponentType
unmarshalComponentType a
    | a == 5120 = Just Byte'
    | a == 5121 = Just UnsignedByte'
    | a == 5122 = Just Short'
    | a == 5123 = Just UnsignedShort'
    | a == 5125 = Just UnsignedInt'
    | a == 5126 = Just Float'
    | otherwise = Nothing

componentByteSize :: ComponentType -> Int
componentByteSize Byte'          = 1
componentByteSize UnsignedByte'  = 1
componentByteSize Short'         = 2
componentByteSize UnsignedShort' = 2
componentByteSize UnsignedInt'   = 4
componentByteSize Float'         = 4

componentTypeToGLenum :: ComponentType -> GL.GLenum
componentTypeToGLenum Byte'          = GL.GL_BYTE
componentTypeToGLenum UnsignedByte'  = GL.GL_UNSIGNED_BYTE
componentTypeToGLenum Short'         = GL.GL_SHORT
componentTypeToGLenum UnsignedShort' = GL.GL_UNSIGNED_SHORT
componentTypeToGLenum UnsignedInt'   = GL.GL_UNSIGNED_INT
componentTypeToGLenum Float'         = GL.GL_FLOAT

marshalValueType :: ValueType -> Text
marshalValueType Scalar = "SCALAR"
marshalValueType Vec2   = "VEC2"
marshalValueType Vec3   = "VEC3"
marshalValueType Vec4   = "VEC4"
marshalValueType Mat2   = "MAT2"
marshalValueType Mat3   = "MAT3"
marshalValueType Mat4   = "MAT4"

unmarshalValueType :: Text -> Maybe ValueType
unmarshalValueType a
    | a == "SCALAR" = Just Scalar
    | a == "VEC2" = Just Vec2
    | a == "VEC3" = Just Vec3
    | a == "VEC4" = Just Vec4
    | a == "MAT2" = Just Mat2
    | a == "MAT3" = Just Mat3
    | a == "MAT4" = Just Mat4
    | otherwise = Nothing

numberOfComponent :: ValueType -> Int
numberOfComponent Scalar = 1
numberOfComponent Vec2   = 2
numberOfComponent Vec3   = 3
numberOfComponent Vec4   = 4
numberOfComponent Mat2   = 2
numberOfComponent Mat3   = 3
numberOfComponent Mat4   = 4

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

createMeshFromPrimitive :: Hree.Scene -> BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> Primitive -> IO Hree.MeshId
createMeshFromPrimitive scene buffers bufferViews accessors primitive = do
    groups <- either error return $ groupAttributes buffers bufferViews accessors primitive
    let (_, geometry) = foldl addAttribBindings (1, Hree.newGeometry) groups
    let material = Hree.basicMaterial Nothing
        mesh = Hree.Mesh geometry material Nothing
    Hree.addMesh scene mesh

addAttribBindings :: (Int, Hree.Geometry) -> ((BufferView, GLW.Buffer), [(Text, Accessor)]) -> (Int, Hree.Geometry)
addAttribBindings (bindingIndex, geometry0) ((bufferView, buffer), attribs) =
    let geometry1 = Hree.addAttribBindings geometry0 bindingIndex attribFormats (buffer, bbs)
        geometry2 = geometry1 { Hree.geometryVerticesCount = min vertexCount (Hree.geometryVerticesCount geometry0) }
    in (bindingIndex + 1, geometry2)
    where
    accessors = map snd attribs
    offset = bufferViewByteOffset bufferView
    stride = fromMaybe (calcStrideFromAccessors accessors) $ bufferViewByteStride bufferView
    bbs = Hree.BindBufferSetting offset stride 0
    vertexCount = minimum . map (accessorCount . snd) $ attribs
    attribFormats = Map.fromList . map f $ attribs
    f (key, accessor) = (Text.encodeUtf8 key, accessorToAttribFormat accessor)

accessorToAttribFormat :: Accessor -> Hree.AttribFormat
accessorToAttribFormat accessor =
    Hree.AttribFormat num formatComponentType normalized offset
    where
    componentType = accessorComponentType accessor
    valueType = accessorType accessor
    normalized = accessorNormalized accessor
    offset = accessorByteOffset accessor
    formatComponentType = componentTypeToGLenum componentType
    num = numberOfComponent valueType

calcStrideFromAccessors :: [Accessor] -> Int
calcStrideFromAccessors = sum . map accessorByteStride

accessorByteStride :: Accessor -> Int
accessorByteStride accessor = num * componentByteSize componentType
    where
    componentType = accessorComponentType accessor
    valueType = accessorType accessor
    num = numberOfComponent valueType

groupAttributes :: BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> Primitive -> Either String [((BufferView, GLW.Buffer), [(Text, Accessor)])]
groupAttributes buffers bufferViews accessors primitive = do
    attributes <- mapM f . Map.toList . primitiveAttributes $ primitive
    let groups = List.groupBy sameBufferView attributes
        groups' = map g groups
    return groups'
    where
    f (key, aid) = do
        accessor <- maybe (Left $ "invalid accessor identifier: " ++ show aid) return $ accessors BV.!? aid
        let bvid = accessorBufferView accessor
        bufferView <- maybe (Left $ "invalid bufferView identifier: " ++ show bvid) return $ bufferViews BV.!? bvid
        let bid = bufferViewBuffer bufferView
        buffer <- maybe (Left $ "invalid buffer identifier: " ++ show bid) return $ buffers BV.!? bid
        return ((bufferView, buffer), (key, accessor))
    g group =
        let (common, _) = head group
        in (common, map snd group)
    sameBufferView ((a, _), _) ((b, _), _) = a == b
