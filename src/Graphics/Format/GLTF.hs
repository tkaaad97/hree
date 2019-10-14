{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Format.GLTF
    ( Accessor(..)
    , Buffer(..)
    , BufferView(..)
    , ComponentType(..)
    , GLTF(..)
    , Mesh(..)
    , Node(..)
    , Primitive(..)
    , Scene(..)
    , ValueType(..)
    , componentByteSize
    , componentTypeToGLenum
    , createImageFromUri
    , loadGLTFFile
    , loadSceneFromFile
    , marshalComponentType
    , marshalValueType
    , numberOfComponent
    , unmarshalComponentType
    , unmarshalValueType
    ) where

import qualified Codec.Picture as Picture
import qualified Codec.Picture.Types as Picture
import Control.Exception (throwIO)
import Control.Monad (unless, void)
import qualified Data.Aeson as Aeson (FromJSON(..), Object, Value,
                                      eitherDecodeFileStrict', withObject,
                                      (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (drop, isPrefixOf, readFile,
                                                stripPrefix, take)
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
import qualified Data.Vector as BV (Vector, empty, imapM_, map, mapM, (!), (!?))
import qualified Data.Vector.Storable as SV (unsafeWith)
import qualified Data.Vector.Unboxed as UV (Vector, length, (!))
import Foreign (castPtr)
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Geometry as Hree (addAttribBindings, newGeometry)
import qualified Graphics.Hree.GL.Types as Hree (AttribFormat(..),
                                                 BindBufferSetting(..),
                                                 BufferSource(..),
                                                 IndexBuffer(..))
import qualified Graphics.Hree.Material as Hree (basicMaterial,
                                                 flatColorMaterial)
import Graphics.Hree.Math
import qualified Graphics.Hree.Scene as Hree (addBuffer, addMesh, addNode,
                                              addRootNodes, addTexture, newNode,
                                              updateNode)
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import qualified Graphics.Hree.Types as Hree (Geometry(..), Mesh(..), MeshId,
                                              Node(..), NodeId, Scene)
import qualified Linear (Quaternion(..), V3(..), V4(..), zero)
import System.Directory (canonicalizePath)
import System.FilePath (dropFileName, (</>))

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

data Image = Image
    { imageUri        :: !(Maybe Text)
    , imageMimeType   :: !(Maybe Text)
    , imageBufferView :: !(Maybe Int)
    , imageName       :: !(Maybe Text)
    , imageExtensions :: !(Maybe Aeson.Object)
    , imageExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data Sampler = Sampler
    { samplerMagFilter  :: !(Maybe Int)
    , samplerMinFilter  :: !(Maybe Int)
    , samplerWrapS      :: !Int
    , samplerWrapT      :: !Int
    , samplerName       :: !(Maybe Text)
    , samplerExtensions :: !(Maybe Aeson.Object)
    , samplerExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data Texture = Texture
    { textureSampler    :: !(Maybe Int)
    , textureSource     :: !(Maybe Int)
    , textureName       :: !(Maybe Text)
    , textureExtensions :: !(Maybe Aeson.Object)
    , textureExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data GLTF = GLTF
    { gltfScenes      :: !(BV.Vector Scene)
    , gltfNodes       :: !(BV.Vector Node)
    , gltfMeshes      :: !(BV.Vector Mesh)
    , gltfBuffers     :: !(BV.Vector Buffer)
    , gltfBufferViews :: !(BV.Vector BufferView)
    , gltfAccessors   :: !(BV.Vector Accessor)
    , gltfImages      :: !(BV.Vector Image)
    , gltfSamplers    :: !(BV.Vector Sampler)
    , gltfTextures    :: !(BV.Vector Texture)
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

instance Aeson.FromJSON Image where
    parseJSON = Aeson.withObject "Image" $ \v -> do
        uri <- v Aeson..:? "uri"
        mimeType <- v Aeson..:? "mimeType"
        bufferView <- v Aeson..:? "bufferView"
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extensions"
        extras <- v Aeson..:? "extras"
        return $ Image uri mimeType bufferView name extensions extras

instance Aeson.FromJSON Sampler where
    parseJSON = Aeson.withObject "Sampler" $ \v -> do
        magFilter <- v Aeson..:? "magFilter"
        minFilter <- v Aeson..:? "minFilter"
        wrapS <- v Aeson..: "wrapS" Aeson..!= defaultWrap
        wrapT <- v Aeson..: "wrapT" Aeson..!= defaultWrap
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extensions"
        extras <- v Aeson..:? "extras"
        return $ Sampler magFilter minFilter wrapS wrapT name extensions extras
        where
        defaultWrap = 10497

instance Aeson.FromJSON Texture where
    parseJSON = Aeson.withObject "Texture" $ \v -> do
        sampler <- v Aeson..:? "sampler"
        source <- v Aeson..:? "source"
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        return $ Texture sampler source name extensions extras

instance Aeson.FromJSON GLTF where
    parseJSON = Aeson.withObject "GLTF" $ \v -> do
        scenes <- v Aeson..:? "scenes" Aeson..!= BV.empty
        nodes <- v Aeson..:? "nodes" Aeson..!= BV.empty
        meshes <- v Aeson..: "meshes" Aeson..!= BV.empty
        buffers <- v Aeson..: "buffers" Aeson..!= BV.empty
        bufferViews <- v Aeson..: "bufferViews" Aeson..!= BV.empty
        accessors <- v Aeson..: "accessors" Aeson..!= BV.empty
        images <- v Aeson..:? "images" Aeson..!= BV.empty
        samplers <- v Aeson..:? "samplers" Aeson..!= BV.empty
        textures <- v Aeson..:? "textures" Aeson..!= BV.empty
        return $ GLTF scenes nodes meshes buffers bufferViews accessors images samplers textures

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

loadSceneFromFile :: FilePath -> Hree.Scene -> IO GLTF
loadSceneFromFile path scene = do
    gltf <- loadGLTFFile path
    let buffers_ = gltfBuffers gltf
        bufferViews_ = gltfBufferViews gltf
        accessors_ = gltfAccessors gltf
        meshes_ = gltfMeshes gltf
        nodes_ = gltfNodes gltf
        scenes_ = gltfScenes gltf
        rootNodes = maybe mempty sceneNodes $ scenes_ BV.!? 0
        basepath = dropFileName path
    buffers <- createBuffers basepath scene buffers_
    meshes <- createGLTFMeshes scene buffers bufferViews_ accessors_ meshes_
    createNodes scene nodes_ rootNodes meshes
    return gltf

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
    (scheme, remainder) = ByteString.break (== ':') uri

createBuffer :: FilePath -> Hree.Scene -> Buffer -> IO GLW.Buffer
createBuffer cd scene (Buffer byteLength uri) = do
    bs <- parseUri cd byteLength (Text.encodeUtf8 uri)
    Hree.addBuffer scene (Hree.BufferSourceByteString bs GL.GL_STATIC_READ)

createBuffers :: FilePath -> Hree.Scene -> BV.Vector Buffer -> IO (BV.Vector GLW.Buffer)
createBuffers cd scene = BV.mapM (createBuffer cd scene)

createGLTFMeshes :: Hree.Scene -> BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Mesh -> IO (BV.Vector (BV.Vector Hree.MeshId))
createGLTFMeshes scene buffers bufferViews accessors =
    BV.mapM (createMeshes scene buffers bufferViews accessors)

createMeshes :: Hree.Scene -> BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> Mesh -> IO (BV.Vector Hree.MeshId)
createMeshes scene buffers bufferViews accessors =
    BV.mapM (createMeshFromPrimitive scene buffers bufferViews accessors) . meshPrimitives

createMeshFromPrimitive :: Hree.Scene -> BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> Primitive -> IO Hree.MeshId
createMeshFromPrimitive scene buffers bufferViews accessors primitive = do
    groups <- either error return $ groupAttributes buffers bufferViews accessors primitive
    let (_, geometry) = foldl addAttribBindings (1, Hree.newGeometry) groups
        verticesCount = minimum . map (accessorCount . snd) . concatMap snd $ groups
        geometry1 = geometry { Hree.geometryVerticesCount = verticesCount }

    geometry2 <- flip (maybe (return geometry1)) (primitiveIndices primitive) $
        either error return . setIndexBuffer geometry1 buffers bufferViews accessors

    let material = Hree.flatColorMaterial (Linear.V4 1 0 0 1) -- Hree.basicMaterial Nothing
        mesh = Hree.Mesh geometry2 material Nothing
    Hree.addMesh scene mesh

addAttribBindings :: (Int, Hree.Geometry) -> ((BufferView, GLW.Buffer), [(Text, Accessor)]) -> (Int, Hree.Geometry)
addAttribBindings (bindingIndex, geometry0) ((bufferView, buffer), attribs) =
    let geometry1 = Hree.addAttribBindings geometry0 bindingIndex attribFormats (buffer, bbs)
    in (bindingIndex + 1, geometry1)
    where
    accessors = map snd attribs
    offset = bufferViewByteOffset bufferView
    stride = fromMaybe (calcStrideFromAccessors accessors) $ bufferViewByteStride bufferView
    bbs = Hree.BindBufferSetting offset stride 0
    attribFormats = Map.fromList . map f $ attribs
    f (key, accessor) = (convertAttribName key, accessorToAttribFormat accessor)

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

convertAttribName :: Text -> ByteString
convertAttribName name
    | name == "POSITION" = "position"
    | name == "NORMAL" = "normal"
    | name == "TANGENT" = "tangent"
    | name == "TEXCOORD_0" = "uv"
    | name == "TEXCOORD_1" = "uv1"
    | name == "COLOR_0" = "color"
    | name == "JOINTS_0" = "joint"
    | name == "WEIGHTS_0" = "weight"
    | otherwise = "unknown"

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

setIndexBuffer :: Hree.Geometry -> BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> Either String Hree.Geometry
setIndexBuffer geometry buffers bufferViews accessors i = do
    accessor <- maybe (fail $ "invalid accessor identifier: " ++ show i) return $ accessors BV.!? i
    bufferView <- maybe (fail $ "invalid bufferView identifier: " ++ show (accessorBufferView accessor)) return $ bufferViews BV.!? accessorBufferView accessor
    buffer <- maybe (fail $ "invalid buffer identifier: " ++ show (bufferViewBuffer bufferView)) return $ buffers BV.!? bufferViewBuffer bufferView
    indexBuffer <- createIndexBuffer buffer bufferView accessor
    return $ geometry { Hree.geometryIndexBuffer = Just indexBuffer }

createIndexBuffer :: GLW.Buffer -> BufferView -> Accessor -> Either String Hree.IndexBuffer
createIndexBuffer buffer bufferView accessor = do
    unless (accessorType accessor == Scalar) $
        fail ("accessorType should be SCALAR. but actual accessorType: " ++ show (accessorType accessor))
    dataType <- convertToIndexBufferDataType (accessorComponentType accessor)
    let count = accessorCount accessor
        offset = bufferViewByteOffset bufferView
    return $ Hree.IndexBuffer buffer dataType (fromIntegral count) offset

convertToIndexBufferDataType :: ComponentType -> Either String GL.GLenum
convertToIndexBufferDataType UnsignedByte' = return GL.GL_UNSIGNED_BYTE
convertToIndexBufferDataType UnsignedShort' = return GL.GL_UNSIGNED_SHORT
convertToIndexBufferDataType UnsignedInt' = return GL.GL_UNSIGNED_INT
convertToIndexBufferDataType a = fail $ "invalid componentType: " ++ show a

createNodes :: Hree.Scene -> BV.Vector Node -> BV.Vector Int -> BV.Vector (BV.Vector Hree.MeshId) -> IO ()
createNodes scene nodes rootNodes meshes = do
    nodeIds <- BV.mapM (createNode scene) nodes
    BV.imapM_ (setNodeChildren scene nodeIds) nodes
    BV.imapM_ (createMeshNodes scene nodeIds meshes) nodes
    let rootNodeIds = BV.map (nodeIds BV.!) rootNodes
    Hree.addRootNodes scene rootNodeIds

createNode :: Hree.Scene -> Node -> IO Hree.NodeId
createNode scene a =
    Hree.addNode scene node False
    where
    translation = fromMaybe Linear.zero (nodeTranslation a)
    rotation = fromMaybe Linear.zero (nodeRotation a)
    scale = fromMaybe (Linear.V3 1 1 1) (nodeScale a)
    node = Hree.newNode
            { Hree.nodeName = nodeName a
            , Hree.nodeTranslation = translation
            , Hree.nodeRotation = rotation
            , Hree.nodeScale = scale
            }

setNodeChildren :: Hree.Scene -> BV.Vector Hree.NodeId -> Int -> Node -> IO ()
setNodeChildren scene nodeIds i a =
    void $ Hree.updateNode scene nodeId f
    where
    nodeId = nodeIds BV.! i
    children = BV.map (nodeIds BV.!) (nodeChildren a)
    f node = node { Hree.nodeChildren = children }

createMeshNodes :: Hree.Scene -> BV.Vector Hree.NodeId -> BV.Vector (BV.Vector Hree.MeshId) -> Int -> Node -> IO ()
createMeshNodes scene nodeIds meshes i a =
    flip (maybe (return ())) (nodeMesh a) $ \j -> do
        let nodeId = nodeIds BV.! i
            meshIds = meshes BV.! j
        children <- BV.mapM (createMeshNode scene) meshIds
        void . Hree.updateNode scene nodeId $ \node ->
            node { Hree.nodeChildren = Hree.nodeChildren node `mappend` children }

createMeshNode :: Hree.Scene -> Hree.MeshId -> IO Hree.NodeId
createMeshNode scene meshId =
    Hree.addNode scene node False
    where
    node = Hree.newNode { Hree.nodeMesh = Just meshId }

createImage :: FilePath -> BV.Vector ByteString -> BV.Vector BufferView -> Image -> IO (Picture.Image Picture.PixelRGBA8)
createImage cd buffers bufferViews image = go (imageUri image) (imageBufferView image) (imageMimeType image)
    where
    go (Just uri) _ mimeType = createImageFromUri cd (Text.encodeUtf8 uri) mimeType
    go _ (Just bvid) mimeType = createImageFromBuffer buffers bufferViews bvid mimeType
    go _ _ _ = throwIO . userError $ "invalid image: " ++ show image

createImageFromUri :: FilePath -> ByteString -> Maybe Text -> IO (Picture.Image Picture.PixelRGBA8)
createImageFromUri cd uri mimeType
    | ByteString.isPrefixOf "data:" uri = do
        bs <- either (throwIO . userError) return . Base64.decode . ByteString.drop 1 . ByteString.dropWhile (/= ',') $ uri
        decodeImage mimeType bs
    | otherwise = do
        path <- canonicalizePath $ cd </> ByteString.unpack uri
        readImage mimeType path

createImageFromBuffer :: BV.Vector ByteString -> BV.Vector BufferView -> Int -> Maybe Text -> IO (Picture.Image Picture.PixelRGBA8)
createImageFromBuffer buffers bufferViews bvid mimeType = decodeImage mimeType =<< sliceBuffer
    where
    sliceBuffer = do
        bufferView <- maybe (throwIO . userError $ "invalid bufferView identifier: " ++ show bvid) return $ bufferViews BV.!? bvid
        let bid = bufferViewBuffer bufferView
            offset = bufferViewByteOffset bufferView
            length = bufferViewByteLength bufferView
        buffer <- maybe (throwIO . userError $ "invalid buffer identifier: " ++ show bid) return $ buffers BV.!? bid
        return . ByteString.take length . ByteString.drop offset $ buffer

decodeImage :: Maybe Text -> ByteString -> IO (Picture.Image Picture.PixelRGBA8)
decodeImage mimeType bs
    | mimeType == Just "image/png" = do
        image <- either (throwIO . userError) return . Picture.decodePng $ bs
        return (Picture.convertRGBA8 image)
    | mimeType == Just "image/jpeg" = do
        image <- either (throwIO . userError) return . Picture.decodeJpeg $ bs
        return (Picture.convertRGBA8 image)
    | otherwise = do
        image <- either (throwIO . userError) return . Picture.decodeImage $ bs
        return (Picture.convertRGBA8 image)

readImage :: Maybe Text -> FilePath -> IO (Picture.Image Picture.PixelRGBA8)
readImage mimeType path
    | mimeType == Just "image/png" = do
        image <- either (throwIO . userError) return =<< Picture.readPng path
        return (Picture.convertRGBA8 image)
    | mimeType == Just "image/jpeg" = do
        image <- either (throwIO . userError) return =<< Picture.readJpeg path
        return (Picture.convertRGBA8 image)
    | otherwise = do
        image <- either (throwIO . userError) return =<< Picture.readImage path
        return (Picture.convertRGBA8 image)

createTexture :: FilePath -> Hree.Scene -> BV.Vector ByteString -> BV.Vector BufferView -> Image -> IO (GLW.Texture 'GLW.GL_TEXTURE_2D)
createTexture cd scene buffers bufferViews image = do
    source <- createImage cd buffers bufferViews image
    let name = Text.encodeUtf8 $ fromMaybe "glTF_texture_" (imageName image)
        width = fromIntegral $ Picture.imageWidth source
        height = fromIntegral $ Picture.imageHeight source
        settings = Hree.TextureSettings 0 GL.GL_RGBA width height False
    (_, texture) <- SV.unsafeWith (Picture.imageData source) $ \ptr -> do
        let sourceData = Hree.TextureSourceData width height PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (castPtr ptr)
        Hree.addTexture scene name settings sourceData
    return texture
