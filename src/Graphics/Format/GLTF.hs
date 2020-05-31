{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Format.GLTF
    ( Accessor(..)
    , AlphaMode(..)
    , Buffer(..)
    , BufferView(..)
    , ComponentType(..)
    , GLTF(..)
    , Image(..)
    , Material(..)
    , Mesh(..)
    , Node(..)
    , NormalTextureInfo(..)
    , OcclusionTextureInfo(..)
    , PbrMetallicRoughness(..)
    , Primitive(..)
    , Sampler(..)
    , Scene(..)
    , Supplement(..)
    , Texture(..)
    , TextureInfo(..)
    , ValueType(..)
    , componentByteSize
    , componentTypeToGLenum
    , createImageFromUri
    , loadGLTFFile
    , loadSceneFromFile
    , loadSceneFromGLBFile
    , loadSceneFromGLBBin
    , marshalComponentType
    , marshalValueType
    , numberOfComponent
    , searchCommonRoot
    , unmarshalComponentType
    , unmarshalValueType
    ) where

import qualified Codec.Picture as Picture
import Control.Exception (throwIO)
import Control.Monad (unless, void)
import qualified Data.Aeson as Aeson (FromJSON(..), Object, Value,
                                      eitherDecodeFileStrict',
                                      eitherDecodeStrict', withObject, withText,
                                      (.!=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (drop, isPrefixOf, readFile,
                                                stripPrefix, take)
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.ByteString.Char8 as ByteString (break, dropWhile, unpack,
                                                      useAsCString)
import Data.Either (either)
import Data.Function ((&))
import Data.Int (Int16, Int8)
import qualified Data.IntSet as IntSet (empty, fromList, isSubsetOf, member,
                                        union)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (singleton, toList)
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as BV (Vector, cons, drop, empty, foldM', head,
                                    imapM_, length, map, mapM, singleton,
                                    toList, unsafeFreeze, unzip, (!), (!?))
import qualified Data.Vector.Mutable as MBV (read, replicate, write)
import qualified Data.Vector.Storable as SV (empty, generate, generateM,
                                             unsafeWith)
import qualified Data.Vector.Unboxed as UV (Vector, generateM, length, map,
                                            unsafeFreeze, (!))
import qualified Data.Vector.Unboxed.Mutable as MUV (read, replicate, write)
import Data.Word (Word16, Word32, Word8)
import qualified Foreign (Ptr, Storable(..), castPtr, peekByteOff)
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Animation as Hree (AnimationClip(..),
                                                  Interpolation(..),
                                                  KeyFrames(..),
                                                  TransformChannel(..),
                                                  TransformTrack(..),
                                                  animationClipTransform)
import qualified Graphics.Hree.Geometry as Hree (addAttribBindings, newGeometry)
import qualified Graphics.Hree.GL as Hree (attribFormat, attribIFormat)
import qualified Graphics.Hree.GL.Types as Hree (AttributeFormat(..),
                                                 BindBufferSetting(..),
                                                 BufferSource(..),
                                                 IndexBuffer(..), Mat4,
                                                 Texture(..), Vec3, Vec4)
import qualified Graphics.Hree.Material.FlatColorMaterial as Hree (FlatColorMaterial,
                                                                   flatColorMaterial)
import qualified Graphics.Hree.Material.StandardMaterial as Hree (StandardMaterial(..),
                                                                  StandardMaterialBlock(..),
                                                                  standardMaterial,
                                                                  standardMaterialBlock)
import qualified Graphics.Hree.Math as Hree (Quaternion, Transform(..),
                                             matrixToTransform)
import qualified Graphics.Hree.Sampler as Hree.Sampler (glTextureMagFilter,
                                                        glTextureMinFilter,
                                                        glTextureWrapS,
                                                        glTextureWrapT,
                                                        setSamplerParameter)
import qualified Graphics.Hree.Scene as Hree (AddedMesh(..), addBuffer, addMesh,
                                              addNode, addRootNodes, addSampler,
                                              addSkin, addSkinnedMesh,
                                              addTexture,
                                              mkDefaultTextureIfNotExists,
                                              newNode, updateNode)
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import qualified Graphics.Hree.Types as Hree (Geometry(..), Mesh(..), MeshId,
                                              Node(..), NodeId, Scene, SkinId)
import qualified Linear (Quaternion(..), V3(..), V4(..), transpose, zero)
import Numeric (showHex)
import System.Directory (canonicalizePath)
import System.FilePath (dropFileName, takeExtension, (</>))

data ComponentType =
    Byte' |
    UnsignedByte' |
    Short' |
    UnsignedShort' |
    Int' |
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
    , bufferUri        :: !(Maybe Text)
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
    , nodeMatrix      :: !(Maybe Hree.Mat4)
    , nodeMesh        :: !(Maybe Int)
    , nodeRotation    :: !(Maybe Hree.Quaternion)
    , nodeScale       :: !(Maybe Hree.Vec3)
    , nodeTranslation :: !(Maybe Hree.Vec3)
    , nodeName        :: !(Maybe Text)
    } deriving (Show, Eq)

data Animation = Animation
    { animationChannels   :: !(BV.Vector Channel)
    , animationSamplers   :: !(BV.Vector AnimationSampler)
    , animationName       :: !(Maybe Text)
    , animationExtensions :: !(Maybe Aeson.Object)
    , animationExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data Channel = Channel
    { channelSampler :: !Int
    , channelTarget  :: !ChannelTarget
    } deriving (Show, Eq)

data ChannelTarget = ChannelTarget
    { channelTargetNode :: !Int
    , channelTargetPath :: !ChannelTargetPath
    } deriving (Show, Eq)

data ChannelTargetPath =
    ChannelTargetPathTranslation |
    ChannelTargetPathRotation |
    ChannelTargetPathScale
    deriving (Show, Eq)

data AnimationSampler = AnimationSampler
    { animationSamplerInput         :: !Int
    , animationSamplerInterpolation :: !AnimationInterpolation
    , animationSamplerOutput        :: !Int
    } deriving (Show, Eq)

data AnimationInterpolation =
    AnimationInterpolationLinear |
    AnimationInterpolationStep |
    AnimationInterpolationCubicSpline
    deriving (Show, Eq)

data Skin = Skin
    { skinInverseBindMatrices :: !(Maybe Int)
    , skinSkeleton            :: !(Maybe Int)
    , skinJoints              :: !(BV.Vector Int)
    , skinName                :: !(Maybe Text)
    , skinExtensions          :: !(Maybe Aeson.Object)
    , skinExtras              :: !(Maybe Aeson.Value)
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

data Material = Material
    { materialName                 :: !(Maybe Text)
    , materialExtensions           :: !(Maybe Aeson.Object)
    , materialExtras               :: !(Maybe Aeson.Value)
    , materialPbrMetallicRoughness :: !(Maybe PbrMetallicRoughness)
    , materialNormalTexture        :: !(Maybe NormalTextureInfo)
    , materialOcclusionTexture     :: !(Maybe OcclusionTextureInfo)
    , materialEmissiveTexture      :: !(Maybe TextureInfo)
    , materialEmissiveFactor       :: !(Linear.V3 Float)
    , materialAlphaMode            :: !AlphaMode
    , materialAlphaCutoff          :: !Float
    , materialDoubleSided          :: !Bool
    } deriving (Show, Eq)

data PbrMetallicRoughness = PbrMetallicRoughness
    { pbrBaseColorFactor          :: !(Linear.V4 Float)
    , pbrBaseColorTexture         :: !(Maybe TextureInfo)
    , pbrMetallicFactor           :: !Float
    , pbrRoughnessFactor          :: !Float
    , pbrMetallicRoughnessTexture :: !(Maybe TextureInfo)
    , pbrExtensions               :: !(Maybe Aeson.Object)
    , pbrExtras                   :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data TextureInfo = TextureInfo
    { textureInfoIndex      :: !Int
    , textureInfoTexCoord   :: !Int
    , textureInfoExtensions :: !(Maybe Aeson.Object)
    , textureInfoExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data NormalTextureInfo = NormalTextureInfo
    { normalTextureInfoIndex      :: !Int
    , normalTextureInfoTexCoord   :: !Int
    , normalTextureInfoScale      :: !Float
    , normalTextureInfoExtensions :: !(Maybe Aeson.Object)
    , normalTextureInfoExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data OcclusionTextureInfo = OcclusionTextureInfo
    { occlusionTextureInfoIndex      :: !Int
    , occlusionTextureInfoTexCoord   :: !Int
    , occlusionTextureInfoStrength   :: !Float
    , occlusionTextureInfoExtensions :: !(Maybe Aeson.Object)
    , occlusionTextureInfoExtras     :: !(Maybe Aeson.Value)
    } deriving (Show, Eq)

data AlphaMode =
    AlphaModeOpaque |
    AlphaModeMask |
    AlphaModeBlend
    deriving (Show, Eq)

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
    , gltfMaterials   :: !(BV.Vector Material)
    , gltfAnimations  :: !(BV.Vector Animation)
    , gltfSkins       :: !(BV.Vector Skin)
    } deriving (Show, Eq)

data Supplement = Supplement
    { supplementNodeIds    :: !(BV.Vector Hree.NodeId)
    , supplementAnimations :: !(BV.Vector Hree.AnimationClip)
    } deriving (Show)

data GLBHeader = GLBHeader Word32 Word32 Word32
    deriving (Show, Eq)

data ChunkHeader = ChunkHeader Word32 Word32
    deriving (Show, Eq)

instance Aeson.FromJSON Buffer where
    parseJSON = Aeson.withObject "Buffer" $ \v -> do
        len <- v Aeson..: "byteLength"
        uri <- v Aeson..:? "uri"
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

instance Aeson.FromJSON Animation where
    parseJSON = Aeson.withObject "Animation" $ \v -> do
        channels <- v Aeson..: "channels"
        samplers <- v Aeson..: "samplers"
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extensions"
        extras <- v Aeson..:? "extras"
        return $ Animation channels samplers name extensions extras

instance Aeson.FromJSON Channel where
    parseJSON = Aeson.withObject "Channel" $ \v -> do
        sampler <- v Aeson..: "sampler"
        target <- v Aeson..: "target"
        return $ Channel sampler target

instance Aeson.FromJSON ChannelTarget where
    parseJSON = Aeson.withObject "ChannelTarget" $ \v -> do
        node <- v Aeson..: "node"
        path <- v Aeson..: "path"
        return $ ChannelTarget node path

instance Aeson.FromJSON ChannelTargetPath where
    parseJSON = Aeson.withText "ChannelTargetPath" f
        where
        f "translation" = return ChannelTargetPathTranslation
        f "rotation"   = return ChannelTargetPathRotation
        f "scale"  = return ChannelTargetPathScale
        f other    = fail $ "invalid channel target path: " ++ Text.unpack other

instance Aeson.FromJSON AnimationSampler where
    parseJSON = Aeson.withObject "AnimationSampler" $ \v -> do
        input <- v Aeson..: "input"
        interpolation <- v Aeson..:? "interpolation" Aeson..!= AnimationInterpolationLinear
        output <- v Aeson..: "output"
        return $ AnimationSampler input interpolation output

instance Aeson.FromJSON AnimationInterpolation where
    parseJSON = Aeson.withText "AnimationInterpolation" f
        where
        f "LINEAR" = return AnimationInterpolationLinear
        f "STEP"   = return AnimationInterpolationStep
        f "CUBICSPLINE"  = return AnimationInterpolationCubicSpline
        f other    = fail $ "invalid animation interpolation: " ++ Text.unpack other

instance Aeson.FromJSON Skin where
    parseJSON = Aeson.withObject "Skin" $ \v -> do
        invMats <- v Aeson..:? "inverseBindMatrices"
        skeleton <- v Aeson..:? "skeleton"
        joints <- v Aeson..: "joints"
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extensions"
        extras <- v Aeson..:? "extras"
        return $ Skin invMats skeleton joints name extensions extras


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
        wrapS <- v Aeson..:? "wrapS" Aeson..!= defaultWrap
        wrapT <- v Aeson..:? "wrapT" Aeson..!= defaultWrap
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

instance Aeson.FromJSON Material where
     parseJSON = Aeson.withObject "Material" $ \v -> do
        name <- v Aeson..:? "name"
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        pbr <- v Aeson..:? "pbrMetallicRoughness"
        normalTexture <- v Aeson..:? "normalTexture"
        occlusionTexture <- v Aeson..:? "occlusionTexture"
        emissiveTexture <- v Aeson..:? "emissiveTexture"
        emissiveFactor <- maybe (return $ Linear.V3 0 0 0) fromVectorToVec3 =<< v Aeson..:? "emissiveFactor"
        alphaMode <- v Aeson..:? "alphaMode" Aeson..!= AlphaModeOpaque
        alphaCutoff <- v Aeson..:? "alphaCutoff" Aeson..!= 0.5
        doubleSided <- v Aeson..:? "doubleSided" Aeson..!= False
        return $ Material name extensions extras pbr normalTexture occlusionTexture emissiveTexture emissiveFactor alphaMode alphaCutoff doubleSided

instance Aeson.FromJSON PbrMetallicRoughness where
    parseJSON = Aeson.withObject "PbrMetallicRoughenss" $ \v -> do
        baseColorFactor <- maybe (return $ Linear.V4 1 1 1 1) fromVectorToVec4 =<< v Aeson..:? "baseColorFactor"
        baseColorTexture <- v Aeson..:? "baseColorTexture"
        metallicFactor <- v Aeson..:? "metallicFactor" Aeson..!= 1
        roughnessFactor <- v Aeson..:? "roughnessFactor" Aeson..!= 1
        metallicRoughnessTexture <- v Aeson..:? "metallicRoughnessTexture"
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        return $ PbrMetallicRoughness baseColorFactor baseColorTexture metallicFactor roughnessFactor metallicRoughnessTexture extensions extras

instance Aeson.FromJSON TextureInfo where
    parseJSON = Aeson.withObject "TextureInfo" $ \v -> do
        index <- v Aeson..: "index"
        textureCoord <- v Aeson..:? "texCoord" Aeson..!= 0
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        return $ TextureInfo index textureCoord extensions extras

instance Aeson.FromJSON NormalTextureInfo where
    parseJSON = Aeson.withObject "NormalTextureInfo" $ \v -> do
        index <- v Aeson..: "index"
        textureCoord <- v Aeson..:? "texCoord" Aeson..!= 0
        scale <- v Aeson..:? "scale" Aeson..!= 1.0
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        return $ NormalTextureInfo index textureCoord scale extensions extras

instance Aeson.FromJSON OcclusionTextureInfo where
    parseJSON = Aeson.withObject "OcclusionTextureInfo" $ \v -> do
        index <- v Aeson..: "index"
        textureCoord <- v Aeson..:? "texCoord" Aeson..!= 0
        strength <- v Aeson..:? "strength" Aeson..!= 1.0
        extensions <- v Aeson..:? "extension"
        extras <- v Aeson..:? "extras"
        return $ OcclusionTextureInfo index textureCoord strength extensions extras

instance Aeson.FromJSON AlphaMode where
    parseJSON = Aeson.withText "AlphaMode" f
        where
        f "OPAQUE" = return AlphaModeOpaque
        f "MASK"   = return AlphaModeMask
        f "BLEND"  = return AlphaModeBlend
        f other    = fail $ "invalid alphaMode: " ++ Text.unpack other

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
        materials <- v Aeson..:? "materials" Aeson..!= BV.empty
        animations <- v Aeson..:? "animations" Aeson..!= BV.empty
        skins <- v Aeson..:? "skins" Aeson..!= BV.empty
        return $ GLTF scenes nodes meshes buffers bufferViews accessors images samplers textures materials animations skins

marshalComponentType :: ComponentType -> Int
marshalComponentType Byte'          = 5120
marshalComponentType UnsignedByte'  = 5121
marshalComponentType Short'         = 5122
marshalComponentType UnsignedShort' = 5123
marshalComponentType Int'           = 5124
marshalComponentType UnsignedInt'   = 5125
marshalComponentType Float'         = 5126

unmarshalComponentType :: Int -> Maybe ComponentType
unmarshalComponentType a
    | a == 5120 = Just Byte'
    | a == 5121 = Just UnsignedByte'
    | a == 5122 = Just Short'
    | a == 5123 = Just UnsignedShort'
    | a == 5124 = Just Int'
    | a == 5125 = Just UnsignedInt'
    | a == 5126 = Just Float'
    | otherwise = Nothing

componentByteSize :: ComponentType -> Int
componentByteSize Byte'          = 1
componentByteSize UnsignedByte'  = 1
componentByteSize Short'         = 2
componentByteSize UnsignedShort' = 2
componentByteSize Int'           = 4
componentByteSize UnsignedInt'   = 4
componentByteSize Float'         = 4

componentTypeToGLenum :: ComponentType -> GL.GLenum
componentTypeToGLenum Byte'          = GL.GL_BYTE
componentTypeToGLenum UnsignedByte'  = GL.GL_UNSIGNED_BYTE
componentTypeToGLenum Short'         = GL.GL_SHORT
componentTypeToGLenum UnsignedShort' = GL.GL_UNSIGNED_SHORT
componentTypeToGLenum Int'           = GL.GL_INT
componentTypeToGLenum UnsignedInt'   = GL.GL_UNSIGNED_INT
componentTypeToGLenum Float'         = GL.GL_FLOAT

isIntegralType :: ComponentType -> Bool
isIntegralType Byte'          = True
isIntegralType UnsignedByte'  = True
isIntegralType Short'         = True
isIntegralType UnsignedShort' = True
isIntegralType Int'           = True
isIntegralType UnsignedInt'   = True
isIntegralType Float'         = False

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

fromVectorToVec3 :: UV.Vector Float -> Aeson.Parser Hree.Vec3
fromVectorToVec3 v
    | UV.length v == 3 =
        let a0 = v UV.! 0
            a1 = v UV.! 1
            a2 = v UV.! 2
        in return $ Linear.V3 a0 a1 a2
    | otherwise = fail "bad array size"

fromVectorToVec4 :: UV.Vector Float -> Aeson.Parser Hree.Vec4
fromVectorToVec4 v
    | UV.length v == 4 =
        let a0 = v UV.! 0
            a1 = v UV.! 1
            a2 = v UV.! 2
            a3 = v UV.! 3
        in return $ Linear.V4 a0 a1 a2 a3
    | otherwise = fail "bad array size"

fromVectorToQuaternion :: UV.Vector Float -> Aeson.Parser Hree.Quaternion
fromVectorToQuaternion v
    | UV.length v == 4 =
        let a0 = v UV.! 3
            a1 = v UV.! 0
            a2 = v UV.! 1
            a3 = v UV.! 2
        in return $ Linear.Quaternion a0 (Linear.V3 a1 a2 a3)
    | otherwise = fail "bad array size"

fromVectorToMat4 :: UV.Vector Float -> Aeson.Parser Hree.Mat4
fromVectorToMat4 v
    | UV.length v == 16 =
        let a00 = v UV.! 0
            a10 = v UV.! 1
            a20 = v UV.! 2
            a30 = v UV.! 3
            a01 = v UV.! 4
            a11 = v UV.! 5
            a21 = v UV.! 6
            a31 = v UV.! 7
            a02 = v UV.! 8
            a12 = v UV.! 9
            a22 = v UV.! 10
            a32 = v UV.! 11
            a03 = v UV.! 12
            a13 = v UV.! 13
            a23 = v UV.! 14
            a33 = v UV.! 15
            r0 = Linear.V4 a00 a01 a02 a03
            r1 = Linear.V4 a10 a11 a12 a13
            r2 = Linear.V4 a20 a21 a22 a23
            r3 = Linear.V4 a30 a31 a32 a33
        in return $ Linear.V4 r0 r1 r2 r3
    | otherwise = fail "bad array size"

loadGLTFFile :: FilePath -> IO GLTF
loadGLTFFile filepath = either (throwIO . userError) return =<< Aeson.eitherDecodeFileStrict' filepath

loadSceneFromGLTF :: FilePath -> GLTF -> Hree.Scene -> IO Supplement
loadSceneFromGLTF = loadSceneFromGLTFInternal Nothing

loadSceneFromGLTFInternal :: Maybe ByteString -> FilePath -> GLTF -> Hree.Scene -> IO Supplement
loadSceneFromGLTFInternal maybeBuffer0 basepath gltf scene = do
    let buffers_ = gltfBuffers gltf
        bufferViews_ = gltfBufferViews gltf
        accessors_ = gltfAccessors gltf
        images_ = gltfImages gltf
        samplers_ = gltfSamplers gltf
        textures_ = gltfTextures gltf
        materials_ = gltfMaterials gltf
        meshes_ = gltfMeshes gltf
        nodes_ = gltfNodes gltf
        scenes_ = gltfScenes gltf
        animations_ = gltfAnimations gltf
        skins_ = gltfSkins gltf
        rootNodes = maybe mempty sceneNodes $ scenes_ BV.!? 0
    defaultTexture <- Hree.mkDefaultTextureIfNotExists scene
    bufferAndBss <- createBuffers maybeBuffer0 basepath scene buffers_
    let (buffers, bss) = BV.unzip bufferAndBss
    sources <- createTextureSources basepath scene bss bufferViews_ images_
    samplers <- createSamplers scene samplers_
    let textures = createTextures defaultTexture sources samplers textures_
        materials = createMaterials textures materials_
    nodeIds <- createNodes scene nodes_ rootNodes
    skinIds <- createSkins scene nodes_ nodeIds bss bufferViews_ accessors_ skins_
    meshes <- createGLTFMeshes buffers bufferViews_ accessors_ materials meshes_
    animations <- createAnimations nodeIds bss bufferViews_ accessors_ animations_
    BV.imapM_ (createMeshNodes scene nodeIds meshes skinIds) nodes_
    let sup = Supplement nodeIds animations
    return sup

loadSceneFromFile :: FilePath -> Hree.Scene -> IO (GLTF, Supplement)
loadSceneFromFile path scene
    | takeExtension path == ".glb" = loadSceneFromGLBFile path scene
    | otherwise = do
        gltf <- loadGLTFFile path
        sup <- loadSceneFromGLTF (dropFileName path) gltf scene
        return (gltf, sup)

loadSceneFromGLBFile :: FilePath -> Hree.Scene -> IO (GLTF, Supplement)
loadSceneFromGLBFile path scene = do
    bs <- ByteString.readFile path
    loadSceneFromGLBBin (dropFileName path) bs scene

loadSceneFromGLBBin :: FilePath -> ByteString -> Hree.Scene -> IO (GLTF, Supplement)
loadSceneFromGLBBin basepath bs scene = do
    GLBHeader _ _ totalLen <- parseGLBHeader bs
    (gltf, chunk0Len) <- parseGLBChunk0 bs glbHeaderLen
    let hasChunk1 = fromIntegral totalLen > (glbHeaderLen + chunkHeaderLen + fromIntegral chunk0Len)
    maybeBuffer0 <- if hasChunk1
        then Just <$> parseGLBChunk1 bs (glbHeaderLen + chunkHeaderLen + fromIntegral chunk0Len)
        else return Nothing
    sup <- loadSceneFromGLTFInternal maybeBuffer0 basepath gltf scene
    return (gltf, sup)
    where
    glbHeaderLen = 12
    chunkHeaderLen = 8
    parseGLBHeader a = do
        let header = ByteString.take 12 a
        ByteString.useAsCString header $ \p -> do
            magic <- Foreign.peekByteOff p 0
            version <- Foreign.peekByteOff p 4
            len <- Foreign.peekByteOff p 8
            unless (magic == 0x46546C67) . throwIO . userError $ "glb magic mismatch. magic=" ++ showHex magic ""
            unless (version == 2) . throwIO . userError $ "unknown gltf version. version=" ++ show version
            return $ GLBHeader magic version len
    parseGLBChunk0 a offset = do
        ChunkHeader len _ <- parseChunkHeader a offset 0x4E4F534A
        let body = ByteString.take (fromIntegral len) . ByteString.drop (offset + 8) $ a
        gltf <- either (throwIO . userError) return $ Aeson.eitherDecodeStrict' body
        return (gltf, len)
    parseGLBChunk1 a offset = do
        ChunkHeader len _ <- parseChunkHeader a offset 0x004E4942
        return . ByteString.take (fromIntegral len) . ByteString.drop (offset + 8) $ a

    parseChunkHeader a offset expectedChunkType = do
        let header = ByteString.take 8 . ByteString.drop offset $ a
        ByteString.useAsCString header $ \p -> do
            len <- Foreign.peekByteOff p 0
            chunkType <- Foreign.peekByteOff p 4
            unless (chunkType == expectedChunkType) . throwIO . userError $
                "chunk type mismatch. chunkType=" ++ showHex chunkType ""
            return $ ChunkHeader len chunkType

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
        _ -> do
            path <- canonicalizePath $ cd </> ByteString.unpack uri
            bs <- ByteString.readFile path
            return $ ByteString.take byteLength bs
    where
    (scheme, remainder) = ByteString.break (== ':') uri

createBuffer :: FilePath -> Hree.Scene -> Buffer -> IO (GLW.Buffer, ByteString)
createBuffer cd scene (Buffer byteLength (Just uri)) = do
    bs <- parseUri cd byteLength (Text.encodeUtf8 uri)
    buffer <- Hree.addBuffer scene (Hree.BufferSourceByteString bs GL.GL_STATIC_READ)
    return (buffer, bs)
createBuffer _ _ (Buffer _ Nothing) =
    throwIO . userError $ "createBuffer failed because data uri is unknown"

createBuffers :: Maybe ByteString -> FilePath -> Hree.Scene -> BV.Vector Buffer -> IO (BV.Vector (GLW.Buffer, ByteString))
createBuffers Nothing cd scene buffers = BV.mapM (createBuffer cd scene) buffers
createBuffers (Just bs0) cd scene buffers = do
    buffer <- Hree.addBuffer scene (Hree.BufferSourceByteString bs0 GL.GL_STATIC_READ)
    BV.cons (buffer, bs0) <$> BV.mapM (createBuffer cd scene) (BV.drop 1 buffers)

createGLTFMeshes :: BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Hree.StandardMaterial -> BV.Vector Mesh -> IO (BV.Vector (BV.Vector (Either (Hree.Mesh Hree.FlatColorMaterial) (Hree.Mesh Hree.StandardMaterial))))
createGLTFMeshes buffers bufferViews accessors materials =
    BV.mapM (createMeshes buffers bufferViews accessors materials)

createMeshes :: BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Hree.StandardMaterial -> Mesh -> IO (BV.Vector (Either (Hree.Mesh Hree.FlatColorMaterial) (Hree.Mesh Hree.StandardMaterial)))
createMeshes buffers bufferViews accessors materials =
    BV.mapM (createMeshFromPrimitive buffers bufferViews accessors materials) . meshPrimitives

createMeshFromPrimitive :: BV.Vector GLW.Buffer -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Hree.StandardMaterial -> Primitive -> IO (Either (Hree.Mesh Hree.FlatColorMaterial) (Hree.Mesh Hree.StandardMaterial))
createMeshFromPrimitive buffers bufferViews accessors materials primitive = do
    attributes <- either (throwIO . userError) return . mapM f . Map.toList . primitiveAttributes $ primitive
    let (_, geometry) = foldl addAttribBinding (1, Hree.newGeometry) attributes
        verticesCount = minimum . map (accessorCount . snd . snd) $ attributes
        geometry1 = geometry { Hree.geometryVerticesCount = verticesCount }

    geometry2 <- flip (maybe (return geometry1)) (primitiveIndices primitive) $
        either error return . setIndexBuffer geometry1 buffers bufferViews accessors

    let defaultMaterial = Hree.flatColorMaterial (Linear.V4 0.5 0.5 0.5 1)
        mesh = maybe (Left $ Hree.Mesh geometry2 defaultMaterial Nothing) Right $ do
            materialIndex <- primitiveMaterial primitive
            material <- materials BV.!? materialIndex
            return $ Hree.Mesh geometry2 material Nothing
    return mesh
    where
    f (key, aid) = do
        accessor <- maybe (Left $ "invalid accessor identifier: " ++ show aid) return $ accessors BV.!? aid
        let bvid = accessorBufferView accessor
        bufferView <- maybe (Left $ "invalid bufferView identifier: " ++ show bvid) return $ bufferViews BV.!? bvid
        let bid = bufferViewBuffer bufferView
        buffer <- maybe (Left $ "invalid buffer identifier: " ++ show bid) return $ buffers BV.!? bid
        return ((bufferView, buffer), (key, accessor))

addAttribBinding :: (Int, Hree.Geometry) -> ((BufferView, GLW.Buffer), (Text, Accessor)) -> (Int, Hree.Geometry)
addAttribBinding (bindingIndex, geometry0) ((bufferView, buffer), (attribKey, accessor)) =
    let geometry1 = Hree.addAttribBindings geometry0 bindingIndex attribFormat (buffer, bbs)
    in (bindingIndex + 1, geometry1)
    where
    offset = bufferViewByteOffset bufferView + accessorByteOffset accessor
    stride = fromMaybe (calcStrideFromAccessors [accessor]) $ bufferViewByteStride bufferView
    bbs = Hree.BindBufferSetting offset stride 0
    attribFormat = Map.singleton (convertAttribName attribKey) (accessorToAttributeFormat accessor)

accessorToAttributeFormat :: Accessor -> Hree.AttributeFormat
accessorToAttributeFormat accessor =
    if isIntegralType componentType
        then Hree.attribIFormat num formatComponentType 0
        else Hree.attribFormat num formatComponentType normalized 0
    where
    componentType = accessorComponentType accessor
    valueType = accessorType accessor
    normalized = accessorNormalized accessor
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
    | name == "JOINTS_0" = "jointIndices"
    | name == "WEIGHTS_0" = "jointWeights"
    | otherwise = "unknown"

calcStrideFromAccessors :: [Accessor] -> Int
calcStrideFromAccessors = sum . map accessorByteStride

accessorByteStride :: Accessor -> Int
accessorByteStride accessor = num * componentByteSize componentType
    where
    componentType = accessorComponentType accessor
    valueType = accessorType accessor
    num = numberOfComponent valueType

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
        offset = bufferViewByteOffset bufferView + accessorByteOffset accessor
    return $ Hree.IndexBuffer buffer dataType (fromIntegral count) offset

convertToIndexBufferDataType :: ComponentType -> Either String GL.GLenum
convertToIndexBufferDataType UnsignedByte' = return GL.GL_UNSIGNED_BYTE
convertToIndexBufferDataType UnsignedShort' = return GL.GL_UNSIGNED_SHORT
convertToIndexBufferDataType UnsignedInt' = return GL.GL_UNSIGNED_INT
convertToIndexBufferDataType a = fail $ "invalid componentType: " ++ show a

createNodes :: Hree.Scene -> BV.Vector Node -> BV.Vector Int -> IO (BV.Vector Hree.NodeId)
createNodes scene nodes rootNodes = do
    nodeIds <- BV.mapM (createNode scene) nodes
    BV.imapM_ (setNodeChildren scene nodeIds) nodes
    let rootNodeIds = BV.map (nodeIds BV.!) rootNodes
    Hree.addRootNodes scene rootNodeIds
    return nodeIds

createNode :: Hree.Scene -> Node -> IO Hree.NodeId
createNode scene a = do
    Hree.addNode scene node False
    where
    Hree.Transform translation rotation scale =
        case nodeMatrix a of
            Just mat -> Hree.matrixToTransform mat
            Nothing ->
                let t = fromMaybe Linear.zero (nodeTranslation a)
                    r = fromMaybe (Linear.Quaternion 1 (Linear.V3 0 0 0)) (nodeRotation a)
                    s = fromMaybe (Linear.V3 1 1 1) (nodeScale a)
                in Hree.Transform t r s
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

createMeshNodes :: Hree.Scene -> BV.Vector Hree.NodeId -> BV.Vector (BV.Vector (Either (Hree.Mesh Hree.FlatColorMaterial) (Hree.Mesh Hree.StandardMaterial))) -> BV.Vector Hree.SkinId -> Int -> Node -> IO ()
createMeshNodes scene nodeIds meshsets skinIds i a =
    whenJust (nodeMesh a) $ \j -> do
        let nodeId = nodeIds BV.! i
            meshes = meshsets BV.! j
            maybeSkinId = (skinIds BV.!) <$> nodeSkin a
        meshIds <- case maybeSkinId of
                    Just skinId -> BV.mapM (addSkinnedMesh skinId) meshes
                    Nothing     -> BV.mapM addMesh meshes
        children <- BV.mapM (createMeshNode scene) meshIds
        void . Hree.updateNode scene nodeId $ \node ->
            node { Hree.nodeChildren = Hree.nodeChildren node `mappend` children }
    where
    addSkinnedMesh skinId (Left mesh) = Hree.addedMeshId <$> Hree.addSkinnedMesh scene mesh skinId
    addSkinnedMesh skinId (Right mesh) = Hree.addedMeshId <$> Hree.addSkinnedMesh scene mesh skinId
    addMesh (Left mesh)  = Hree.addedMeshId <$> Hree.addMesh scene mesh
    addMesh (Right mesh) = Hree.addedMeshId <$> Hree.addMesh scene mesh

createMeshNode :: Hree.Scene -> Hree.MeshId -> IO Hree.NodeId
createMeshNode scene meshId =
    Hree.addNode scene node False
    where
    node = Hree.newNode { Hree.nodeMesh = Just meshId }

createSkins :: Hree.Scene -> BV.Vector Node -> BV.Vector Hree.NodeId -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Skin -> IO (BV.Vector Hree.SkinId)
createSkins scene nodes nodeIds buffers bufferViews accessors =
    mapM (createSkin scene nodes nodeIds buffers bufferViews accessors)

createSkin :: Hree.Scene -> BV.Vector Node -> BV.Vector Hree.NodeId -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Skin -> IO Hree.SkinId
createSkin scene nodes nodeIds buffers bufferViews accessors skin = do
    let joints = skinJoints skin
    invMats <- maybe (return SV.empty) (createMat4VectorFromBuffer SV.generateM buffers bufferViews accessors) $ skinInverseBindMatrices skin
    skeleton <- maybe (searchCommonRoot nodes joints) return . skinSkeleton $ skin
    let skeletonNodeId = nodeIds BV.! skeleton
        jointNodeIds = SV.generate (BV.length joints) ((nodeIds BV.!) . (joints BV.!))
    Hree.addSkin scene skeletonNodeId jointNodeIds invMats

createAnimations :: BV.Vector Hree.NodeId -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector Animation -> IO (BV.Vector Hree.AnimationClip)
createAnimations nodeIds buffers bufferViews accessors = BV.mapM (createAnimation nodeIds buffers bufferViews accessors)

createAnimation :: BV.Vector Hree.NodeId -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Animation -> IO Hree.AnimationClip
createAnimation nodeIds buffers bufferViews accessors animation = do
    channels <- BV.mapM (createAnimationChannel nodeIds buffers bufferViews accessors (animationSamplers animation)) (animationChannels animation)
    return $ Hree.animationClipTransform channels

createAnimationChannel :: BV.Vector Hree.NodeId -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> BV.Vector AnimationSampler -> Channel -> IO Hree.TransformChannel
createAnimationChannel nodeIds buffers bufferViews accessors samplers channel = do
    let AnimationSampler input interpolation output = samplers BV.! channelSampler channel
        ChannelTarget targetNode path = channelTarget channel
        nodeId = nodeIds BV.! targetNode
    keyFrames <- createKeyFrames path interpolation buffers bufferViews accessors input output
    return $ Hree.TransformChannel nodeId (BV.singleton keyFrames)

createKeyFrames :: ChannelTargetPath -> AnimationInterpolation -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> Int -> IO (Hree.KeyFrames Hree.TransformTrack)
createKeyFrames ChannelTargetPathTranslation interpolation buffers bufferViews accessors input output = do
    timePoints <- UV.map (round . (* 1.0E+9)) <$> createFloatVectorFromBuffer UV.generateM buffers bufferViews accessors input
    values <- createVec3VectorFromBuffer UV.generateM buffers bufferViews accessors output
    let track = Hree.TransformTrackTranslation values
        interpolation' = convertInterpolation interpolation
    return $ Hree.KeyFrames interpolation' timePoints track
createKeyFrames ChannelTargetPathRotation interpolation buffers bufferViews accessors input output = do
    timePoints <- UV.map (round . (* 1.0E+9)) <$> createFloatVectorFromBuffer UV.generateM buffers bufferViews accessors input
    values <- createQuaternionVectorFromBuffer UV.generateM buffers bufferViews accessors output
    let track = Hree.TransformTrackRotation values
        interpolation' = convertInterpolation interpolation
    return $ Hree.KeyFrames interpolation' timePoints track
createKeyFrames ChannelTargetPathScale interpolation buffers bufferViews accessors input output = do
    timePoints <- UV.map (round . (* 1.0E+9)) <$> createFloatVectorFromBuffer UV.generateM buffers bufferViews accessors input
    values <- createVec3VectorFromBuffer UV.generateM buffers bufferViews accessors output
    let track = Hree.TransformTrackScale values
        interpolation' = convertInterpolation interpolation
    return $ Hree.KeyFrames interpolation' timePoints track

convertInterpolation :: AnimationInterpolation -> Hree.Interpolation
convertInterpolation AnimationInterpolationLinear = Hree.InterpolationLinear
convertInterpolation AnimationInterpolationStep = Hree.InterpolationStep
convertInterpolation AnimationInterpolationCubicSpline = Hree.InterpolationLinear

createVectorFromBuffer ::
    forall a v.
    Int ->
    (Foreign.Ptr () -> Int -> IO a) ->
    (Int -> (Int -> IO a) -> IO v) ->
    BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> IO v
createVectorFromBuffer minStride peekByteOff generateM buffers bufferViews accessors i = do
    let accessor = accessors BV.! i
        view = bufferViews BV.! accessorBufferView accessor
        buffer = buffers BV.! bufferViewBuffer view
        byteOffset = bufferViewByteOffset view
        byteLen = bufferViewByteLength view
        count = accessorCount accessor
        aoffset = accessorByteOffset accessor
        slice = ByteString.take byteLen . ByteString.drop byteOffset $ buffer
        stride = fromMaybe minStride . bufferViewByteStride $ view
    unless (stride >= minStride) . throwIO . userError $ "bad stride. i: " ++ show i ++ ", minStride: " ++ show minStride ++ ", actual: " ++ show stride
    unless (byteLen >= stride * count) . throwIO . userError $ "bad bufferView byte length"
    ByteString.useAsCString slice $ \ptr ->
        generateM count (\j -> peekByteOff (Foreign.castPtr ptr) (aoffset + stride * j))

createFloatVectorFromBuffer ::
    (Int -> (Int -> IO Float) -> IO v) ->
    BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> IO v
createFloatVectorFromBuffer generateM buffers bufferViews accessors i = do
    let accessor = accessors BV.! i
    unless (accessorType accessor == Scalar) . throwIO . userError $ "bad valueType"
    unless (accessorComponentType accessor == Float') . throwIO . userError $ "bad componentType"
    createVectorFromBuffer 4 Foreign.peekByteOff generateM buffers bufferViews accessors i

createMat4VectorFromBuffer ::
    (Int -> (Int -> IO Hree.Mat4) -> IO v) ->
    BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> IO v
createMat4VectorFromBuffer generateM buffers bufferViews accessors i = do
    let accessor = accessors BV.! i
    unless (accessorType accessor == Mat4) . throwIO . userError $ "bad valueType"
    unless (accessorComponentType accessor == Float') . throwIO . userError $ "bad componentType"
    createVectorFromBuffer 64 ((fmap Linear.transpose .) . Foreign.peekByteOff) generateM buffers bufferViews accessors i

createVec3VectorFromBuffer ::
    (Int -> (Int -> IO Hree.Vec3) -> IO v) ->
    BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> IO v
createVec3VectorFromBuffer generateM buffers bufferViews accessors i = do
    let accessor = accessors BV.! i
    unless (accessorType accessor == Vec3) . throwIO . userError $ "bad valueType"
    unless (accessorComponentType accessor == Float') . throwIO . userError $ "bad componentType"
    createVectorFromBuffer 12 Foreign.peekByteOff generateM buffers bufferViews accessors i

createQuaternionVectorFromBuffer ::
    (Int -> (Int -> IO Hree.Quaternion) -> IO v) ->
    BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Accessor -> Int -> IO v
createQuaternionVectorFromBuffer generateM buffers bufferViews accessors i = do
    let accessor = accessors BV.! i
        valueType = accessorType accessor
        componentType = accessorComponentType accessor
    (peekByteOff, minStride) <- maybe (throwIO . userError $ "bad accessor") return $ mkQuaternionPeekByteOffAndStride valueType componentType
    createVectorFromBuffer minStride peekByteOff generateM buffers bufferViews accessors i

mkQuaternionPeekByteOffAndStride :: ValueType -> ComponentType -> Maybe (Foreign.Ptr () -> Int -> IO (Linear.Quaternion Float), Int)
mkQuaternionPeekByteOffAndStride Vec4 Float' = Just ((fmap f .) . Foreign.peekByteOff, 16)
    where
    f :: Linear.V4 Float -> Linear.Quaternion Float
    f (Linear.V4 x y z w) = Linear.Quaternion w (Linear.V3 x y z)
mkQuaternionPeekByteOffAndStride Vec4 Byte' = Just ((fmap f .) . Foreign.peekByteOff, 4)
    where
    f :: Linear.V4 Int8 -> Linear.Quaternion Float
    f (Linear.V4 x y z w) = Linear.Quaternion (toFloat w) (Linear.V3 (toFloat x) (toFloat y) (toFloat z))
    toFloat a = max (fromIntegral a / 127.0) (-1.0)
mkQuaternionPeekByteOffAndStride Vec4 UnsignedByte' = Just ((fmap f .) . Foreign.peekByteOff, 4)
    where
    f :: Linear.V4 Word8 -> Linear.Quaternion Float
    f (Linear.V4 x y z w) = Linear.Quaternion (toFloat w) (Linear.V3 (toFloat x) (toFloat y) (toFloat z))
    toFloat a = max (fromIntegral a / 256.0) (-1.0)
mkQuaternionPeekByteOffAndStride Vec4 Short' = Just ((fmap f .) . Foreign.peekByteOff, 8)
    where
    f :: Linear.V4 Int16 -> Linear.Quaternion Float
    f (Linear.V4 x y z w) = Linear.Quaternion (toFloat w) (Linear.V3 (toFloat x) (toFloat y) (toFloat z))
    toFloat a = max (fromIntegral a / 32767.0) (-1.0)
mkQuaternionPeekByteOffAndStride Vec4 UnsignedShort' = Just ((fmap f .) . Foreign.peekByteOff, 8)
    where
    f :: Linear.V4 Word16 -> Linear.Quaternion Float
    f (Linear.V4 x y z w) = Linear.Quaternion (toFloat w) (Linear.V3 (toFloat x) (toFloat y) (toFloat z))
    toFloat a = max (fromIntegral a / 65535.0) (-1.0)
mkQuaternionPeekByteOffAndStride _ _ = Nothing

searchCommonRoot :: BV.Vector Node -> BV.Vector Int -> IO Int
searchCommonRoot nodes joints = do
    let nodeCount = BV.length nodes
        joint0 = BV.head joints
        jointSet = IntSet.fromList . BV.toList $ joints
    parents <- MUV.replicate nodeCount (-1)
    BV.imapM_ (\i n -> BV.mapM (flip (MUV.write parents) i) (nodeChildren n)) nodes

    root <- flip searchRoot joint0 =<< UV.unsafeFreeze parents

    descendantSearched <- MUV.replicate nodeCount False
    descendants <- MBV.replicate nodeCount IntSet.empty

    _ <- searchDescendants descendantSearched descendants root

    parentsFreezed <- UV.unsafeFreeze parents
    descendantsFreezed <- BV.unsafeFreeze descendants
    searchCommon parentsFreezed descendantsFreezed jointSet joint0

    where
    searchRoot parents i =
        if parents UV.! i < 0
            then return i
            else searchRoot parents (parents UV.! i)

    searchDescendants descendantSearched memo i = do
        let node = nodes BV.! i
        searched <- MUV.read descendantSearched i
        if searched
            then MBV.read memo i
            else do
                let childrenSet = IntSet.fromList . BV.toList $ nodeChildren node
                ds <- BV.foldM' (\a b -> IntSet.union a <$> searchDescendants descendantSearched memo b) childrenSet . nodeChildren $ node
                MUV.write descendantSearched i True
                MBV.write memo i ds
                return ds

    searchCommon parents descendants jointSet i
        | i < 0 = throwIO . userError $ "common root not found"
        | IntSet.member i jointSet = searchCommon parents descendants jointSet (parents UV.! i)
        | IntSet.isSubsetOf jointSet (descendants BV.! i) = return i
        | otherwise = searchCommon parents descendants jointSet (parents UV.! i)

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
            len = bufferViewByteLength bufferView
        buffer <- maybe (throwIO . userError $ "invalid buffer identifier: " ++ show bid) return $ buffers BV.!? bid
        return . ByteString.take len . ByteString.drop offset $ buffer

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

createTextureSource :: FilePath -> Hree.Scene -> BV.Vector ByteString -> BV.Vector BufferView -> Image -> IO (GLW.Texture 'GLW.GL_TEXTURE_2D, GLW.Sampler)
createTextureSource cd scene buffers bufferViews image = do
    source <- createImage cd buffers bufferViews image
    let name = Text.encodeUtf8 $ fromMaybe "glTF_texture_" (imageName image)
        width = fromIntegral $ Picture.imageWidth source
        height = fromIntegral $ Picture.imageHeight source
        settings = Hree.TextureSettings 1 GL.GL_RGBA8 width height False
    (_, texture) <- SV.unsafeWith (Picture.imageData source) $ \ptr -> do
        let sourceData = Hree.TextureSourceData width height PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr ptr)
        Hree.addTexture scene name settings sourceData
    let sname = Text.encodeUtf8 $ fromMaybe "glTF_sourcesampler_" (imageName image)
    (_, sampler) <- Hree.addSampler scene sname
    return (texture, sampler)

createTextureSources :: FilePath -> Hree.Scene -> BV.Vector ByteString -> BV.Vector BufferView -> BV.Vector Image -> IO (BV.Vector (GLW.Texture 'GLW.GL_TEXTURE_2D, GLW.Sampler))
createTextureSources cd scene buffers bufferViews =
    BV.mapM (createTextureSource cd scene buffers bufferViews)

createSampler :: Hree.Scene -> Sampler -> IO GLW.Sampler
createSampler scene sampler = do
    let name = Text.encodeUtf8 $ fromMaybe "glTF_sampler_" (samplerName sampler)
    (_, a) <- Hree.addSampler scene name
    whenJust (samplerMagFilter sampler >>= unmarshalSamplerFilterParameter) $
        Hree.Sampler.setSamplerParameter a Hree.Sampler.glTextureMagFilter
    whenJust (samplerMinFilter sampler >>= unmarshalSamplerFilterParameter) $
        Hree.Sampler.setSamplerParameter a Hree.Sampler.glTextureMinFilter
    whenJust (unmarshalSamplerWrapParameter . samplerWrapS $ sampler) $
        Hree.Sampler.setSamplerParameter a Hree.Sampler.glTextureWrapS
    whenJust (unmarshalSamplerWrapParameter . samplerWrapT $ sampler) $
        Hree.Sampler.setSamplerParameter a Hree.Sampler.glTextureWrapT
    return a

createSamplers :: Hree.Scene -> BV.Vector Sampler -> IO (BV.Vector GLW.Sampler)
createSamplers scene = BV.mapM (createSampler scene)

createTexture :: BV.Vector (GLW.Texture 'GLW.GL_TEXTURE_2D, GLW.Sampler) -> BV.Vector GLW.Sampler -> Texture -> Maybe Hree.Texture
createTexture sources samplers texture = do
    sourceIndex <- textureSource texture
    (source, sourceSampler) <- sources BV.!? sourceIndex
    let sampler = fromMaybe sourceSampler $ (samplers BV.!?) =<< textureSampler texture
    return (Hree.Texture (source, sampler))

createTextures :: Hree.Texture -> BV.Vector (GLW.Texture 'GLW.GL_TEXTURE_2D, GLW.Sampler) -> BV.Vector GLW.Sampler -> BV.Vector Texture -> BV.Vector Hree.Texture
createTextures defaultTexture sources samplers = BV.map (fromMaybe defaultTexture . createTexture sources samplers)

createMaterial :: BV.Vector Hree.Texture -> Material -> Hree.StandardMaterial
createMaterial textures m =
    Hree.standardMaterial Hree.standardMaterialBlock
        & setWhenJust setPbrMetallicRoughness (materialPbrMetallicRoughness m)
        & setWhenJust setNormalTexture (materialNormalTexture m)
    where
    setWhenJust _ Nothing a  = a
    setWhenJust f (Just b) a = a `f` b
    setPbrMetallicRoughness a pbr = a
        { Hree.uniformBlock = (Hree.uniformBlock a)
            { Hree.baseColorFactor = pbrBaseColorFactor pbr
            , Hree.metallicFactor = pbrMetallicFactor pbr
            , Hree.roughnessFactor = pbrRoughnessFactor pbr
            }
        }
        & setWhenJust setBaseColorTexture (pbrBaseColorTexture pbr)
        & setWhenJust setMetallicRoughnessTexture (pbrMetallicRoughnessTexture pbr)
    setBaseColorTexture a info = fromMaybe a $ do
        texture <- textures BV.!? textureInfoIndex info
        return $ a { Hree.baseColorTexture = Just texture }
    setNormalTexture a info = fromMaybe a $ do
        texture <- textures BV.!? normalTextureInfoIndex info
        return $ a { Hree.normalTexture = Just texture }
    setMetallicRoughnessTexture a info = fromMaybe a $ do
        texture <- textures BV.!? textureInfoIndex info
        return $ a { Hree.metallicRoughnessTexture = Just texture }

createMaterials :: BV.Vector Hree.Texture -> BV.Vector Material -> BV.Vector Hree.StandardMaterial
createMaterials textures = BV.map (createMaterial textures)

unmarshalSamplerFilterParameter :: Int -> Maybe GL.GLint
unmarshalSamplerFilterParameter a
    | a == 9728 = Just GL.GL_NEAREST
    | a == 9729 = Just GL.GL_LINEAR
    | a == 9984 = Just GL.GL_NEAREST_MIPMAP_NEAREST
    | a == 9985 = Just GL.GL_LINEAR_MIPMAP_NEAREST
    | a == 9986 = Just GL.GL_NEAREST_MIPMAP_LINEAR
    | a == 9987 = Just GL.GL_LINEAR_MIPMAP_LINEAR
    | otherwise = Nothing

unmarshalSamplerWrapParameter :: Int -> Maybe GL.GLint
unmarshalSamplerWrapParameter a
    | a == 33071 = Just GL.GL_CLAMP_TO_EDGE
    | a == 33648 = Just GL.GL_MIRRORED_REPEAT
    | a == 10497 = Just GL.GL_REPEAT
    | otherwise = Nothing

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = return ()
whenJust (Just a) f = f a
