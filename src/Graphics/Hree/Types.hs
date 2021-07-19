{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.Types
    ( ClearOption(..)
    , Geometry(..)
    , GeometryInfo(..)
    , LightId(..)
    , LightStore
    , MappingSource(..)
    , Material(..)
    , MaterialId(..)
    , MaterialInfo(..)
    , MatricesBlockBinder(..)
    , Mesh(..)
    , MeshId(..)
    , MeshInfo(..)
    , MeshMaterial(..)
    , NodeId(..)
    , Node(..)
    , NodeInfo(..)
    , Renderer(..)
    , RendererOption(..)
    , RendererState(..)
    , Scene(..)
    , SceneState(..)
    , Skin(..)
    , SkinId(..)
    , SamplerParamValue(..)
    , TextureMappingType(..)
    , TextureSettings(..)
    , TextureSourceData(..)
    , TransformInfo(..)
    ) where

import Chronos (Time)
import Data.ByteString (ByteString)
import qualified Data.Component as Component
import Data.Hashable (Hashable(..), hashUsing)
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as MBV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import Data.Word (Word8)
import Foreign (ForeignPtr, Storable(..), castPtr, plusPtr)
import GHC.TypeNats (KnownNat)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.GL.Block (Elem)
import Graphics.Hree.GL.Sampler (SamplerParamValue(..))
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.UniformBlock
import Graphics.Hree.Light
import Graphics.Hree.Math
import Graphics.Hree.Program
import Linear (V4(..))

data Geometry = Geometry
    { geometryAttribBindings    :: !(Map ByteString AttribBinding)
    , geometryBufferSources     :: !(IntMap (BufferSource, BindBufferSetting))
    , geometryIndexBufferSource :: !(Maybe IndexBufferSource)
    , geometryVerticesCount     :: !Int
    } deriving (Show)

data GeometryInfo = GeometryInfo
    { geometryInfoAttribBindings :: !(Map ByteString AttribBinding)
    , geometryInfoBuffers        :: !(IntMap (GLW.Buffer, BindBufferSetting))
    , geometryInfoIndexBuffer    :: !(Maybe IndexBuffer)
    , geometryInfoVerticesCount  :: !Int
    } deriving (Show)

data TextureMappingType =
    BaseColorMapping |
    NormalMapping |
    EmissiveMapping |
    MetallicRoughnessMapping |
    OcclusionMapping
    deriving (Show, Eq, Enum, Ord)

instance Hashable TextureMappingType where
    hashWithSalt = hashUsing fromEnum

data TextureSettings = TextureSettings
    { textureLevels         :: !GL.GLint
    , textureInternalFormat :: !GL.GLenum
    , textureWidth          :: !GL.GLsizei
    , textureHeight         :: !GL.GLsizei
    , textureGenerateMipmap :: !Bool
    } deriving (Show, Eq)

data TextureSourceData = TextureSourceData
    { sourceWidth    :: !GL.GLsizei
    , sourceHeight   :: !GL.GLsizei
    , sourceFormat   :: !GLW.PixelFormat
    , sourceDataType :: !GL.GLenum
    , sourcePixels   :: !(ForeignPtr ())
    }

data MappingSource = MappingSource
    { mappingSourceTextureSettings    :: !TextureSettings
    , mappingSourceTextoureSourceData :: !TextureSourceData
    , mappingSourceSamplerParamValues :: ![SamplerParamValue]
    }

data Material a = Material
    { materialUniformBlock  :: !a
    , materialMappings      :: ![(TextureMappingType, MappingSource)]
    , materialRenderOption  :: !PartialRenderOption
    , materialProgramOption :: !PartialProgramOption
    , materialProgramSpec   :: !ProgramSpec
    }

data MaterialInfo = MaterialInfo
    { materialInfoId            :: !(MaterialId ())
    , materialInfoUniformBlock  :: !(SV.Vector Word8)
    , materialInfoMappings      :: !(Map TextureMappingType TextureAndSampler)
    , materialInfoRenderOption  :: !RenderOption
    , materialInfoProgramOption :: !PartialProgramOption
    , materialInfoProgramSpec   :: !ProgramSpec
    } deriving (Show, Eq)

data MeshMaterial = MeshMaterial
    { meshMaterialMaterialId    :: !(MaterialId ())
    , meshMaterialUniformBlock  :: !GLW.Buffer
    , meshMaterialMappings      :: !(Map ByteString TextureAndSampler)
    , meshMaterialRenderOption  :: !RenderOption
    , meshMaterialProgramOption :: !ProgramOption
    , meshMaterialProgramSpec   :: !ProgramSpec
    , meshMaterialProgramName   :: !ProgramName
    } deriving (Show, Eq)

newtype MaterialId a = MaterialId
    { unMaterialId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

newtype LightId = LightId
    { unLightId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

newtype MeshId = MeshId
    { unMeshId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

data Mesh b = Mesh
    { meshGeometry      :: !Geometry
    , meshMaterialId    :: !(MaterialId b)
    , meshInstanceCount :: !(Maybe Int)
    } deriving (Show)

data MeshInfo = MeshInfo
    { meshInfoId            :: !MeshId
    , meshInfoGeometry      :: !GeometryInfo
    , meshInfoMaterial      :: !MeshMaterial
    , meshInfoInstanceCount :: !(Maybe Int)
    , meshInfoSkin          :: !(Maybe SkinId)
    , meshInfoVertexArray   :: !(Maybe GLW.VertexArray)
    } deriving (Show)

newtype NodeId = NodeId
    { unNodeId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

newtype SkinId = SkinId
    { unSkinId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

data Node = Node
    { nodeName              :: !(Maybe Text)
    , nodeMesh              :: !(Maybe MeshId)
    , nodeChildren          :: !(BV.Vector NodeId)
    , nodeTranslation       :: !Vec3
    , nodeRotation          :: !Quaternion
    , nodeScale             :: !Vec3
    , nodeInverseBindMatrix :: !Mat4
    } deriving (Show, Eq)

data NodeInfo = NodeInfo
    { nodeInfoId            :: !NodeId
    , nodeInfoNode          :: !Node
    , nodeInfoUniformBlocks :: ![(UniformBufferBindingIndex, GLW.Buffer)]
    } deriving (Show, Eq)

data TransformInfo = TransformInfo
    { transformInfoTransform :: !Transform
    , transformInfoUpdated   :: !Bool
    , transformInfoSyncedAt  :: !Time
    } deriving (Show, Eq)

data Scene = Scene
    { sceneState                          :: !(IORef SceneState)
    , sceneMaterialStore                  :: !(Component.ComponentStore MBV.MVector (MaterialId ()) MaterialInfo)
    , sceneMeshStore                      :: !(Component.ComponentStore MBV.MVector MeshId MeshInfo)
    , sceneNodeStore                      :: !(Component.ComponentStore MBV.MVector NodeId NodeInfo)
    , sceneNodeTransformStore             :: !(Component.ComponentStore MSV.MVector NodeId TransformInfo)
    , sceneNodeTransformMatrixStore       :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneNodeGlobalTransformMatrixStore :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneLightStore                     :: !LightStore
    , sceneSkinStore                      :: !(Component.ComponentStore MBV.MVector SkinId Skin)
    }

data SceneState = SceneState
    { ssMaterialCounter   :: !(MaterialId ())
    , ssMeshCounter       :: !MeshId
    , ssNodeCounter       :: !NodeId
    , ssLightCounter      :: !LightId
    , ssSkinCounter       :: !SkinId
    , ssRootNodes         :: !(BV.Vector NodeId)
    , ssBuffers           :: ![GLW.Buffer]
    , ssDefaultTexture    :: !(Maybe TextureAndSampler)
    , ssCameraBlockBinder :: !(Maybe (UniformBlockBinder CameraBlock))
    , ssLightBlockBinder  :: !(Maybe (UniformBlockBinder LightBlock))
    } deriving (Show)

data ClearOption = ClearOption
    { clearOptionColor   :: !(Maybe (V4 GL.GLfloat))
    , clearOptionDepth   :: !(Maybe GL.GLdouble)
    , clearOptionStencil :: !(Maybe GL.GLint)
    } deriving (Show, Eq)

data RendererOption = RendererOption
    { rendererOptionAutoClear :: !ClearOption
    } deriving (Show, Eq)

data Renderer = Renderer
    { rendererOption :: !RendererOption
    , rendererState  :: !(IORef RendererState)
    }

data RendererState = RendererState
    { rendererStatePrograms      :: !(Map ProgramName ProgramInfo)
    } deriving (Show)

data MatricesBlockBinder = forall n. KnownNat n => MatricesBlockBinder
    { unMatricesBlockBinder :: !(UniformBlockBinder (LimitedVector n (Elem Mat4)))
    }

data Skin = Skin
    { skinSkeleton                   :: !NodeId
    , skinInverseBindMatrices        :: !(SV.Vector Mat4)
    , skinJoints                     :: !(SV.Vector NodeId)
    , skinJointMatricesBinder        :: !MatricesBlockBinder
    , skinJointInverseMatricesBinder :: !MatricesBlockBinder
    } deriving (Show)

type LightStore = Component.ComponentStore MSV.MVector LightId (Elem LightStruct)

instance Show MatricesBlockBinder where
    show (MatricesBlockBinder a) = "MatricesBlockBinder {" ++ show a ++ "}"

instance Storable TransformInfo where
    sizeOf _ = 56

    alignment _ = 8

    peek ptr = do
        transform <- peek $ castPtr ptr
        updated <- peek $ castPtr ptr `plusPtr` 40
        syncedAt <- peek $ castPtr ptr `plusPtr` 48
        return $ TransformInfo transform updated syncedAt

    poke ptr (TransformInfo transform updated syncedAt) = do
        poke (castPtr ptr) transform
        poke (castPtr ptr `plusPtr` 40) updated
        poke (castPtr ptr `plusPtr` 48) syncedAt
