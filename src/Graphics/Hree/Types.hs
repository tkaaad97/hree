{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.Types
    ( Geometry(..)
    , LightId(..)
    , LightStore
    , Material(..)
    , MatricesBlockBinder(..)
    , Mesh(..)
    , MeshId(..)
    , MeshInfo(..)
    , NodeId(..)
    , Node(..)
    , NodeInfo(..)
    , Scene(..)
    , SceneState(..)
    , Skin(..)
    , SkinId(..)
    , TransformInfo(..)
    ) where

import Chronos (Time)
import Data.ByteString (ByteString)
import qualified Data.Component as Component
import Data.Hashable (Hashable(..))
import Data.IntMap.Strict (IntMap)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as MBV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import Foreign (Storable(..), castPtr, plusPtr)
import GHC.TypeNats (KnownNat)
import qualified GLW
import Graphics.Hree.Camera
import Graphics.Hree.GL.Block (Elem)
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.UniformBlock
import Graphics.Hree.Light
import Graphics.Hree.Math
import Graphics.Hree.Program

data Geometry = Geometry
    { geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBuffers        :: !(IntMap (GLW.Buffer, BindBufferSetting))
    , geometryIndexBuffer    :: !(Maybe IndexBuffer)
    , geometryVerticesCount  :: !Int
    } deriving (Show)

data Material = Material
    { materialUniforms    :: !(Map ByteString Uniform)
    , materialTextures    :: !(Map ByteString Texture)
    , materialProgramSpec :: !(Options -> ProgramSpec)
    }

instance Show Material where
    show (Material us ts _) = "Material { materialUniforms = " ++ show us ++ ", materialTextures = " ++ show ts ++ ", materialProgramSpec = Function }"

newtype LightId = LightId
    { unLightId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

newtype MeshId = MeshId
    { unMeshId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

data Mesh = Mesh
    { meshGeometry      :: !Geometry
    , meshMaterial      :: !Material
    , meshInstanceCount :: !(Maybe Int)
    } deriving (Show)

data MeshInfo = MeshInfo
    { meshInfoId          :: !MeshId
    , meshInfoMesh        :: !Mesh
    , meshInfoSkin        :: !(Maybe SkinId)
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !(ProgramSpec, ProgramName)
    , meshInfoVertexArray :: !(Maybe GLW.VertexArray)
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
    { nodeInfoId   :: !NodeId
    , nodeInfoNode :: !Node
    } deriving (Show, Eq)

data TransformInfo = TransformInfo
    { transformInfoTransform :: !Transform
    , transformInfoUpdated   :: !Bool
    , transformInfoSyncedAt  :: !Time
    } deriving (Show, Eq)

data Scene = Scene
    { sceneState                          :: !(IORef SceneState)
    , sceneMeshStore                      :: !(Component.ComponentStore MBV.MVector MeshId MeshInfo)
    , sceneNodeStore                      :: !(Component.ComponentStore MBV.MVector NodeId NodeInfo)
    , sceneNodeTransformStore             :: !(Component.ComponentStore MSV.MVector NodeId TransformInfo)
    , sceneNodeTransformMatrixStore       :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneNodeGlobalTransformMatrixStore :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneLightStore                     :: !LightStore
    , sceneSkinStore                      :: !(Component.ComponentStore MBV.MVector SkinId Skin)
    }

data SceneState = SceneState
    { ssMeshCounter       :: !MeshId
    , ssNodeCounter       :: !NodeId
    , ssLightCounter      :: !LightId
    , ssSkinCounter       :: !SkinId
    , ssRootNodes         :: !(BV.Vector NodeId)
    , ssBuffers           :: ![GLW.Buffer]
    , ssTextures          :: !(Map ByteString (GLW.Texture 'GLW.GL_TEXTURE_2D))
    , ssSamplers          :: !(Map ByteString GLW.Sampler)
    , ssDefaultTexture    :: !(Maybe Texture)
    , ssCameraBlockBinder :: !(Maybe (UniformBlockBinder CameraBlock))
    , ssLightBlockBinder  :: !(Maybe (UniformBlockBinder LightBlock))
    , ssPrograms          :: !(Map ProgramName ProgramInfo)
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
