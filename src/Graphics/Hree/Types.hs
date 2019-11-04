{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.Types
    ( Geometry(..)
    , Material(..)
    , Mesh(..)
    , MeshId(..)
    , MeshInfo(..)
    , NodeId(..)
    , Node(..)
    , NodeInfo(..)
    , Scene(..)
    , SceneState(..)
    ) where

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
import Foreign (Storable)
import qualified GLW
import Graphics.Hree.GL.Types
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
    , materialTextures    :: ![Texture]
    , materialProgramSpec :: !(Options -> ProgramSpec)
    }

instance Show Material where
    show (Material us ts _) = "Material { materialUniforms = " ++ show us ++ ", materialTextures = " ++ show ts ++ ", materialProgramSpec = Function }"

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
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !(Either ProgramSpec ProgramInfo)
    , meshInfoVertexArray :: !GLW.VertexArray
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
    , nodeSkin              :: !(Maybe SkinId)
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

data Scene = Scene
    { sceneState                          :: !(IORef SceneState)
    , sceneMeshStore                      :: !(Component.ComponentStore MBV.MVector MeshId MeshInfo)
    , sceneNodeStore                      :: !(Component.ComponentStore MBV.MVector NodeId NodeInfo)
    , sceneNodeTransformStore             :: !(Component.ComponentStore MSV.MVector NodeId Transform)
    , sceneNodeTransformMatrixStore       :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneNodeGlobalTransformMatrixStore :: !(Component.ComponentStore MSV.MVector NodeId Mat4)
    , sceneSkinStore                      :: !(Component.ComponentStore MBV.MVector SkinId Skin)
    }

data SceneState = SceneState
    { ssMeshCounter    :: !MeshId
    , ssNodeCounter    :: !NodeId
    , ssRootNodes      :: !(BV.Vector NodeId)
    , ssBuffers        :: ![GLW.Buffer]
    , ssTextures       :: !(Map ByteString (GLW.Texture 'GLW.GL_TEXTURE_2D))
    , ssSamplers       :: !(Map ByteString GLW.Sampler)
    , ssDefaultTexture :: !(Maybe Texture)
    , ssPrograms       :: !(Map ProgramName ProgramInfo)
    } deriving (Show)

data Skin = Skin
    { skinInverseBindMatrices :: !(SV.Vector Mat4)
    , skinJoints              :: !(SV.Vector NodeId)
    } deriving (Show, Eq)
