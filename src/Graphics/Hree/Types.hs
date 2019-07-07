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
import Data.Hashable (Hashable(..))
import Data.IntMap.Strict (IntMap)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as MBV
import qualified Data.Vector.Storable.Mutable as MSV
import Foreign (Storable)
import qualified GLW
import qualified Graphics.Hree.Component as Component
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
    { materialUniforms         :: !(Map ByteString Uniform)
    , materialTextures         :: ![Texture]
    , materialProgramSpecifier :: !ProgramSpec
    } deriving (Show)

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
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GLW.VertexArray
    } deriving (Show)

newtype NodeId = NodeId
    { unNodeId :: Int
    } deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

data Node = Node
    { nodeName        :: !(Maybe ByteString)
    , nodeMesh        :: !(Maybe MeshId)
    , nodeTranslation :: !Vec3
    , nodeRotation    :: !Quaternion
    , nodeScale       :: !Vec3
    } deriving (Show, Eq)

data NodeInfo = NodeInfo
    { nodeInfoId   :: !NodeId
    , nodeInfoNode :: !Node
    } deriving (Show, Eq)

data Scene = Scene
    { sceneState                    :: !(IORef SceneState)
    , sceneMeshStore                :: !(Component.ComponentStore MBV.MVector MeshInfo)
    , sceneNodeStore                :: !(Component.ComponentStore MBV.MVector NodeInfo)
    , sceneNodeTransformStore       :: !(Component.ComponentStore MSV.MVector Transform)
    , sceneNodeTransformMatrixStore :: !(Component.ComponentStore MSV.MVector Mat4)
    }

data SceneState = SceneState
    { ssMeshCounter    :: !MeshId
    , ssNodeCounter    :: !NodeId
    , ssRootNodes      :: !(BV.Vector NodeId)
    , ssBuffers        :: ![GLW.Buffer]
    , ssTextures       :: !(Map ByteString (GLW.Texture 'GLW.GL_TEXTURE_2D))
    , ssSamplers       :: !(Map ByteString GLW.Sampler)
    , ssDefaultTexture :: !(Maybe Texture)
    , ssPrograms       :: !(Map ProgramSpec ProgramInfo)
    }
