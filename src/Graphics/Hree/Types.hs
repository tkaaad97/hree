{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.Types
    ( Geometry(..)
    , Material(..)
    , Mesh(..)
    , MeshId(..)
    , MeshInfo(..)
    , Scene(..)
    , SceneState(..)
    ) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Mutable as BV
import qualified Data.Vector.Storable.Mutable as SV
import qualified GLW
import qualified Graphics.Hree.Component as Component
import Graphics.Hree.GL.Types
import Graphics.Hree.Math
import Graphics.Hree.Program

data Geometry = Geometry
    { geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBuffers        :: !(IntMap (GLW.Buffer, BindBufferSetting))
    , geometryIndexBuffer    :: !(Maybe GLW.Buffer)
    , geometryCount          :: !Int
    } deriving (Show)

data Material = Material
    { materialUniforms         :: !(Map ByteString Uniform)
    , materialTextures         :: ![Texture]
    , materialProgramSpecifier :: !ProgramSpec
    } deriving (Show)

newtype MeshId = MeshId
    { unMeshId :: Int
    } deriving (Show, Eq, Ord, Hashable)

data Mesh = Mesh
    { meshGeometry :: Geometry
    , meshMaterial :: Material
    } deriving (Show)

data MeshInfo = MeshInfo
    { meshInfoId          :: !MeshId
    , meshInfoMesh        :: !Mesh
    , meshInfoBuffers     :: ![GLW.Buffer]
    , meshInfoProgram     :: !ProgramInfo
    , meshInfoVertexArray :: !GLW.VertexArray
    } deriving (Show)

data Scene = Scene
    { sceneState                    :: !(IORef SceneState)
    , sceneMeshStore                :: !(Component.ComponentStore BV.MVector MeshInfo)
    , sceneMeshTransformStore       :: !(Component.ComponentStore SV.MVector Transform)
    , sceneMeshTransformMatrixStore :: !(Component.ComponentStore SV.MVector Mat4)
    }

data SceneState = SceneState
    { ssMeshCounter    :: !Int
    , ssBuffers        :: ![GLW.Buffer]
    , ssTextures       :: !(Map ByteString (GLW.Texture 'GLW.GL_TEXTURE_2D))
    , ssSamplers       :: !(Map ByteString GLW.Sampler)
    , ssDefaultTexture :: !(Maybe Texture)
    , ssPrograms       :: !(Map ProgramSpec ProgramInfo)
    }
