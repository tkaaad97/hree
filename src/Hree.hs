module Hree
    ( module Hree.Animation
    , module Hree.Camera
    , module Hree.Geometry
    , module Hree.Geometry.Box
    , module Hree.GL.Block
    , module Hree.GL.Types
    , module Hree.GL.UniformBlock
    , module Hree.GL.Vertex
    , module Hree.Light
    , module Hree.Material
    , module Hree.Math
    , module Hree.GL.Sampler
    , module Hree.Scene
    , module Hree.SceneTask
    , module Hree.Skin
    , module Hree.GL.Texture
    , module Hree.Types
    , mesh
    , node
    , skinnedMesh
    ) where

import Hree.Animation
import Hree.Camera
import Hree.Geometry
import Hree.Geometry.Box
import Hree.GL.Block
import Hree.GL.Sampler
import Hree.GL.Texture (TextureParam(..))
import Hree.GL.Types
import Hree.GL.UniformBlock
import Hree.GL.Vertex (BasicVertex(..), PositionAndNormal(..), SpriteOffset(..),
                       SpriteVertex(..), Uv(..), Vertex(..), VertexField(..),
                       VertexSpec(..))
import Hree.Light
import Hree.Material
import Hree.Math
import Hree.Scene
import Hree.SceneTask
import Hree.Skin
import Hree.Types (ClearOption(..), LightId, MappingSource(..), Material(..),
                   MaterialId, MatricesBlockBinder, Mesh(..), MeshId, Node(..),
                   NodeId, Renderer(rendererOption), RendererOption(..), Scene,
                   Skin, SkinId, TextureMappingType(..), TextureSettings(..),
                   TextureSourceData(..))
import qualified Linear (Quaternion(..), V3(..), identity)

mesh :: Geometry -> MaterialId b -> Mesh b
mesh geometry materialId = Mesh geometry materialId Nothing Nothing Nothing

skinnedMesh :: Geometry -> MaterialId b -> SkinId -> Mesh b
skinnedMesh geometry materialId skinId = Mesh geometry materialId Nothing (Just skinId) Nothing

node :: Node
node = Node Nothing mempty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1) Linear.identity
