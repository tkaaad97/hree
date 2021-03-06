module Graphics.Hree
    ( module Graphics.Hree.Animation
    , module Graphics.Hree.Camera
    , module Graphics.Hree.Geometry
    , module Graphics.Hree.Geometry.Box
    , module Graphics.Hree.GL.Block
    , module Graphics.Hree.GL.Types
    , module Graphics.Hree.GL.UniformBlock
    , module Graphics.Hree.GL.Vertex
    , module Graphics.Hree.Light
    , module Graphics.Hree.Material
    , module Graphics.Hree.Math
    , module Graphics.Hree.GL.Sampler
    , module Graphics.Hree.Scene
    , module Graphics.Hree.SceneTask
    , module Graphics.Hree.Skin
    , module Graphics.Hree.GL.Texture
    , module Graphics.Hree.Types
    , mesh
    , node
    , skinnedMesh
    ) where

import Graphics.Hree.Animation
import Graphics.Hree.Camera
import Graphics.Hree.Geometry
import Graphics.Hree.Geometry.Box
import Graphics.Hree.GL.Block
import Graphics.Hree.GL.Sampler
import Graphics.Hree.GL.Texture (TextureParam(..))
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.UniformBlock
import Graphics.Hree.GL.Vertex (BasicVertex(..), PositionAndNormal(..),
                                SpriteOffset(..), SpriteVertex(..), Uv(..),
                                Vertex(..), VertexField(..), VertexSpec(..))
import Graphics.Hree.Light
import Graphics.Hree.Material
import Graphics.Hree.Math
import Graphics.Hree.Scene
import Graphics.Hree.SceneTask
import Graphics.Hree.Skin
import Graphics.Hree.Types (ClearOption(..), LightId, MappingSource(..),
                            Material(..), MaterialId, MatricesBlockBinder,
                            Mesh(..), MeshId, Node(..), NodeId,
                            Renderer(rendererOption), RendererOption(..), Scene,
                            Skin, SkinId, TextureMappingType(..),
                            TextureSettings(..), TextureSourceData(..))
import qualified Linear (Quaternion(..), V3(..), identity)

mesh :: Geometry -> MaterialId b -> Mesh b
mesh geometry materialId = Mesh geometry materialId Nothing Nothing Nothing

skinnedMesh :: Geometry -> MaterialId b -> SkinId -> Mesh b
skinnedMesh geometry materialId skinId = Mesh geometry materialId Nothing (Just skinId) Nothing

node :: Node
node = Node Nothing mempty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1) Linear.identity
