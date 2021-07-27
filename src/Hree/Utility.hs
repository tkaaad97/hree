module Hree.Utility
    ( mesh
    , node
    , skinnedMesh
    ) where

import Hree.Types
import qualified Linear (Quaternion(..), V3(..), identity)

mesh :: Geometry -> MaterialId b -> Mesh b
mesh geometry materialId = Mesh geometry materialId Nothing Nothing Nothing

skinnedMesh :: Geometry -> MaterialId b -> SkinId -> Mesh b
skinnedMesh geometry materialId skinId_ = Mesh geometry materialId Nothing (Just skinId_) Nothing

node :: Node
node = Node Nothing mempty (Linear.V3 0 0 0) (Linear.Quaternion 1 (Linear.V3 0 0 0)) (Linear.V3 1 1 1) Linear.identity
