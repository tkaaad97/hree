module Graphics.Hree.Mesh
    ( Mesh(..)
    ) where

import Graphics.Hree.Geometry
import Graphics.Hree.Material

data Mesh = Mesh
    { meshGeometry :: Geometry
    , meshMaterial :: Material
    }
