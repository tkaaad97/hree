module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramSpec
    ) where

import Graphics.Hree.Geometry
import Graphics.Hree.Material
import Graphics.Hree.Program

data Mesh = Mesh
    { meshGeometry :: Geometry
    , meshMaterial :: Material
    } deriving (Show)

resolveProgramSpec :: Mesh -> ProgramSpec
resolveProgramSpec = materialProgramSpecifier . meshMaterial
