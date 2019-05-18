module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramSpec
    ) where

import Graphics.Hree.Geometry
import Graphics.Hree.Material
import Graphics.Hree.Program
import Graphics.Hree.Types

resolveProgramSpec :: Mesh -> ProgramSpec
resolveProgramSpec = materialProgramSpecifier . meshMaterial
