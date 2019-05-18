module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramSpec
    ) where

import Graphics.Hree.Program
import Graphics.Hree.Types

resolveProgramSpec :: Mesh -> ProgramSpec
resolveProgramSpec = materialProgramSpecifier . meshMaterial
