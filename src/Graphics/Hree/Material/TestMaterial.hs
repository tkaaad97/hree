{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.TestMaterial
    ( testMaterial
    ) where

import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Graphics.Hree.Types (Material(..))

testMaterial :: Material ()
testMaterial = Material
    { materialUniformBlock = ()
    , materialTextures = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = EmbeddedProgram TestProgram
    }
