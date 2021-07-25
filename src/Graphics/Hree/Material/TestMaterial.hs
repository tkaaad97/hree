{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.TestMaterial
    ( testMaterial
    ) where

import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramOption_(..),
                              ProgramSpec(..))
import Graphics.Hree.Types (Material(..))

testMaterial :: Material ()
testMaterial = Material
    { materialUniformBlock = ()
    , materialMappings = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty { programOptionGlslVersion = pure (Just 140) }
    , materialProgramSpec = EmbeddedProgram TestProgram
    }
