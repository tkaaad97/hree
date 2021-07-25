{-# LANGUAGE TypeFamilies #-}
module Hree.Material.TestMaterial
    ( testMaterial
    ) where

import Hree.Program (EmbeddedProgramType(..), ProgramOption_(..),
                     ProgramSpec(..))
import Hree.Types (Material(..))

testMaterial :: Material ()
testMaterial = Material
    { materialUniformBlock = ()
    , materialMappings = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty { programOptionGlslVersion = pure (Just 140) }
    , materialProgramSpec = EmbeddedProgram TestProgram
    }
