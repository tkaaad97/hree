{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.TestMaterial
    ( TestMaterial(..)
    , testMaterial
    ) where

import Graphics.Hree.Material (Material(..))
import Graphics.Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))

data TestMaterial = TestMaterial
    deriving (Show, Eq)

instance Material TestMaterial where
    type MaterialUniformBlock TestMaterial = ()
    materialUniformBlock _ = ()
    materialTextures = mempty
    materialHasTextureMapping _ _ = False
    materialProgramSpec _ = EmbeddedProgram TestProgram

testMaterial :: TestMaterial
testMaterial = TestMaterial
