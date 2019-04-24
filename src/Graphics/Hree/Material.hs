{-# LANGUAGE DataKinds #-}
module Graphics.Hree.Material
    ( Material(..)
    , Texture(..)
    , basicMaterial
    , testMaterial
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified GLW
import Graphics.Hree.GL.Types
import Graphics.Hree.Program

data Texture = Texture
    { textureSource  :: !(GLW.Texture 'GLW.GL_TEXTURE_2D)
    , textureSampler :: !GLW.Sampler
    } deriving (Show, Eq)

data Material = Material
    { materialUniforms         :: !(Map ByteString Uniform)
    , materialTextures         :: ![Texture]
    , materialProgramSpecifier :: !ProgramSpec
    } deriving (Show)

basicMaterial :: Material
basicMaterial = Material u t p
    where
    u = Map.empty
    t = []
    p = basicProgramSpec

testMaterial :: Material
testMaterial = Material u t p
    where
    u = Map.empty
    t = []
    p = testProgramSpec
