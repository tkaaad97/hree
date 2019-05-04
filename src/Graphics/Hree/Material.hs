{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Material
    ( Material(..)
    , Texture(..)
    , basicMaterial
    , flatColorMaterial
    , testMaterial
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Program
import Linear (V4)

data Material = Material
    { materialUniforms         :: !(Map ByteString Uniform)
    , materialTextures         :: ![Texture]
    , materialProgramSpecifier :: !ProgramSpec
    } deriving (Show)

basicMaterial :: Maybe Texture -> Material
basicMaterial a = Material u t p
    where
    u = Map.empty
    t = maybeToList a
    p = basicProgramSpec

flatColorMaterial :: V4 GL.GLfloat -> Material
flatColorMaterial color = Material u t p
    where
    u = Map.singleton "color" (Uniform color)
    t = []
    p = flatColorProgramSpec

testMaterial :: Material
testMaterial = Material u t p
    where
    u = Map.empty
    t = []
    p = testProgramSpec
