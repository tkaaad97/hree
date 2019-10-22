{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Material
    ( Material(..)
    , Texture(..)
    , basicMaterial
    , flatColorMaterial
    , spriteMaterial
    , standardMaterial
    , testMaterial
    , setDirectionalLight
    ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Program
import Graphics.Hree.Types
import Linear (V3, V4)

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

spriteMaterial :: Texture -> Material
spriteMaterial a = Material u t p
    where
    u = Map.empty
    t = [a]
    p = spriteProgramSpec

standardMaterial :: Float -> Float -> Maybe Texture -> Material
standardMaterial metalness roughness a = Material u t p
    where
    u = Map.fromList
        [ ("metalness", Uniform metalness)
        , ("roughness", Uniform roughness)
        ]
    t = maybeToList a
    p = standardProgramSpec

testMaterial :: Material
testMaterial = Material u t p
    where
    u = Map.empty
    t = []
    p = testProgramSpec

setDirectionalLight :: Material -> V3 Float -> Material
setDirectionalLight m dl = m { materialUniforms = us' }
    where
    u = Uniform dl
    us = materialUniforms m
    us' = Map.insert "directionalLight" u us
