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
    , setBaseColorTexture
    , setNormalTexture
    , setMetallicRoughnessTexture
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Program
import Graphics.Hree.Types
import Linear (V3, V4)

basicMaterial :: Material
basicMaterial = Material mempty mempty basicProgramSpec

flatColorMaterial :: V4 GL.GLfloat -> Material
flatColorMaterial color = Material u mempty flatColorProgramSpec
    where
    u = Map.singleton "color" (Uniform color)

spriteMaterial :: Texture -> Material
spriteMaterial = setBaseColorTexture (Material mempty mempty spriteProgramSpec)

standardMaterial :: Float -> Float -> Material
standardMaterial metalness roughness = Material u mempty standardProgramSpec
    where
    u = Map.fromList
        [ ("metalness", Uniform metalness)
        , ("roughness", Uniform roughness)
        ]

testMaterial :: Material
testMaterial = Material mempty mempty testProgramSpec

setDirectionalLight :: Material -> V3 Float -> Material
setDirectionalLight m dl =
    let us = materialUniforms m
        us' = Map.insert "directionalLight" (Uniform dl) us
    in m { materialUniforms = us' }

setTexture :: Material -> ByteString -> Texture -> Material
setTexture m key texture =
    let ts = materialTextures m
        ts' = Map.insert key texture ts
    in m { materialTextures = ts' }

setBaseColorTexture :: Material -> Texture -> Material
setBaseColorTexture m = setTexture m "baseColorTexture"

setNormalTexture :: Material -> Texture -> Material
setNormalTexture m = setTexture m "normalTexture"

setMetallicRoughnessTexture :: Material -> Texture -> Material
setMetallicRoughnessTexture m = setTexture m "metallicRoughnessTexture"
