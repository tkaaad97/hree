{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Material
    ( Material(..)
    , Texture(..)
    , basicMaterial
    , flatColorMaterial
    , setBaseColorFactor
    , setBaseColorTexture
    , setDirectionalLight
    , setMetallicFactor
    , setMetallicRoughnessTexture
    , setNormalTexture
    , setRoughnessFactor
    , spriteMaterial
    , standardMaterial
    , testMaterial
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
        [ ("metallicFactor", Uniform metalness)
        , ("roughnessFactor", Uniform roughness)
        ]

testMaterial :: Material
testMaterial = Material mempty mempty testProgramSpec

setUniform :: Material -> ByteString -> Uniform -> Material
setUniform m key uniform =
    let us = materialUniforms m
        us' = Map.insert key uniform us
    in m { materialUniforms = us' }

setTexture :: Material -> ByteString -> Texture -> Material
setTexture m key texture =
    let ts = materialTextures m
        ts' = Map.insert key texture ts
    in m { materialTextures = ts' }

setBaseColorFactor :: Material -> V4 GL.GLfloat -> Material
setBaseColorFactor m = setUniform m "baseColorFactor" . Uniform

setMetallicFactor :: Material -> GL.GLfloat -> Material
setMetallicFactor m = setUniform m "metallicFactor" . Uniform

setRoughnessFactor :: Material -> GL.GLfloat -> Material
setRoughnessFactor m = setUniform m "roughnessFactor" . Uniform

setDirectionalLight :: Material -> V3 Float -> Material
setDirectionalLight m = setUniform m "directionalLight" . Uniform

setBaseColorTexture :: Material -> Texture -> Material
setBaseColorTexture = flip setTexture "baseColorTexture"

setNormalTexture :: Material -> Texture -> Material
setNormalTexture = flip setTexture "normalTexture"

setMetallicRoughnessTexture :: Material -> Texture -> Material
setMetallicRoughnessTexture = flip setTexture "metallicRoughnessTexture"
