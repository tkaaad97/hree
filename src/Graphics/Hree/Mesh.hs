{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramSpec
    ) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Graphics.Hree.Program
import Graphics.Hree.Types

resolveProgramSpec :: Mesh -> ProgramSpec
resolveProgramSpec mesh =
    let geo = meshGeometry mesh
        material = meshMaterial mesh
        textures = materialTextures material
        options = defaultOptions
            & applyWhen (hasAttribute geo "color") (`setHasVertexColor` True)
            & applyWhen (Map.member "normalTexture" textures) (`setHasNormalMap` True)
            & applyWhen (Map.member "metallicRoughnessTexture" textures) (`setHasMetallicRoughnessMap` True)
    in (materialProgramSpec . meshMaterial $ mesh) options
    where
    applyWhen True f a  = f a
    applyWhen False _ a = a

hasAttribute :: Geometry -> ByteString -> Bool
hasAttribute geo attribName = Map.member attribName (geometryAttribBindings geo)
