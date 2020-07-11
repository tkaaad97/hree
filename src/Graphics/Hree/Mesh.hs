{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramOption
    ) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Graphics.Hree.Material (materialHasTextureMapping)
import Graphics.Hree.Program
import Graphics.Hree.Skin (maxJointCount)
import Graphics.Hree.Types

resolveProgramOption :: Mesh b -> Maybe Skin -> ProgramOption
resolveProgramOption mesh maybeSkin =
    let geo = meshGeometry mesh
        material = meshMaterial mesh
        options = defaultProgramOption
            & applyWhen (hasAttribute geo "jointIndices") (`setHasJointIndices` True)
            & applyWhen (hasAttribute geo "jointWeights") (`setHasJointWeights` True)
            & applyWhen (hasAttribute geo "normal") (`setHasVertexNormal` True)
            & applyWhen (hasAttribute geo "tangent") (`setHasVertexTangent` True)
            & applyWhen (hasAttribute geo "color") (`setHasVertexColor` True)
            & applyWhen (materialHasTextureMapping material NormalMapping) (`setHasNormalMap` True)
            & applyWhen (materialHasTextureMapping material MetallicRoughnessMapping) (`setHasMetallicRoughnessMap` True)
            & applyWhen (materialHasTextureMapping material EmissiveMapping) (`setHasEmissiveMap` True)
            & applyWhen (materialHasTextureMapping material OcclusionMapping) (`setHasOcclusionMap` True)
            & applySkinOptions maybeSkin
            & flip applyPartialProgramOption (materialProgramOption material)
    in options
    where
    applyWhen True f a  = f a
    applyWhen False _ a = a
    applySkinOptions (Just skin) =
        (`setUseVertexSkinning` True) . (`setMaxJointCount` maxJointCount skin)
    applySkinOptions Nothing = id

hasAttribute :: Geometry -> ByteString -> Bool
hasAttribute geo attribName = Map.member attribName (geometryAttribBindings geo)
