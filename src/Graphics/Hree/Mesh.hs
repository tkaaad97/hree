{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Mesh
    ( Mesh(..)
    , meshProgramOption
    ) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Graphics.Hree.Program
import Graphics.Hree.Skin (maxJointCount)
import Graphics.Hree.Types

meshProgramOption :: Mesh b -> MaterialInfo -> Maybe Skin -> ProgramOption
meshProgramOption mesh materialInfo maybeSkin =
    let geo = meshGeometry mesh
        options = defaultProgramOption
            & applyWhen (hasAttribute geo "jointIndices") (`setHasJointIndices` True)
            & applyWhen (hasAttribute geo "jointWeights") (`setHasJointWeights` True)
            & applyWhen (hasAttribute geo "normal") (`setHasVertexNormal` True)
            & applyWhen (hasAttribute geo "tangent") (`setHasVertexTangent` True)
            & applyWhen (hasAttribute geo "color") (`setHasVertexColor` True)
            & applyWhen (hasTextureMapping NormalMapping) (`setHasNormalMap` True)
            & applyWhen (hasTextureMapping MetallicRoughnessMapping) (`setHasMetallicRoughnessMap` True)
            & applyWhen (hasTextureMapping EmissiveMapping) (`setHasEmissiveMap` True)
            & applyWhen (hasTextureMapping OcclusionMapping) (`setHasOcclusionMap` True)
            & applySkinOptions maybeSkin
            & flip applyPartialProgramOption (materialInfoProgramOption materialInfo)
    in options
    where
    applyWhen True f a  = f a
    applyWhen False _ a = a
    applySkinOptions (Just skin) =
        (`setUseVertexSkinning` True) . (`setMaxJointCount` maxJointCount skin)
    applySkinOptions Nothing = id

    mappings = materialInfoMappings materialInfo
    hasTextureMapping = flip Map.member mappings

hasAttribute :: Geometry -> ByteString -> Bool
hasAttribute geo attribName = Map.member attribName (geometryAttribBindings geo)
