{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.Mesh
    ( Mesh(..)
    , resolveProgramSpec
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Graphics.Hree.Program
import Graphics.Hree.Types

resolveProgramSpec :: Mesh -> ProgramSpec
resolveProgramSpec mesh =
    let geo = meshGeometry mesh
        options = if hasAttribute geo "color"
                    then defaultOptions { optionsHasVertexColor = True }
                    else defaultOptions { optionsHasVertexColor = False }
    in (materialProgramSpec . meshMaterial $ mesh) options

hasAttribute :: Geometry -> ByteString -> Bool
hasAttribute geo attribName = Map.member attribName (geometryAttribBindings geo)
