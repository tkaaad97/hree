module Graphics.Hree.Material
    ( Material(..)
    , basicMaterial
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Graphics.Hree.GL.Types
import Graphics.Hree.Program
import Graphics.Hree.Texture

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
