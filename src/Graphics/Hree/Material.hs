module Graphics.Hree.Material
    ( Material(..)
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Graphics.Hree.GL.Types
import Graphics.Hree.Program
import Graphics.Hree.Texture

data Material = Material
    { materialUniforms         :: !(Map ByteString Uniform)
    , materialTextures         :: ![Texture]
    , materialProgramSpecifier :: !ProgramSpec
    } deriving (Show)
