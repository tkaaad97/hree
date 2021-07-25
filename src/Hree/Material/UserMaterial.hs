{-# LANGUAGE TypeFamilies #-}
module Hree.Material.UserMaterial
    ( userMaterial
    ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as BV (Vector)
import Hree.GL.Types (UniformBufferBindingIndex)
import Hree.Program (ProgramSpec(..), ShaderSource(..))
import Hree.Types (Material(..))

userMaterial :: a -> ShaderSource -> ShaderSource -> BV.Vector (ByteString, UniformBufferBindingIndex) -> Material a
userMaterial block vshader fshader bindingPoints = Material
    { materialUniformBlock = block
    , materialMappings = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = UserProgram vshader fshader bindingPoints
    }
