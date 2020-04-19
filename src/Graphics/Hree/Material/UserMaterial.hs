{-# LANGUAGE TypeFamilies #-}
module Graphics.Hree.Material.UserMaterial
    ( UserMaterial(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as BV (Vector)
import Graphics.Hree.GL.Block (Block(..))
import Graphics.Hree.GL.Types (BufferBindingIndex, Texture)
import Graphics.Hree.Material (Material(..))
import Graphics.Hree.Program (ProgramSpec(..), ShaderSource(..))

data UserMaterial a = UserMaterial
    { userMaterialBlock :: !a
    , userMaterialTextures :: !(BV.Vector (ByteString, Texture))
    , userMaterialVertexShaderSource :: !ShaderSource
    , userMaterialFragmentShaderSource :: !ShaderSource
    , userMaterialBindingPoints :: !(BV.Vector (ByteString, BufferBindingIndex))
    } deriving (Show, Eq)

instance Block a => Material (UserMaterial a) where
    type MaterialUniformBlock (UserMaterial a) = a
    materialUniformBlock = userMaterialBlock
    materialTextures = userMaterialTextures
    materialHasTextureMapping _ _ = False
    materialProgramSpec a _ = UserProgram (userMaterialVertexShaderSource a) (userMaterialFragmentShaderSource a) (userMaterialBindingPoints a)
