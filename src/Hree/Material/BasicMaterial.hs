{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Hree.Material.BasicMaterial
    ( BasicMaterial
    , BasicMaterialBlock(..)
    , basicMaterial
    ) where

import qualified Graphics.GL as GL (GLfloat)
import Hree.GL.Block (Block(..))
import Hree.Program (EmbeddedProgramType(..), ProgramSpec(..))
import Hree.Types (Material(..))
import Linear (V3(..))

type BasicMaterial = Material BasicMaterialBlock

data BasicMaterialBlock = BasicMaterialBlock
    { directionalLight :: !(V3 GL.GLfloat)
    } deriving (Show, Eq)

instance Block BasicMaterialBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12

    peekByteOffStd140 ptr off = do
        l <- peekByteOffStd140 ptr off
        return $ BasicMaterialBlock l

    pokeByteOffStd140 ptr off (BasicMaterialBlock l) = do
        pokeByteOffStd140 ptr off l

basicMaterial :: V3 GL.GLfloat -> Material BasicMaterialBlock
basicMaterial l = Material
    { materialUniformBlock = BasicMaterialBlock l
    , materialMappings = mempty
    , materialRenderOption = mempty
    , materialProgramOption = mempty
    , materialProgramSpec = EmbeddedProgram BasicProgram
    }
