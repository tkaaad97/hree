module Graphics.Hree.Program
    ( VertexShaderSpec(..)
    , FragmentShaderSpec(..)
    , ProgramSpec(..)
    ) where

data VertexShaderSpec = VertexShaderSpec
    deriving (Show, Eq)

data FragmentShaderSpec = FragmentShaderSpec
    deriving (Show, Eq)

data ProgramSpec = ProgramSpec
    { vertexShaderSpec   :: !VertexShaderSpec
    , fragmentShaderSpec :: !FragmentShaderSpec
    } deriving (Show, Eq)
