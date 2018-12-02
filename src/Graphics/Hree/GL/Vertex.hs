{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.GL.Vertex
    ( BasicVertex(..)
    , Vertex(..)
    , VertexField(..)
    , VertexSpec(..)
    ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import qualified Graphics.GL as GLRaw
import Graphics.Hree.GL.Types
import Graphics.Hree.Math
import qualified Graphics.Rendering.OpenGL as GL

data VertexField = VertexField
    { vertexFieldAttribName        :: !ByteString
    , vertexFieldAttribFormat      :: !AttribFormat
    , vertexFieldBindBufferSetting :: !BindBufferSetting
    } deriving (Show, Eq)

newtype VertexSpec = VertexSpec
    { unVertexSpec :: [VertexField]
    } deriving (Show, Eq)

class Storable a => Vertex a where
    vertexSpec :: Proxy a -> VertexSpec

data BasicVertex = BasicVertex
    { bvPosition :: !Vec3
    , bvNormal   :: !Vec3
    , bvUv       :: !Vec2
    , bvColor    :: !ColorW8
    } deriving (Show, Eq)

alignOffset :: Storable v => v -> Int -> Int
alignOffset a offset = offset + ((alignment a - offset) `mod` alignment a)

instance Storable BasicVertex where
    sizeOf _ = 36

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` positionOffset
        n <- peek $ castPtr ptr `plusPtr` normalOffset
        u <- peek $ castPtr ptr `plusPtr` uvOffset
        c <- peek $ castPtr ptr `plusPtr` colorOffset
        return $ BasicVertex p n u c

    poke ptr (BasicVertex p n u c) = do
        poke (castPtr ptr `plusPtr` positionOffset) p
        poke (castPtr ptr `plusPtr` normalOffset) n
        poke (castPtr ptr `plusPtr` uvOffset) u
        poke (castPtr ptr `plusPtr` colorOffset) c

positionOffset :: Int
positionOffset = alignOffset (undefined :: Vec3) 0

normalOffset :: Int
normalOffset = alignOffset (undefined :: Vec3) positionOffset

uvOffset :: Int
uvOffset = alignOffset (undefined :: Vec2) normalOffset

colorOffset :: Int
colorOffset = alignOffset (undefined :: ColorW8) uvOffset

instance Vertex BasicVertex where
    vertexSpec _ = VertexSpec
        [ positionField
        , normalField
        , uvField
        , colorField
        ]

        where
        bbs = BindBufferSetting 0 (sizeOf (undefined :: BasicVertex))
        positionField = VertexField "position" (AttribFormat 3 GLRaw.GL_FLOAT False positionOffset) bbs
        normalField = VertexField "position" (AttribFormat 3 GLRaw.GL_FLOAT False normalOffset) bbs
        uvField = VertexField "uv" (AttribFormat 2 GLRaw.GL_FLOAT False uvOffset) bbs
        colorField = VertexField "color" (AttribFormat 3 GLRaw.GL_UNSIGNED_BYTE False colorOffset) bbs
