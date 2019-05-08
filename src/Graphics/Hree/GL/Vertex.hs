{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hree.GL.Vertex
    ( BasicVertex(..)
    , PositionAndNormal(..)
    , Vertex(..)
    , VertexField(..)
    , VertexSpec(..)
    , positionOffset
    , normalOffset
    , uvOffset
    , colorOffset
    ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.Math

data VertexField = VertexField
    { vertexFieldAttribName   :: !ByteString
    , vertexFieldAttribFormat :: !AttribFormat
    } deriving (Show, Eq)

data VertexSpec = VertexSpec
    { vertexSpecBindBufferSetting :: !BindBufferSetting
    , vertexSpecFields            :: ![VertexField]
    }

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
normalOffset = alignOffset (undefined :: Vec3) (positionOffset + sizeOf (undefined :: Vec3))

uvOffset :: Int
uvOffset = alignOffset (undefined :: Vec2) (normalOffset + sizeOf (undefined :: Vec3))

colorOffset :: Int
colorOffset = alignOffset (undefined :: ColorW8) (uvOffset + sizeOf (undefined :: Vec2))

instance Vertex BasicVertex where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionField = VertexField "position" (AttribFormat 3 GL.GL_FLOAT False positionOffset)
        normalField = VertexField "normal" (AttribFormat 3 GL.GL_FLOAT False normalOffset)
        uvField = VertexField "uv" (AttribFormat 2 GL.GL_FLOAT False uvOffset)
        colorField = VertexField "color" (AttribFormat 4 GL.GL_UNSIGNED_BYTE False colorOffset)
        fields = [ positionField
            , normalField
            , uvField
            , colorField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: BasicVertex))

data PositionAndNormal = PositionAndNormal
    { pnPosition :: !Vec3
    , pnNormal   :: !Vec3
    } deriving (Show, Eq)

instance Storable PositionAndNormal where
    sizeOf _ = 24

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` positionOffset
        n <- peek $ castPtr ptr `plusPtr` normalOffset
        return $ PositionAndNormal p n

    poke ptr (PositionAndNormal p n) = do
        poke (castPtr ptr `plusPtr` positionOffset) p
        poke (castPtr ptr `plusPtr` normalOffset) n

instance Vertex PositionAndNormal where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionField = VertexField "position" (AttribFormat 3 GL.GL_FLOAT False positionOffset)
        normalField = VertexField "normal" (AttribFormat 3 GL.GL_FLOAT False normalOffset)
        fields = [ positionField
            , normalField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: PositionAndNormal))
