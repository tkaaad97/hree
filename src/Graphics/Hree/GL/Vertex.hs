{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Graphics.Hree.GL.Vertex
    ( BasicVertex(..)
    , PositionAndNormal(..)
    , SpriteOffset(..)
    , SpriteVertex(..)
    , Uv(..)
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
        colorField = VertexField "color" (AttribFormat 4 GL.GL_UNSIGNED_BYTE True colorOffset)
        fields = [ positionField
            , normalField
            , uvField
            , colorField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: BasicVertex)) 0

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
        bbs = BindBufferSetting 0 (sizeOf (undefined :: PositionAndNormal)) 0

newtype Uv = Uv
    { unUv :: Vec2
    } deriving (Show, Eq, Storable)

instance Vertex Uv where
    vertexSpec _ = VertexSpec bbs fields

        where
        uvField = VertexField "uv" (AttribFormat 2 GL.GL_FLOAT False 0)
        fields = [ uvField ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: Uv)) 0

data SpriteOffset = SpriteOffset
    { soPositionOffset :: !Vec3
    , soUvOffset       :: !Vec2
    } deriving (Show, Eq)

instance Storable SpriteOffset where
    sizeOf _ = 20

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` 0
        u <- peek $ castPtr ptr `plusPtr` 12
        return $ SpriteOffset p u

    poke ptr (SpriteOffset p u) = do
        poke (castPtr ptr `plusPtr` 0) p
        poke (castPtr ptr `plusPtr` 12) u

instance Vertex SpriteOffset where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionOffsetField = VertexField "positionOffset" (AttribFormat 3 GL.GL_FLOAT False 0)
        uvOffsetField = VertexField "uvOffset" (AttribFormat 2 GL.GL_FLOAT False 12)
        fields =
            [ positionOffsetField
            , uvOffsetField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: SpriteOffset)) 0

data SpriteVertex = SpriteVertex
    { svPosition :: !Vec3
    , svSize     :: !Vec3
    , svAngle    :: !Float
    , svUv       :: !Vec2
    , svUvSize   :: !Vec2
    } deriving (Show, Eq)

instance Storable SpriteVertex where
    sizeOf _ = 44

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` 0
        s <- peek $ castPtr ptr `plusPtr` 12
        a <- peek $ castPtr ptr `plusPtr` 24
        u <- peek $ castPtr ptr `plusPtr` 28
        us <- peek $ castPtr ptr `plusPtr` 36
        return $ SpriteVertex p s a u us

    poke ptr (SpriteVertex p s a u us) = do
        poke (castPtr ptr `plusPtr` 0) p
        poke (castPtr ptr `plusPtr` 12) s
        poke (castPtr ptr `plusPtr` 24) a
        poke (castPtr ptr `plusPtr` 28) u
        poke (castPtr ptr `plusPtr` 36) us

instance Vertex SpriteVertex where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionField = VertexField "position" (AttribFormat 3 GL.GL_FLOAT False 0)
        sizeField = VertexField "size" (AttribFormat 3 GL.GL_FLOAT False 12)
        angleField = VertexField "angle" (AttribFormat 1 GL.GL_FLOAT False 24)
        uvField = VertexField "uv" (AttribFormat 2 GL.GL_FLOAT False 28)
        uvSizeField = VertexField "uvSize" (AttribFormat 2 GL.GL_FLOAT False 36)
        fields =
            [ positionField
            , sizeField
            , angleField
            , uvField
            , uvSizeField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: SpriteVertex)) 1
