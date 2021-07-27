{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hree.Vertex
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
import Hree.GL (attribFormat, attribIFormat)
import Hree.GL.Types
import Hree.Math

data VertexField = VertexField
    { vertexFieldAttribName      :: !ByteString
    , vertexFieldAttributeFormat :: !AttributeFormat
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
        positionField = VertexField "position" (attribFormat 3 GL.GL_FLOAT False positionOffset)
        normalField = VertexField "normal" (attribFormat 3 GL.GL_FLOAT False normalOffset)
        uvField = VertexField "uv" (attribFormat 2 GL.GL_FLOAT False uvOffset)
        colorField = VertexField "color" (attribFormat 4 GL.GL_UNSIGNED_BYTE True colorOffset)
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
        positionField = VertexField "position" (attribFormat 3 GL.GL_FLOAT False positionOffset)
        normalField = VertexField "normal" (attribFormat 3 GL.GL_FLOAT False normalOffset)
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
        uvField = VertexField "uv" (attribFormat 2 GL.GL_FLOAT False 0)
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
        positionOffsetField = VertexField "positionOffset" (attribFormat 3 GL.GL_FLOAT False 0)
        uvOffsetField = VertexField "uvOffset" (attribFormat 2 GL.GL_FLOAT False 12)
        fields =
            [ positionOffsetField
            , uvOffsetField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: SpriteOffset)) 0

data SpriteVertex = SpriteVertex
    { svPosition  :: !Vec3
    , svSize      :: !Vec3
    , svCenter    :: !Vec3
    , svAngle     :: !Float
    , svUv        :: !Vec2
    , svUvSize    :: !Vec2
    , svUseTile   :: !GL.GLuint
    , svTileIndex :: !GL.GLuint
    } deriving (Show, Eq)

instance Storable SpriteVertex where
    sizeOf _ = 64

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` 0
        s <- peek $ castPtr ptr `plusPtr` 12
        c <- peek $ castPtr ptr `plusPtr` 24
        a <- peek $ castPtr ptr `plusPtr` 36
        u <- peek $ castPtr ptr `plusPtr` 40
        us <- peek $ castPtr ptr `plusPtr` 48
        useTile <- peek $ castPtr ptr `plusPtr` 56
        tileIndex <- peek $ castPtr ptr `plusPtr` 60
        return $ SpriteVertex p s c a u us useTile tileIndex

    poke ptr (SpriteVertex p s c a u us useTile tileIndex) = do
        poke (castPtr ptr `plusPtr` 0) p
        poke (castPtr ptr `plusPtr` 12) s
        poke (castPtr ptr `plusPtr` 24) c
        poke (castPtr ptr `plusPtr` 36) a
        poke (castPtr ptr `plusPtr` 40) u
        poke (castPtr ptr `plusPtr` 48) us
        poke (castPtr ptr `plusPtr` 56) useTile
        poke (castPtr ptr `plusPtr` 60) tileIndex

instance Vertex SpriteVertex where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionField = VertexField "position" (attribFormat 3 GL.GL_FLOAT False 0)
        sizeField = VertexField "size" (attribFormat 3 GL.GL_FLOAT False 12)
        centerField = VertexField "center" (attribFormat 3 GL.GL_FLOAT False 24)
        angleField = VertexField "angle" (attribFormat 1 GL.GL_FLOAT False 36)
        uvField = VertexField "uv" (attribFormat 2 GL.GL_FLOAT False 40)
        uvSizeField = VertexField "uvSize" (attribFormat 2 GL.GL_FLOAT False 48)
        useTileField = VertexField "useTile" (attribIFormat 1 GL.GL_UNSIGNED_INT 56)
        tileIndexField = VertexField "tileIndex" (attribIFormat 1 GL.GL_UNSIGNED_INT 60)
        fields =
            [ positionField
            , sizeField
            , centerField
            , angleField
            , uvField
            , uvSizeField
            , useTileField
            , tileIndexField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: SpriteVertex)) 1
