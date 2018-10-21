module Vertex
    ( BasicVertex(..)
    ) where

import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Graphics.Hree.Math

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
