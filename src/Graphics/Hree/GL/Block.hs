{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Hree.GL.Block
    ( Block(..)
    ) where

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign (Ptr)
import qualified Foreign (Storable(..), plusPtr)
import Graphics.Hree.GL.Types (BVec2, BVec3, BVec4, DMat2, DMat2x3, DMat2x4,
                               DMat3, DMat3x2, DMat3x4, DMat4, DMat4x2, DMat4x3,
                               DVec2, DVec3, DVec4, IVec2, IVec3, IVec4, Mat2,
                               Mat2x3, Mat2x4, Mat3, Mat3x2, Mat3x4, Mat4,
                               Mat4x2, Mat4x3, UVec2, UVec3, UVec4, Vec2, Vec3,
                               Vec4)
import Linear (V2(..), V3(..), V4(..))

class Block a where
    alignmentStd140 :: p a -> Int
    sizeOfStd140 :: p a -> Int
    peekStd140 :: Ptr a -> IO a
    pokeStd140 :: Ptr a -> a -> IO ()
    peekElemOffStd140 :: Ptr a -> Int -> IO a
    pokeElemOffStd140 :: Ptr a -> Int -> a -> IO ()
    peekByteOffStd140 :: Ptr b -> Int -> IO a
    pokeByteOffStd140 :: Ptr b -> Int -> a -> IO ()

    peekStd140 ptr = peekElemOffStd140 ptr 0
    pokeStd140 ptr = pokeElemOffStd140 ptr 0

    peekElemOffStd140 ptr off = peekByteOffStd140 ptr (off * sizeOfStd140 ptr)
    pokeElemOffStd140 ptr off = pokeByteOffStd140 ptr (off * sizeOfStd140 ptr)

    peekByteOffStd140 ptr off = peekStd140 (ptr `Foreign.plusPtr` off)
    pokeByteOffStd140 ptr off = pokeStd140 (ptr `Foreign.plusPtr` off)

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True  = 1

instance Block Bool where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 ptr off = toBool <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fromBool

instance Block Float where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Int32 where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Word32 where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Double where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block BVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 ptr off = fmap toBool <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBool

instance Block BVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 ptr off = fmap toBool <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBool

instance Block BVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 ptr off = fmap toBool <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBool

instance Block Vec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Vec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Vec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block IVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block IVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block IVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block UVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block UVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block UVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block DVec2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block DVec3 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 24
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block DVec4 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 32
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Block Mat2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 32
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        return $ V2
            (V2 e00 e01)
            (V2 e10 e11)
    pokeByteOffStd140 ptr off (V2 (V2 e00 e01) (V2 e10 e11)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)

instance Block Mat2x3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 48
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 16)
        return $ V3
            (V2 e00 e01)
            (V2 e10 e11)
            (V2 e20 e21)
    pokeByteOffStd140 ptr off (V3 (V2 e00 e01) (V2 e10 e11) (V2 e20 e21)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 16) (V3 e01 e11 e21)

instance Block Mat2x4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 16)
        return $ V4
            (V2 e00 e01)
            (V2 e10 e11)
            (V2 e20 e21)
            (V2 e30 e31)
    pokeByteOffStd140 ptr off (V4 (V2 e00 e01) (V2 e10 e11) (V2 e20 e21) (V2 e30 e31)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 16) (V4 e01 e11 e21 e31)

instance Block Mat3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 48
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 16)
        V3 e02 e12 e22 <- Foreign.peekByteOff ptr (off + 32)
        return $ V3
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
            (V3 e20 e21 e22)
    pokeByteOffStd140 ptr off (V3 (V3 e00 e01 e02) (V3 e10 e11 e12) (V3 e20 e21 e22)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 16) (V3 e01 e11 e21)
        Foreign.pokeByteOff ptr (off + 32) (V3 e02 e12 e22)

instance Block Mat3x2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 48
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        V2 e02 e12 <- Foreign.peekByteOff ptr (off + 32)
        return $ V2
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
    pokeByteOffStd140 ptr off (V2 (V3 e00 e01 e02) (V3 e10 e11 e12)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)
        Foreign.pokeByteOff ptr (off + 32) (V2 e02 e12)

instance Block Mat3x4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 48
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 16)
        V4 e02 e12 e22 e32 <- Foreign.peekByteOff ptr (off + 32)
        return $ V4
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
            (V3 e20 e21 e22)
            (V3 e30 e31 e32)
    pokeByteOffStd140 ptr off (V4 (V3 e00 e01 e02) (V3 e10 e11 e12) (V3 e20 e21 e22) (V3 e30 e31 e32)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 16) (V4 e01 e11 e21 e31)
        Foreign.pokeByteOff ptr (off + 32) (V4 e02 e12 e22 e32)

instance Block Mat4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 16)
        V4 e02 e12 e22 e32 <- Foreign.peekByteOff ptr (off + 32)
        V4 e03 e13 e23 e33 <- Foreign.peekByteOff ptr (off + 48)
        return $ V4
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
            (V4 e20 e21 e22 e23)
            (V4 e30 e31 e32 e33)
    pokeByteOffStd140 ptr off (V4 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13) (V4 e20 e21 e22 e23) (V4 e30 e31 e32 e33)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 16) (V4 e01 e11 e21 e31)
        Foreign.pokeByteOff ptr (off + 32) (V4 e02 e12 e22 e32)
        Foreign.pokeByteOff ptr (off + 48) (V4 e03 e13 e23 e33)

instance Block Mat4x2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        V2 e02 e12 <- Foreign.peekByteOff ptr (off + 32)
        V2 e03 e13 <- Foreign.peekByteOff ptr (off + 48)
        return $ V2
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
    pokeByteOffStd140 ptr off (V2 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)
        Foreign.pokeByteOff ptr (off + 32) (V2 e02 e12)
        Foreign.pokeByteOff ptr (off + 48) (V2 e03 e13)

instance Block Mat4x3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 16)
        V3 e02 e12 e22 <- Foreign.peekByteOff ptr (off + 32)
        V3 e03 e13 e23 <- Foreign.peekByteOff ptr (off + 48)
        return $ V3
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
            (V4 e20 e21 e22 e23)
    pokeByteOffStd140 ptr off (V3 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13) (V4 e20 e21 e22 e23)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 16) (V3 e01 e11 e21)
        Foreign.pokeByteOff ptr (off + 32) (V3 e02 e12 e22)
        Foreign.pokeByteOff ptr (off + 48) (V3 e03 e13 e23)

instance Block DMat2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 32
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        return $ V2
            (V2 e00 e01)
            (V2 e10 e11)
    pokeByteOffStd140 ptr off (V2 (V2 e00 e01) (V2 e10 e11)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)

instance Block DMat2x3 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 32)
        return $ V3
            (V2 e00 e01)
            (V2 e10 e11)
            (V2 e20 e21)
    pokeByteOffStd140 ptr off (V3 (V2 e00 e01) (V2 e10 e11) (V2 e20 e21)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 32) (V3 e01 e11 e21)

instance Block DMat2x4 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 32)
        return $ V4
            (V2 e00 e01)
            (V2 e10 e11)
            (V2 e20 e21)
            (V2 e30 e31)
    pokeByteOffStd140 ptr off (V4 (V2 e00 e01) (V2 e10 e11) (V2 e20 e21) (V2 e30 e31)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 32) (V4 e01 e11 e21 e31)

instance Block DMat3 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 96
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 32)
        V3 e02 e12 e22 <- Foreign.peekByteOff ptr (off + 64)
        return $ V3
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
            (V3 e20 e21 e22)
    pokeByteOffStd140 ptr off (V3 (V3 e00 e01 e02) (V3 e10 e11 e12) (V3 e20 e21 e22)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 32) (V3 e01 e11 e21)
        Foreign.pokeByteOff ptr (off + 64) (V3 e02 e12 e22)

instance Block DMat3x2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 96
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        V2 e02 e12 <- Foreign.peekByteOff ptr (off + 32)
        return $ V2
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
    pokeByteOffStd140 ptr off (V2 (V3 e00 e01 e02) (V3 e10 e11 e12)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)
        Foreign.pokeByteOff ptr (off + 32) (V2 e02 e12)

instance Block DMat3x4 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 96
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 32)
        V4 e02 e12 e22 e32 <- Foreign.peekByteOff ptr (off + 64)
        return $ V4
            (V3 e00 e01 e02)
            (V3 e10 e11 e12)
            (V3 e20 e21 e22)
            (V3 e30 e31 e32)
    pokeByteOffStd140 ptr off (V4 (V3 e00 e01 e02) (V3 e10 e11 e12) (V3 e20 e21 e22) (V3 e30 e31 e32)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 32) (V4 e01 e11 e21 e31)
        Foreign.pokeByteOff ptr (off + 64) (V4 e02 e12 e22 e32)

instance Block DMat4 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 128
    peekByteOffStd140 ptr off = do
        V4 e00 e10 e20 e30 <- Foreign.peekByteOff ptr off
        V4 e01 e11 e21 e31 <- Foreign.peekByteOff ptr (off + 32)
        V4 e02 e12 e22 e32 <- Foreign.peekByteOff ptr (off + 64)
        V4 e03 e13 e23 e33 <- Foreign.peekByteOff ptr (off + 96)
        return $ V4
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
            (V4 e20 e21 e22 e23)
            (V4 e30 e31 e32 e33)
    pokeByteOffStd140 ptr off (V4 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13) (V4 e20 e21 e22 e23) (V4 e30 e31 e32 e33)) = do
        Foreign.pokeByteOff ptr off (V4 e00 e10 e20 e30)
        Foreign.pokeByteOff ptr (off + 32) (V4 e01 e11 e21 e31)
        Foreign.pokeByteOff ptr (off + 64) (V4 e02 e12 e22 e32)
        Foreign.pokeByteOff ptr (off + 96) (V4 e03 e13 e23 e33)

instance Block DMat4x2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64
    peekByteOffStd140 ptr off = do
        V2 e00 e10 <- Foreign.peekByteOff ptr off
        V2 e01 e11 <- Foreign.peekByteOff ptr (off + 16)
        V2 e02 e12 <- Foreign.peekByteOff ptr (off + 32)
        V2 e03 e13 <- Foreign.peekByteOff ptr (off + 48)
        return $ V2
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
    pokeByteOffStd140 ptr off (V2 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13)) = do
        Foreign.pokeByteOff ptr off (V2 e00 e10)
        Foreign.pokeByteOff ptr (off + 16) (V2 e01 e11)
        Foreign.pokeByteOff ptr (off + 32) (V2 e02 e12)
        Foreign.pokeByteOff ptr (off + 48) (V2 e03 e13)

instance Block DMat4x3 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 128
    peekByteOffStd140 ptr off = do
        V3 e00 e10 e20 <- Foreign.peekByteOff ptr off
        V3 e01 e11 e21 <- Foreign.peekByteOff ptr (off + 32)
        V3 e02 e12 e22 <- Foreign.peekByteOff ptr (off + 64)
        V3 e03 e13 e23 <- Foreign.peekByteOff ptr (off + 96)
        return $ V3
            (V4 e00 e01 e02 e03)
            (V4 e10 e11 e12 e13)
            (V4 e20 e21 e22 e23)
    pokeByteOffStd140 ptr off (V3 (V4 e00 e01 e02 e03) (V4 e10 e11 e12 e13) (V4 e20 e21 e22 e23)) = do
        Foreign.pokeByteOff ptr off (V3 e00 e10 e20)
        Foreign.pokeByteOff ptr (off + 32) (V3 e01 e11 e21)
        Foreign.pokeByteOff ptr (off + 64) (V3 e02 e12 e22)
        Foreign.pokeByteOff ptr (off + 96) (V3 e03 e13 e23)
