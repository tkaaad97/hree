{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Hree.GL.Block
    ( Block(..)
    , Element(..)
    , Elem(..)
    , Std140(..)
    ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Generic as GV (imapM_)
import qualified Data.Vector.Storable as SV (generateM, length)
import Data.Word (Word32)
import Foreign (Ptr)
import qualified Foreign (Storable(..), castPtr, plusPtr)
import GHC.TypeNats (KnownNat, natVal)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types (BVec2, BVec3, BVec4, DMat2, DMat2x3, DMat2x4,
                               DMat3, DMat3x2, DMat3x4, DMat4, DMat4x2, DMat4x3,
                               DVec2, DVec3, DVec4, IVec2, IVec3, IVec4,
                               LimitedVector(..), Mat2, Mat2x3, Mat2x4, Mat3,
                               Mat3x2, Mat3x4, Mat4, Mat4x2, Mat4x3, UVec2,
                               UVec3, UVec4, Vec2, Vec3, Vec4)
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
    writeBuffer :: Ptr a -> GLW.Buffer -> IO ()

    peekStd140 ptr = peekElemOffStd140 ptr 0
    pokeStd140 ptr = pokeElemOffStd140 ptr 0

    peekElemOffStd140 ptr off = peekByteOffStd140 ptr (off * sizeOfStd140 ptr)
    pokeElemOffStd140 ptr off = pokeByteOffStd140 ptr (off * sizeOfStd140 ptr)

    peekByteOffStd140 ptr off = peekStd140 (ptr `Foreign.plusPtr` off)
    pokeByteOffStd140 ptr off = pokeStd140 (ptr `Foreign.plusPtr` off)

    writeBuffer ptr buffer =
        let size = fromIntegral $ sizeOfStd140 ptr
        in GLW.glNamedBufferSubData buffer 0 size (Foreign.castPtr ptr)

class Block a => Element a where
    elemAlignmentStd140 :: p a -> Int
    elemStrideStd140 :: p a -> Int

newtype Elem a = Elem { unElem :: a }
    deriving (Show, Eq)

instance Element a => Foreign.Storable (Elem a) where
    sizeOf _ = elemStrideStd140 (Proxy :: Proxy a)
    alignment _ = elemAlignmentStd140 (Proxy :: Proxy a)
    peekByteOff ptr off = Elem <$> peekByteOffStd140 ptr off
    pokeByteOff ptr off = pokeByteOffStd140 ptr off . unElem

newtype Std140 a = Std140 { unStd140 :: a }
    deriving (Show, Eq)

instance Block a => Foreign.Storable (Std140 a) where
    sizeOf _ = sizeOfStd140 (Proxy :: Proxy a)
    alignment _ = alignmentStd140 (Proxy :: Proxy a)
    peekByteOff ptr off = Std140 <$> peekByteOffStd140 ptr off
    pokeByteOff ptr off = pokeByteOffStd140 ptr off . unStd140

toBoolean :: Word32 -> GL.GLboolean
toBoolean = fromIntegral

fromBoolean :: GL.GLboolean -> Word32
fromBoolean = fromIntegral

instance Block () where
    alignmentStd140 _ = 1
    sizeOfStd140 _ = 1
    peekByteOffStd140 _ _ = return ()
    pokeByteOffStd140 _ _ _ = return ()

instance Block GL.GLboolean where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 ptr off = toBoolean <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fromBoolean

instance Element GL.GLboolean where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Float where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Float where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Int32 where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Int32 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Word32 where
    alignmentStd140 _ = 4
    sizeOfStd140 _ = 4
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Word32 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Double where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Double where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block BVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 ptr off = fmap toBoolean <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBoolean

instance Element BVec2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block BVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 ptr off = fmap toBoolean <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBoolean

instance Element BVec3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block BVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 ptr off = fmap toBoolean <$> Foreign.peekByteOff ptr off
    pokeByteOffStd140 ptr off = Foreign.pokeByteOff ptr off . fmap fromBoolean

instance Element BVec4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Vec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Vec2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Vec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Vec3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block Vec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element Vec4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block IVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element IVec2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block IVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element IVec3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block IVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element IVec4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block UVec2 where
    alignmentStd140 _ = 8
    sizeOfStd140 _ = 8
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element UVec2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block UVec3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 12
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element UVec3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block UVec4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element UVec4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block DVec2 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 16
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element DVec2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 16

instance Block DVec3 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 24
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element DVec3 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 32

instance Block DVec4 where
    alignmentStd140 _ = 32
    sizeOfStd140 _ = 32
    peekByteOffStd140 = Foreign.peekByteOff
    pokeByteOffStd140 = Foreign.pokeByteOff

instance Element DVec4 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 32

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

instance Element Mat2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 32

instance Block Mat2x3 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 32
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

instance Element Mat2x3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 32

instance Block Mat2x4 where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 32
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

instance Element Mat2x4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 32

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

instance Element Mat3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 48

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

instance Element Mat3x2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 48

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

instance Element Mat3x4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 48

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

instance Element Mat4 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

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

instance Element Mat4x2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

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

instance Element Mat4x3 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

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

instance Element DMat2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 32

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

instance Element DMat2x3 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 64

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

instance Element DMat2x4 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 64

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

instance Element DMat3 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 96

instance Block DMat3x2 where
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

instance Element DMat3x2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 48

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

instance Element DMat3x4 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 96

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

instance Element DMat4 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 128

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

instance Element DMat4x2 where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

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

instance Element DMat4x3 where
    elemAlignmentStd140 _ = 32
    elemStrideStd140 _ = 128

instance (Element a, KnownNat n) => Block (LimitedVector n (Elem a)) where
    alignmentStd140 _ = elemAlignmentStd140 (Proxy :: Proxy a)
    sizeOfStd140 _ =
        let h = elemAlignmentStd140 (Proxy :: Proxy a)
            b = fromIntegral (natVal (Proxy :: Proxy n)) * elemStrideStd140 (Proxy :: Proxy a)
        in h + b
    peekByteOffStd140 ptr off = do
        let align = elemAlignmentStd140 (Proxy :: Proxy a)
            stride = elemStrideStd140 (Proxy :: Proxy a)
            limit = fromIntegral . natVal $ (Proxy :: Proxy n)
            off' = off + align
        size <- peekByteOffStd140 ptr off :: IO Int32
        v <- SV.generateM (min (fromIntegral size) limit) $ \i -> Elem <$> peekByteOffStd140 ptr (off' + stride * i)
        return (LimitedVector v)
    pokeByteOffStd140 ptr off (LimitedVector v) = do
        let align = elemAlignmentStd140 (Proxy :: Proxy a)
            stride = elemStrideStd140 (Proxy :: Proxy a)
            limit = fromIntegral . natVal $ (Proxy :: Proxy n)
            off' = off + align
            size :: Int32
            size = fromIntegral . SV.length $ v
            size' = min size limit
        pokeByteOffStd140 ptr off size'
        GV.imapM_ (\i -> pokeByteOffStd140 ptr (off' + stride * i) . unElem) v
    writeBuffer ptr buffer = do
        len <- peekStd140 (Foreign.castPtr ptr) :: IO Int32
        let align = elemAlignmentStd140 (Proxy :: Proxy a)
            stride = elemStrideStd140 (Proxy :: Proxy a)
            limit = fromIntegral . natVal $ (Proxy :: Proxy n)
            size = fromIntegral (align + (min (fromIntegral len) limit * stride))
        GLW.glNamedBufferSubData buffer 0 size (Foreign.castPtr ptr)
