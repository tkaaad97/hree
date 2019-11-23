module Graphics.Hree.GL.Block
    ( Block(..)
    ) where

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign (Ptr)
import qualified Foreign (Storable(..), plusPtr)

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
