{-# LANGUAGE CPP #-}
module Graphics.Format.GLTF.Draco
#ifndef ENABLE_DRACO
    () where
#else
    ( Decoder
    , DecoderBuffer
    , PointCloud
    , Mesh
    , PointAttribute
    , encodedGeometryTypePointCloud
    , encodedGeometryTypeTriangularMesh
    , newDecoder
    , deleteDecoder
    , newDecoderBuffer
    , deleteDecoderBuffer
    , newPointCloud
    , deletePointCloud
    , newMesh
    , deleteMesh
    , getEncodedGeometryType
    , castMeshToPointCloud
    , decodeBufferToPointCloud
    , decodeBufferToMesh
    , getAttributeByUniqueId
    , getAttributeFloatArrayForAllPoints
    , getAttributeInt8ArrayForAllPoints
    , getAttributeUInt8ArrayForAllPoints
    , getAttributeInt16ArrayForAllPoints
    , getAttributeUInt16ArrayForAllPoints
    , getAttributeInt32ArrayForAllPoints
    , getAttributeUInt32ArrayForAllPoints
    , getNumFaces
    , getIndices
    , ok
    , errorMsg
    ) where

import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Foreign (Ptr)
import Foreign.C (CChar(..), CInt(..), CLong(..), CSize(..), CString)

data Decoder
data DecoderBuffer
data Mesh
data PointCloud
data PointAttribute
data Status

encodedGeometryTypePointCloud :: CInt
encodedGeometryTypePointCloud = 0;

encodedGeometryTypeTriangularMesh :: CInt
encodedGeometryTypeTriangularMesh = 1;

foreign import ccall "draco_newDecoder" newDecoder
    :: IO (Ptr Decoder)

foreign import ccall "draco_deleteDecoder" deleteDecoder
    :: Ptr Decoder -> IO ()

foreign import ccall "draco_newDecoderBuffer" newDecoderBuffer
    :: Ptr CChar -> CSize -> IO (Ptr DecoderBuffer)

foreign import ccall "draco_deleteDecoderBuffer" deleteDecoderBuffer
    :: Ptr DecoderBuffer -> IO ()

foreign import ccall "draco_newMesh" newMesh
    :: IO (Ptr Mesh)

foreign import ccall "draco_deleteMesh" deleteMesh
    :: Ptr Mesh -> IO ()

foreign import ccall "draco_newPointCloud" newPointCloud
    :: IO (Ptr PointCloud)

foreign import ccall "draco_deletePointCloud" deletePointCloud
    :: Ptr PointCloud -> IO ()

foreign import ccall "draco_castMeshToPointCloud" castMeshToPointCloud
    :: Ptr Mesh -> IO (Ptr PointCloud)

foreign import ccall "draco_getEncodedGeometryType" getEncodedGeometryType
    :: Ptr Decoder -> Ptr DecoderBuffer -> IO CInt

foreign import ccall "draco_decodeBufferToPointCloud" decodeBufferToPointCloud
    :: Ptr Decoder -> Ptr DecoderBuffer -> Ptr PointCloud -> IO (Ptr Status)

foreign import ccall "draco_decodeBufferToMesh" decodeBufferToMesh
    :: Ptr Decoder -> Ptr DecoderBuffer -> Ptr Mesh -> IO (Ptr Status)

foreign import ccall "draco_getAttribute" getAttribute
    :: Ptr PointCloud -> CLong -> IO (Ptr PointAttribute)

foreign import ccall "draco_getAttributeByUniqueId" getAttributeByUniqueId
    :: Ptr PointCloud -> CLong -> IO (Ptr PointAttribute)

foreign import ccall "draco_getAttributeFloatArrayForAllPoints" getAttributeFloatArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeInt8ArrayForAllPoints" getAttributeInt8ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeUInt8ArrayForAllPoints" getAttributeUInt8ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeInt16ArrayForAllPoints" getAttributeInt16ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeUInt16ArrayForAllPoints" getAttributeUInt16ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeInt32ArrayForAllPoints" getAttributeInt32ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getAttributeUInt32ArrayForAllPoints" getAttributeUInt32ArrayForAllPoints
    :: Ptr PointCloud -> Ptr PointAttribute -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_getNumFaces" getNumFaces
    :: Ptr Mesh -> IO CSize

foreign import ccall "draco_getIndices" getIndices
    :: Ptr Mesh -> CInt -> Ptr () -> IO Bool

foreign import ccall "draco_ok" ok
    :: Ptr Status -> IO Bool

foreign import ccall "draco_error_msg" errorMsg
    :: Ptr Status -> IO CString

#endif
