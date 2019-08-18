{-# LANGUAGE OverloadedStrings #-}
module Graphics.Format.GLTF
    (
    ) where

import qualified Data.Aeson as Aeson (FromJSON(..), withObject, (.!=), (.:),
                                      (.:?))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as BV (Vector)

data Buffer = Buffer
    { bufferByteLength :: !Int
    , bufferUri        :: !Text
    } deriving (Show, Eq)

data BufferView = BufferView
    { bufferViewBufferId :: !Int
    , bufferViewLength   :: !Int
    , bufferViewOffset   :: !Int
    , bufferViewName     :: !(Maybe Text)
    } deriving (Show, Eq)

data Accessor = Accessor
    { accessorBufferView    :: !Int
    , accessorByteOffset    :: !Int
    , accessorComponentType :: !Int
    , accessorNormalized    :: !Bool
    , accessorCount         :: !Int
    , accessorType          :: !Text
    , accessorName          :: !(Maybe Text)
    } deriving (Show, Eq)

data Mesh = Mesh
    { meshPrimitives :: !(BV.Vector Primitive)
    , meshName       :: !(Maybe Text)
    } deriving (Show, Eq)

data Primitive = Primitive
    { primitiveAttributes :: !(Map Text Int)
    , primitiveIndices    :: !(Maybe Int)
    , primitiveMaterial   :: !(Maybe Int)
    , primitiveMode       :: !(Maybe Int)
    } deriving (Show, Eq)

instance Aeson.FromJSON Buffer where
    parseJSON = Aeson.withObject "Buffer" $ \v -> do
        len <- v Aeson..: "byteLength"
        uri <- v Aeson..: "uri"
        return $ Buffer len uri

instance Aeson.FromJSON BufferView where
    parseJSON = Aeson.withObject "BufferView" $ \v -> do
        buffer <- v Aeson..: "buffer"
        offset <- v Aeson..:? "byteOffset" Aeson..!= 0
        len <- v Aeson..: "byteLength"
        name <- v Aeson..:? "name"
        return $ BufferView buffer len offset name

instance Aeson.FromJSON Accessor where
    parseJSON = Aeson.withObject "Accessor" $ \v -> do
        bufferView <- v Aeson..: "bufferView"
        offset <- v Aeson..:? "byteOffset" Aeson..!= 0
        componentType <- v Aeson..: "componentType"
        normalized <- v Aeson..:? "normalized" Aeson..!= False
        count <- v Aeson..: "count"
        type' <- v Aeson..: "type"
        name <- v Aeson..:? "name"
        return $ Accessor bufferView offset componentType normalized count type' name

instance Aeson.FromJSON Mesh where
    parseJSON = Aeson.withObject "Mesh" $ \v -> do
        primitives <- v Aeson..: "primitives"
        name <- v Aeson..:? "name"
        return $ Mesh primitives name

instance Aeson.FromJSON Primitive where
    parseJSON = Aeson.withObject "Primitive" $ \v -> do
        attributes <- v Aeson..: "attributes"
        indices <- v Aeson..:? "indices"
        material <- v Aeson..:? "material"
        mode <- v Aeson..:? "mode"
        return $ Primitive attributes indices material mode