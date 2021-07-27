{-# LANGUAGE ScopedTypeVariables #-}
module Hree.Geometry
    ( Geometry(..)
    , addAttribBindings
    , addVerticesToGeometry
    , emptyGeometry
    , spriteGeometry
    , spriteGeometryVertices
    , setIndexBufferSource
    , setIndexBufferSourceUByte
    , setIndexBufferSourceUShort
    , setIndexBufferSourceUInt
    ) where

import Data.ByteString (ByteString)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word16, Word32, Word8)
import qualified GLW
import qualified Graphics.GL as GL
import Hree.GL.Types (AttribBinding(AttribBinding), AttributeFormat,
                      BindBufferSetting, BufferSource(BufferSourceVector),
                      IndexBufferSource(IndexBufferSource))
import Hree.Types (Geometry(..))
import Hree.Vertex (SpriteOffset(SpriteOffset), Vertex(..),
                    VertexField(VertexField, vertexFieldAttribName),
                    VertexSpec(VertexSpec))
import Linear (V2(..), V3(..))

emptyGeometry :: Geometry
emptyGeometry = Geometry Map.empty IntMap.empty Nothing 0

addAttribBindings :: Geometry -> Map ByteString AttributeFormat -> (BufferSource, BindBufferSetting) -> Geometry
addAttribBindings geo xs b = geo'
    where
    bufferSources = geometryBufferSources geo
    bindingIndex = (+ 1) . lookupMaxKey 0 $ bufferSources
    adding = Map.map (AttribBinding (GLW.BindingIndex $ fromIntegral bindingIndex)) xs
    attribBindings = Map.union (geometryAttribBindings geo) adding
    bufferSources' = IntMap.insert bindingIndex b (geometryBufferSources geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBufferSources = bufferSources' }
    lookupMaxKey d m
        | IntMap.null m = d
        | otherwise = fst . IntMap.findMax $ m

addVerticesToGeometry :: forall a. (Vertex a) => Geometry -> Vector a -> GL.GLenum -> Geometry
addVerticesToGeometry geometry storage usage =
    let bufferSource = BufferSourceVector storage usage
        bufferSources' = IntMap.insert bindingIndex (bufferSource, bbs) bufferSources
    in Geometry attribBindings' bufferSources' indexBuffer count
    where
    Geometry attribBindings bufferSources indexBuffer c = geometry
    count = if c == 0 then Vector.length storage else c
    bindingIndex = (+ 1) . lookupMaxKey 0 $ bufferSources
    VertexSpec bbs fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField _ format) = AttribBinding (GLW.BindingIndex . fromIntegral $ bindingIndex) format
    newAttribBindings = Map.fromList $ zip keys bindings
    attribBindings' = Map.union attribBindings newAttribBindings
    lookupMaxKey d m
        | IntMap.null m = d
        | otherwise = fst . IntMap.findMax $ m

setIndexBufferSource :: Geometry -> IndexBufferSource -> Geometry
setIndexBufferSource geo indexBufferSource = geo { geometryIndexBufferSource = Just indexBufferSource }

setIndexBufferSourceUByte :: Geometry -> Vector Word8 -> Geometry
setIndexBufferSourceUByte geo v = setIndexBufferSource geo indexBufferSource
    where
    s = BufferSourceVector v GL.GL_STATIC_READ
    l = fromIntegral (Vector.length v)
    indexBufferSource = IndexBufferSource s GL.GL_UNSIGNED_BYTE l 0

setIndexBufferSourceUShort :: Geometry -> Vector Word16 -> Geometry
setIndexBufferSourceUShort geo v = setIndexBufferSource geo indexBufferSource
    where
    s = BufferSourceVector v GL.GL_STATIC_READ
    l = fromIntegral (Vector.length v)
    indexBufferSource = IndexBufferSource s GL.GL_UNSIGNED_SHORT l 0

setIndexBufferSourceUInt :: Geometry -> Vector Word32 -> Geometry
setIndexBufferSourceUInt geo v = setIndexBufferSource geo indexBufferSource
    where
    s = BufferSourceVector v GL.GL_STATIC_READ
    l = fromIntegral (Vector.length v)
    indexBufferSource = IndexBufferSource s GL.GL_UNSIGNED_INT l 0

spriteGeometry :: Geometry
spriteGeometry = addVerticesToGeometry emptyGeometry spriteGeometryVertices GL.GL_STATIC_READ

spriteGeometryVertices :: Vector SpriteOffset
spriteGeometryVertices = Vector.fromList
    [ SpriteOffset (V3 0 0 0) (V2 0 0)
    , SpriteOffset (V3 1 0 0) (V2 1 0)
    , SpriteOffset (V3 0 1 0) (V2 0 1)
    , SpriteOffset (V3 1 1 0) (V2 1 1)
    , SpriteOffset (V3 0 1 0) (V2 0 1)
    , SpriteOffset (V3 1 0 0) (V2 1 0)
    ]
