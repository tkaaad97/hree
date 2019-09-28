{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Geometry
    ( Geometry(..)
    , addAttribBindings
    , addVerticesToGeometry
    , newGeometry
    , newSpriteGeometry
    , setIndexBuffer
    ) where

import Data.ByteString (ByteString)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Storable(..))
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import Graphics.Hree.Scene (addBuffer)
import Graphics.Hree.Types
import Linear (V2(..), V3(..))

newGeometry :: Geometry
newGeometry = Geometry Map.empty IntMap.empty Nothing 0

addAttribBindings :: Geometry -> Int -> Map ByteString AttribFormat -> (GLW.Buffer, BindBufferSetting) -> Geometry
addAttribBindings geo bindingIndex xs b = geo'
    where
    adding = Map.map (AttribBinding (GLW.BindingIndex $ fromIntegral bindingIndex)) xs
    attribBindings = Map.union (geometryAttribBindings geo) adding
    buffers = IntMap.insert bindingIndex b (geometryBuffers geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBuffers = buffers }

addVerticesToGeometry :: forall a. (Storable a, Vertex a) => Geometry -> Vector a -> GL.GLenum -> Scene -> IO Geometry
addVerticesToGeometry geometry storage usage scene = do
    buffer <- addBuffer scene (BufferSourceVector storage usage)
    let buffers' = IntMap.insert bindingIndex (buffer, bbs) buffers
    return (Geometry attribBindings' buffers' indexBuffer count)
    where
    Geometry attribBindings buffers indexBuffer c = geometry
    count = if c == 0 then Vector.length storage else c
    bindingIndex = maybe 0 (+ 1) . fmap fst . IntMap.lookupMax $ buffers
    VertexSpec bbs fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField _ format) = AttribBinding (GLW.BindingIndex . fromIntegral $ bindingIndex) format
    newAttribBindings = Map.fromList $ zip keys bindings
    attribBindings' = Map.union attribBindings newAttribBindings

setIndexBuffer :: Geometry -> IndexBuffer -> Geometry
setIndexBuffer geo indexBuffer = geo { geometryIndexBuffer = Just indexBuffer }

newSpriteGeometry :: Scene -> IO (Geometry, Vector SpriteOffset)
newSpriteGeometry scene = do
    geo <- addVerticesToGeometry newGeometry offsets GL.GL_STATIC_READ scene
    return (geo, offsets)
    where
    offsets = Vector.fromList
        [ SpriteOffset (V3 0 0 0) (V2 0 0)
        , SpriteOffset (V3 1 0 0) (V2 1 0)
        , SpriteOffset (V3 0 1 0) (V2 0 1)
        , SpriteOffset (V3 1 1 0) (V2 1 1)
        , SpriteOffset (V3 0 1 0) (V2 0 1)
        , SpriteOffset (V3 1 0 0) (V2 1 0)
        ]
