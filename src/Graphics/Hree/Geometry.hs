{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Geometry
    ( Geometry(..)
    , addAttribBindings
    , empty
    , fromVertexVector
    ) where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign.Storable (Storable)
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex

data Geometry = Geometry
    { geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBuffers        :: !(IntMap (BufferSource, BindBufferSetting))
    , geometryIndexBuffer    :: !(Maybe (Vector GL.GLuint, GL.GLenum))
    , geometryCount          :: !Int
    }

empty :: Geometry
empty = Geometry Map.empty IntMap.empty Nothing 0

addAttribBindings :: Geometry -> Int -> Map ByteString AttribBinding -> (BufferSource, BindBufferSetting) -> Geometry
addAttribBindings geo bindingIndex xs b = geo'
    where
    attribBindings = Map.union (geometryAttribBindings geo) xs
    buffers = IntMap.insert bindingIndex b (geometryBuffers geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBuffers = buffers }

fromVertexVector :: forall a. (Storable a, Vertex a) => GLW.BindingIndex -> Vector a -> GL.GLenum -> Geometry
fromVertexVector bindingIndex storage usage = Geometry attribBindings buffers Nothing num
    where
    VertexSpec bbs fields = vertexSpec (Proxy :: Proxy a)
    buffers = IntMap.singleton (fromIntegral . GLW.unBindingIndex $ bindingIndex) (BufferSource storage usage, bbs)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField name format) = AttribBinding bindingIndex format
    attribBindings = Map.fromList $ zip keys bindings
    num = Vector.length storage
