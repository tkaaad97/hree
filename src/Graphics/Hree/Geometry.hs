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
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import qualified Graphics.Rendering.OpenGL as GL

data Geometry = Geometry
    { geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBufferSources  :: !(IntMap BufferSource)
    , geometryIndexBuffer    :: !(Maybe (Vector GL.GLuint, GL.BufferUsage))
    , geometryCount          :: !Int
    }

empty :: Geometry
empty = Geometry Map.empty IntMap.empty Nothing 0

addAttribBindings :: Geometry -> Int -> Map ByteString AttribBinding -> BufferSource -> Geometry
addAttribBindings geo bindingIndex xs bufferSource = geo'
    where
    attribBindings = Map.union (geometryAttribBindings geo) xs
    sources = IntMap.insert bindingIndex bufferSource (geometryBufferSources geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBufferSources = sources }

fromVertexVector :: forall a. (Storable a, Vertex a) => Int -> Vector a -> GL.BufferUsage -> Geometry
fromVertexVector bindingIndex storage usage = Geometry attribBindings sources Nothing num
    where
    sources = IntMap.singleton bindingIndex $ BufferSource storage usage
    VertexSpec fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField name format setting) = AttribBinding bindingIndex format setting
    attribBindings = Map.fromList $ zip keys bindings
    num = Vector.length storage `div` 3
