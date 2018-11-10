{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Hree.Geometry
    ( Geometry(..)
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
    { geometryId             :: !Int
    , geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBufferSources  :: !(IntMap BufferSource)
    , geometryIndexBuffer    :: !(Maybe (Vector GL.GLushort, GL.BufferUsage))
    , geometryCount          :: !Int
    }

addAttribBindings :: Geometry -> Int -> Map ByteString (AttribInfo, BindBufferSetting) -> BufferSource -> Geometry
addAttribBindings geo bindingIndex settings bufferSource = geo'
    where
    xs = Map.map (uncurry $ AttribBinding bindingIndex) settings
    attribBindings = Map.union (geometryAttribBindings geo) xs
    sources = IntMap.insert bindingIndex bufferSource (geometryBufferSources geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBufferSources = sources }

fromVertexVector :: forall a. (Storable a, Vertex a) => Int -> Int -> Vector a -> GL.BufferUsage -> Geometry
fromVertexVector index bindingIndex storage usage = Geometry index attribBindings sources Nothing (Vector.length storage)
    where
    sources = IntMap.singleton bindingIndex $ BufferSource storage usage
    VertexSpec fields = vertexSpec (Proxy :: Proxy a)
    keys = map vertexFieldAttribName fields
    bindings = map toAttribBinding fields
    toAttribBinding (VertexField name format setting) = AttribBinding bindingIndex (AttribInfo name format) setting
    attribBindings = Map.fromList $ zip keys bindings
