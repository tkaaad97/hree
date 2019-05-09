module Graphics.Hree.Geometry
    ( Geometry(..)
    , addAttribBindings
    , newGeometry
    ) where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types

data Geometry = Geometry
    { geometryAttribBindings :: !(Map ByteString AttribBinding)
    , geometryBuffers        :: !(IntMap (GLW.Buffer, BindBufferSetting))
    , geometryIndexBuffer    :: !(Maybe GLW.Buffer)
    , geometryCount          :: !Int
    } deriving (Show)

newGeometry :: Int -> Geometry
newGeometry = Geometry Map.empty IntMap.empty Nothing

addAttribBindings :: Geometry -> Int -> Map ByteString AttribBinding -> (GLW.Buffer, BindBufferSetting) -> Geometry
addAttribBindings geo bindingIndex xs b = geo'
    where
    attribBindings = Map.union (geometryAttribBindings geo) xs
    buffers = IntMap.insert bindingIndex b (geometryBuffers geo)
    geo' = geo { geometryAttribBindings = attribBindings, geometryBuffers = buffers }
