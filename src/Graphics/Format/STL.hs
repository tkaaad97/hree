{-# LANGUAGE StrictData #-}
module Graphics.Format.STL
    (
    ) where

import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry
import Graphics.Hree.GL.Vertex (PositionAndNormal(..))
import Graphics.Hree.Types (Scene)
import Linear (V3)

data Triangle = Triangle
    { triangleNormal  :: V3 Float
    , triangleVertex1 :: V3 Float
    , triangleVertex2 :: V3 Float
    , triangleVertex3 :: V3 Float
    , triangleExtra   :: Word16
    } deriving (Show, Eq)

instance Binary Triangle where
    put (Triangle normal v1 v2 v3 ex) = do
        put normal
        put v1
        put v2
        put v3
        put ex

    get = do
        normal <- get
        v1 <- get
        v2 <- get
        v3 <- get
        ex <- get
        return (Triangle normal v1 v2 v3 ex)

data STL = STL
    { stlHeader      :: Vector Word8
    , stlTriangleNum :: Word32
    , stlTriangles   :: Vector Triangle
    } deriving (Show, Eq)

stlHeaderLength :: Int
stlHeaderLength = 80

instance Binary STL where
    put (STL header num triangles) = do
        Vector.mapM_ put header
        put num
        Vector.mapM_ put triangles

    get = do
        header <- Vector.replicateM stlHeaderLength get
        num <- get
        triangles <- Vector.replicateM (fromIntegral num) get
        return (STL header num triangles)

createGeometryFromSTL :: STL -> Scene -> IO (Geometry, SV.Vector PositionAndNormal)
createGeometryFromSTL stl scene = do
    let len = fromIntegral (stlTriangleNum stl) * 3
        vs = SV.generate len f
        geo = newGeometry
    geo' <- addVerticesToGeometry geo vs GL.GL_STATIC_READ scene
    return (geo', vs)
    where
    f i =
        let (d, m) = divMod i 3
            triangles = stlTriangles stl
            triangle = triangles Vector.! d
            position = case m of
                    0 -> triangleVertex1 triangle
                    1 -> triangleVertex2 triangle
                    2 -> triangleVertex3 triangle
            normal = triangleNormal triangle
        in PositionAndNormal position normal
