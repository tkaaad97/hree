module Hree.Loader.STL
    ( Triangle(..)
    , STL(..)
    , createGeometryFromSTL
    , loadGeometryFromFile
    ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as ByteString (readFile)
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize (decode, getFloat32le, getWord16le,
                                              getWord32le, putFloat32le,
                                              putWord16le, putWord32le)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import qualified Graphics.GL as GL
import Hree.Geometry
import Hree.Vertex (BasicVertex(..))
import Linear (V2(..), V3(..), V4(..))

data Triangle = Triangle
    { triangleNormal  :: !(V3 Float)
    , triangleVertex1 :: !(V3 Float)
    , triangleVertex2 :: !(V3 Float)
    , triangleVertex3 :: !(V3 Float)
    , triangleExtra   :: !Word16
    } deriving (Show, Eq)

instance Serialize Triangle where
    put (Triangle (V3 n0 n1 n2) (V3 v10 v11 v12) (V3 v20 v21 v22) (V3 v30 v31 v32) ex) = do
        Serialize.putFloat32le n0
        Serialize.putFloat32le n1
        Serialize.putFloat32le n2
        Serialize.putFloat32le v10
        Serialize.putFloat32le v11
        Serialize.putFloat32le v12
        Serialize.putFloat32le v20
        Serialize.putFloat32le v21
        Serialize.putFloat32le v22
        Serialize.putFloat32le v30
        Serialize.putFloat32le v31
        Serialize.putFloat32le v32
        Serialize.putWord16le ex

    get = do
        n0 <- Serialize.getFloat32le
        n1 <- Serialize.getFloat32le
        n2 <- Serialize.getFloat32le
        v10 <- Serialize.getFloat32le
        v11 <- Serialize.getFloat32le
        v12 <- Serialize.getFloat32le
        v20 <- Serialize.getFloat32le
        v21 <- Serialize.getFloat32le
        v22 <- Serialize.getFloat32le
        v30 <- Serialize.getFloat32le
        v31 <- Serialize.getFloat32le
        v32 <- Serialize.getFloat32le
        ex <- Serialize.getWord16le
        return (Triangle (V3 n0 n1 n2) (V3 v10 v11 v12) (V3 v20 v21 v22) (V3 v30 v31 v32) ex)

data STL = STL
    { stlHeader      :: !(Vector Word8)
    , stlTriangleNum :: !Word32
    , stlTriangles   :: !(Vector Triangle)
    } deriving (Show, Eq)

stlHeaderLength :: Int
stlHeaderLength = 80

instance Serialize STL where
    put (STL header num triangles) = do
        Vector.mapM_ put header
        Serialize.putWord32le num
        Vector.mapM_ put triangles

    get = do
        header <- Vector.replicateM stlHeaderLength get
        num <- Serialize.getWord32le
        triangles <- Vector.replicateM (fromIntegral num) get
        return (STL header num triangles)

createGeometryFromSTL :: STL -> IO (Geometry, SV.Vector BasicVertex)
createGeometryFromSTL stl = do
    let len = fromIntegral (stlTriangleNum stl) * 3
        vs = SV.generate len f
        geo = addVerticesToGeometry emptyGeometry vs GL.GL_STATIC_READ
    return (geo, vs)
    where
    f i =
        let (d, m) = divMod i 3
            triangles = stlTriangles stl
            triangle = triangles Vector.! d
            position = case m of
                    0 -> triangleVertex1 triangle
                    1 -> triangleVertex2 triangle
                    _ -> triangleVertex3 triangle
            normal = triangleNormal triangle
        in BasicVertex position normal (V2 0 0) (V4 255 255 255 255)

loadGeometryFromFile :: FilePath -> IO Geometry
loadGeometryFromFile path = do
    bs <- ByteString.readFile path
    stl <- either (throwIO . userError) return $ Serialize.decode bs
    fst <$> createGeometryFromSTL stl
