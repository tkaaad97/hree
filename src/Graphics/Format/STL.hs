module Graphics.Format.STL
    ( Triangle(..)
    , STL(..)
    , createGeometryFromSTL
    , loadGeometryFromFile
    ) where

import Data.Binary (Binary(..))
import qualified Data.Binary as Binary (decode)
import qualified Data.Binary.Get as Binary (getFloatle, getWord16le,
                                            getWord32le)
import qualified Data.Binary.Put as Binary (putFloatle, putWord16le,
                                            putWord32le)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry
import Graphics.Hree.GL.Vertex (BasicVertex(..))
import Graphics.Hree.Types (Scene)
import Linear (V2(..), V3(..), V4(..))

data Triangle = Triangle
    { triangleNormal  :: !(V3 Float)
    , triangleVertex1 :: !(V3 Float)
    , triangleVertex2 :: !(V3 Float)
    , triangleVertex3 :: !(V3 Float)
    , triangleExtra   :: !Word16
    } deriving (Show, Eq)

instance Binary Triangle where
    put (Triangle (V3 n0 n1 n2) (V3 v10 v11 v12) (V3 v20 v21 v22) (V3 v30 v31 v32) ex) = do
        Binary.putFloatle n0
        Binary.putFloatle n1
        Binary.putFloatle n2
        Binary.putFloatle v10
        Binary.putFloatle v11
        Binary.putFloatle v12
        Binary.putFloatle v20
        Binary.putFloatle v21
        Binary.putFloatle v22
        Binary.putFloatle v30
        Binary.putFloatle v31
        Binary.putFloatle v32
        Binary.putWord16le ex

    get = do
        n0 <- Binary.getFloatle
        n1 <- Binary.getFloatle
        n2 <- Binary.getFloatle
        v10 <- Binary.getFloatle
        v11 <- Binary.getFloatle
        v12 <- Binary.getFloatle
        v20 <- Binary.getFloatle
        v21 <- Binary.getFloatle
        v22 <- Binary.getFloatle
        v30 <- Binary.getFloatle
        v31 <- Binary.getFloatle
        v32 <- Binary.getFloatle
        ex <- Binary.getWord16le
        return (Triangle (V3 n0 n1 n2) (V3 v10 v11 v12) (V3 v20 v21 v22) (V3 v30 v31 v32) ex)

data STL = STL
    { stlHeader      :: !(Vector Word8)
    , stlTriangleNum :: !Word32
    , stlTriangles   :: !(Vector Triangle)
    } deriving (Show, Eq)

stlHeaderLength :: Int
stlHeaderLength = 80

instance Binary STL where
    put (STL header num triangles) = do
        Vector.mapM_ put header
        Binary.putWord32le num
        Vector.mapM_ put triangles

    get = do
        header <- Vector.replicateM stlHeaderLength get
        num <- Binary.getWord32le
        triangles <- Vector.replicateM (fromIntegral num) get
        return (STL header num triangles)

createGeometryFromSTL :: STL -> Scene -> IO (Geometry, SV.Vector BasicVertex)
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
                    _ -> triangleVertex3 triangle
            normal = triangleNormal triangle
        in BasicVertex position normal (V2 0 0) (V4 255 255 255 255)

loadGeometryFromFile :: FilePath -> Scene -> IO Geometry
loadGeometryFromFile path scene = do
    bs <- ByteString.readFile path
    let stl = Binary.decode bs
    fst <$> createGeometryFromSTL stl scene
