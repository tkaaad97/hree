module Graphics.Format.STL
    ( Triangle(..)
    , STL(..)
    , createGeometryFromSTL
    , loadGeometryFromSTLFile
    ) where

import Data.Binary (Binary(..), Get(..))
import qualified Data.Binary as Binary (decode)
import qualified Data.Binary.Get as Binary (getFloatle, getWord16le,
                                            getWord32le)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SV
import Data.Word (Word16, Word32, Word8)
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry
import Graphics.Hree.GL.Vertex (PositionAndNormal(..))
import Graphics.Hree.Types (Scene)
import Linear (V3(..))

data Triangle = Triangle
    { triangleNormal  :: !(V3 Float)
    , triangleVertex1 :: !(V3 Float)
    , triangleVertex2 :: !(V3 Float)
    , triangleVertex3 :: !(V3 Float)
    , triangleExtra   :: !Word16
    } deriving (Show, Eq)

instance Binary Triangle where
    put (Triangle normal v1 v2 v3 ex) = do
        put normal
        put v1
        put v2
        put v3
        put ex

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
        put num
        Vector.mapM_ put triangles

    get = do
        header <- Vector.replicateM stlHeaderLength get
        num <- Binary.getWord32le
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

loadGeometryFromSTLFile :: FilePath -> Scene -> IO (Geometry, SV.Vector PositionAndNormal)
loadGeometryFromSTLFile path scene = do
    bs <- ByteString.readFile path
    let stl = Binary.decode bs
    createGeometryFromSTL stl scene
