module Graphics.Hree.Geometry.Box
    ( createBoxGeometry
    ) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry (Geometry, newGeometry)
import Graphics.Hree.GL.Vertex (PositionAndNormal(..), Vertex)
import Graphics.Hree.Scene (Scene, addVerticesToGeometry)
import Linear (V3(..))

createBoxGeometry :: Int -> Int -> Int -> Scene -> IO (Geometry, Vector PositionAndNormal)
createBoxGeometry width height depth scene = do
    let geo = newGeometry . Vector.length $ vs
    geo' <- addVerticesToGeometry geo vs GL.GL_STATIC_READ scene
    return (geo', vs)
    where
    x = fromIntegral width
    y = fromIntegral height
    z = fromIntegral depth
    vs = Vector.fromList
        $  genBoxFace (V3 0 0 0) (V3 x 0 0) (V3 0 y 0) (V3 x y 0) (V3 0 0 (-1))
        ++ genBoxFace (V3 0 0 0) (V3 0 y 0) (V3 0 0 z) (V3 0 y z) (V3 (-1) 0 0)
        ++ genBoxFace (V3 0 0 0) (V3 0 0 z) (V3 x 0 0) (V3 x 0 z) (V3 0 (-1) 0)
        ++ genBoxFace (V3 x y z) (V3 0 y z) (V3 x 0 z) (V3 0 0 z) (V3 0 0 1)
        ++ genBoxFace (V3 x y z) (V3 x 0 z) (V3 x y 0) (V3 x 0 0) (V3 1 0 0)
        ++ genBoxFace (V3 x y z) (V3 x y 0) (V3 0 y z) (V3 0 y 0) (V3 0 1 0)

    genBoxFace a b c d n =
        [ PositionAndNormal a n
        , PositionAndNormal b n
        , PositionAndNormal c n
        , PositionAndNormal d n
        , PositionAndNormal c n
        , PositionAndNormal b n
        ]
