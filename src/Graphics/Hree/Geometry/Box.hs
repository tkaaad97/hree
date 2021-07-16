module Graphics.Hree.Geometry.Box
    ( createBoxGeometry
    ) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified Graphics.GL as GL
import Graphics.Hree.Geometry (Geometry, addVerticesToGeometry, newGeometry)
import Graphics.Hree.GL.Vertex (PositionAndNormal(..))
import Linear (V3(..))

createBoxGeometry :: Float -> Float -> Float -> (Geometry, Vector PositionAndNormal)
createBoxGeometry width height depth =
    let geo = addVerticesToGeometry newGeometry vs GL.GL_STATIC_READ
    in (geo, vs)
    where
    x = width
    y = height
    z = depth
    vs = Vector.fromList
        $  genBoxFace (V3 0 0 0) (V3 0 y 0) (V3 x 0 0) (V3 x y 0) (V3 0 0 (-1))
        ++ genBoxFace (V3 0 0 0) (V3 0 0 z) (V3 0 y 0) (V3 0 y z) (V3 (-1) 0 0)
        ++ genBoxFace (V3 0 0 0) (V3 x 0 0) (V3 0 0 z) (V3 x 0 z) (V3 0 (-1) 0)
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
