module Hree.Geometry.Box
    ( boxGeometry
    , boxGeometryVertices
    ) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified Graphics.GL as GL
import Hree.Geometry (Geometry, addVerticesToGeometry, emptyGeometry)
import Hree.GL.Vertex (PositionAndNormal(..))
import Linear (V3(..))

boxGeometry :: Float -> Float -> Float -> Geometry
boxGeometry width height depth =
    addVerticesToGeometry emptyGeometry (boxGeometryVertices width height depth) GL.GL_STATIC_READ

boxGeometryVertices :: Float -> Float -> Float -> Vector PositionAndNormal
boxGeometryVertices width height depth = vs
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
