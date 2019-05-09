module Main where

import qualified Data.Vector.Storable as Vector
import Example
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.CameraControl.SphericalControl
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Mesh (Mesh(..))
import Graphics.Hree.Scene
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))

main :: IO ()
main = do
    withWindow width height "canvas-sample" init onDisplay

    where
    width  = 640
    height = 480

    x = 0.8
    vs = Vector.fromList
        [ BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Material.flatColorMaterial (V4 0 1 0 1.0)

    --proj = orthographic 0 1 0 1 (-10) 10
    proj = perspective 90 1.0 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        scene <- newScene
        geometry <- addVerticesToGeometry (Geometry.newGeometry 6) vs GL.GL_STREAM_DRAW scene
        let mesh = Mesh geometry material
        addMesh scene mesh
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w
