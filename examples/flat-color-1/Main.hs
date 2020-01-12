module Main where

import qualified Data.Vector.Storable as Vector
import Example
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
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
        renderer <- newRenderer
        scene <- newScene
        geometry <- Geometry.addVerticesToGeometry Geometry.newGeometry vs GL.GL_STREAM_DRAW scene
        let mesh = Mesh geometry material Nothing
        meshId <- addMesh scene mesh
        _ <- addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            renderScene r s c
            GLFW.swapBuffers w
