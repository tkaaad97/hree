module Main where

import qualified Data.Vector.Storable as Vector
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Mesh (Mesh(..))
import Graphics.Hree.Scene
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Sample

main :: IO ()
main = do
    withWindow width height "canvas-sample" init onDisplay

    putStrLn "ended!"

    where
    width  = 640
    height = 480

    x = 1.0
    vs = Vector.fromList
        [ BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    geometry = Geometry.fromVertexVector 0 vs GL.StreamDraw

    material = Material.basicMaterial

    mesh = Mesh geometry material

    proj = orthographic 0 1 0 1 (-10) 10

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init = do
        scene <- newScene
        putStrLn "newScene"
        addMesh scene mesh
        putStrLn "addMesh"
        camera <- newCamera proj la
        putStrLn "newCamera"
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w
