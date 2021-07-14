module FlatColor1 where

import qualified Data.Vector.Storable as Vector
import Example
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.FlatColorMaterial as Material
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "flat-color-1" init onDisplay

    where
    width  = 640
    height = 480

    x = 0.8
    vs = Vector.fromList
        [ Hree.BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Material.flatColorMaterial (V4 0 1 0 1.0)

    --proj = orthographic 0 1 0 1 (-10) 10
    proj = Hree.perspective 90 1.0 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        let geometry = Hree.addVerticesToGeometry Hree.newGeometry vs GL.GL_STREAM_DRAW
            mesh = Hree.Mesh geometry material Nothing
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.newNode{ Hree.nodeMesh = Just meshId } True
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
