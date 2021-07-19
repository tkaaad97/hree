{-# LANGUAGE OverloadedStrings #-}
module TransformRotate1 where

import Example
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.FlatColorMaterial as Material
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "transform-rotate-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.perspective 90 defaultAspect 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        let geometry = Hree.boxGeometry 0.5 0.5 0.5
            material = Material.flatColorMaterial (V4 0.2 0.4 0.6 1)
        materialId <- Hree.addMaterial scene material
        let mesh = Hree.Mesh geometry materialId Nothing
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        nodeId <- Hree.addNode scene Hree.newNode{ Hree.nodeMesh = Just meshId } True
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, nodeId)

    deltaAngle = pi / 300

    onDisplay (r, s, c, n) w = do
        render
        GLFW.pollEvents
        Hree.rotateNode s n (V3 0 0 1) deltaAngle
        onDisplay (r, s, c, n) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
