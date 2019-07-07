{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector.Storable as Vector
import Example
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.Geometry.Box
import Graphics.Hree.GL.Types
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))

main :: IO ()
main = do
    withWindow width height "transform-rotate-1" init onDisplay

    where
    width  = 640
    height = 480
    aspect = fromIntegral width / fromIntegral height

    proj = perspective 90 aspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- newScene
        (geometry, _) <- createBoxGeometry 0.5 0.5 0.5 scene
        let material = Material.flatColorMaterial (V4 0.2 0.4 0.6 1)
            mesh = Mesh geometry material Nothing
        meshId <- addMesh scene mesh
        nodeId <- addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera, nodeId)

    deltaAngle = pi / 300

    onDisplay (s, c, n) w = do
        render
        GLFW.pollEvents
        rotateNode s n (V3 0 0 1) deltaAngle
        onDisplay (s, c, n) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    resizeWindow' camera win w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio p @ Perspective {} aspect =
        p { perspectiveAspect = aspect }
    updateProjectionAspectRatio p _ = p
