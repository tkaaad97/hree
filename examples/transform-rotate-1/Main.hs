{-# LANGUAGE OverloadedStrings #-}
module Main where

import Example
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Geometry.Box
import qualified Graphics.Hree.Material.FlatColorMaterial as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
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

    proj = perspective 90 defaultAspect 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        renderer <- newRenderer
        scene <- newScene
        (geometry, _) <- createBoxGeometry 0.5 0.5 0.5 scene
        let material = Material.flatColorMaterial (V4 0.2 0.4 0.6 1)
            mesh = Mesh geometry material Nothing
        meshId <- addedMeshId <$> addMesh scene mesh
        nodeId <- addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (renderer, scene, camera, nodeId)

    deltaAngle = pi / 300

    onDisplay (r, s, c, n) w = do
        render
        GLFW.pollEvents
        rotateNode s n (V3 0 0 1) deltaAngle
        onDisplay (r, s, c, n) w

        where
        render = do
            renderScene r s c
            GLFW.swapBuffers w

    resizeWindow' camera _ w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio (PerspectiveProjection (Perspective fov _ near far)) aspect =
        PerspectiveProjection $ Perspective fov aspect near far
    updateProjectionAspectRatio p _ = p

