{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as BV
import Example
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Geometry.Box
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..), V4(..), inv44)
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "child-node-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    inverseBindMatrix = Linear.inv44 (V4 (V4 1 0 0 0) (V4 0 1 0 0.05) (V4 0 0 1 0) (V4 0 0 0 1))

    init w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- newScene
        (geometry, _) <- createBoxGeometry 0.5 0.1 0.1 scene
        let material = Material.flatColorMaterial (V4 0.1 0.1 0.95 1)
            childMaterial = Material.flatColorMaterial (V4 0.95 0.1 0.1 1)
            childMesh = Mesh geometry childMaterial Nothing
            mesh = Mesh geometry material Nothing
        childMeshId <- addMesh scene childMesh
        meshId <- addMesh scene mesh

        let childNode = newNode
                { nodeMesh = Just childMeshId
                , nodeTranslation = V3 0.5 0 0
                , nodeScale = V3 0.5 1 1
                , nodeInverseBindMatrix = inverseBindMatrix
                }
        childNodeId <- addNode scene childNode False

        let node = newNode
                { nodeMesh = Just meshId
                , nodeTranslation = V3 0 0 0
                , nodeChildren = BV.fromList [childNodeId]
                , nodeInverseBindMatrix = inverseBindMatrix
                }
        nodeId <- addNode scene node True

        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera, nodeId, childNodeId)

    deltaAngle = pi / 300

    onDisplay (s, c, n, cn) w = do
        render
        GLFW.pollEvents
        rotateNode s n (V3 0 0 1) deltaAngle
        rotateNode s cn (V3 0 0 1) deltaAngle
        onDisplay (s, c, n, cn) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    resizeWindow' camera _ w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio (Perspective fov _ near far) aspect =
        Perspective fov aspect near far
    updateProjectionAspectRatio p _ = p
