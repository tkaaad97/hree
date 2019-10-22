{-# LANGUAGE OverloadedStrings #-}
module Main where

import Example
import qualified GLW
import qualified Graphics.Format.STL as STL (loadGeometryFromFile)
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import System.Environment (getArgs)
import qualified System.FilePath as FilePath (takeExtension)

main :: IO ()
main = do
    args <- getArgs
    let path = "examples/models/stl/binary/monkey.stl"
        (metalness, roughness) = case args of
            []        -> (0.5, 0.5)
            [a]       -> (read a, 0.5)
            a : b : _ -> (read a, read b)
    putStrLn $ "metalness: " ++ show metalness
    putStrLn $ "roughness: " ++ show roughness
    withWindow width height "standard-material-1" (init path metalness roughness) onDisplay

    where
    width  = 640
    height = 480
    aspect = fromIntegral width / fromIntegral height

    proj = perspective 90 aspect 0.1 1000.0

    la = lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

    init path metalness roughness w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- newScene
        geometry <- STL.loadGeometryFromFile path scene
        let material = Material.standardMaterial metalness roughness Nothing
            material' = Material.setDirectionalLight material (V3 (-1) 0 (-5))
            mesh = Mesh geometry material' Nothing
        meshId <- addMesh scene mesh
        addNode scene newNode{ nodeMesh = Just meshId } True
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

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
