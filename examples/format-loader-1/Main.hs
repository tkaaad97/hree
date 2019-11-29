{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (throwIO)
import Control.Monad (void)
import Example
import qualified GLW
import qualified Graphics.Format.GLTF as GLTF (loadSceneFromFile)
import qualified Graphics.Format.PLY as PLY (loadGeometryFromFile)
import qualified Graphics.Format.STL as STL (loadGeometryFromFile)
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Scene
import Graphics.Hree.Types (Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import Prelude hiding (init)
import System.Environment (getArgs)
import qualified System.FilePath as FilePath (takeExtension)

main :: IO ()
main = do
    args <- getArgs
    let path = if null args then "examples/models/stl/binary/monkey.stl" else head args
    withWindow width height "format-loader-1" (init path) onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = perspective 90 defaultAspect 0.001 1000.0

    la = lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- newScene
        loadScene path scene (FilePath.takeExtension path)
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera)

    loadScene path scene ".gltf" =
        void $ GLTF.loadSceneFromFile path scene

    loadScene path scene extension = do
        geometry <- case extension of
                        ".stl" -> STL.loadGeometryFromFile path scene
                        ".ply" -> PLY.loadGeometryFromFile path scene
                        _ -> throwIO . userError $ "unknown format. path: " ++ path
        let material = Material.basicMaterial
                `Material.setDirectionalLight` V3 0.5 (-1) (-0.5)
            mesh = Mesh geometry material Nothing
        meshId <- addMesh scene mesh
        void $ addNode scene newNode{ nodeMesh = Just meshId } True

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

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

    updateProjectionAspectRatio (PerspectiveProjection (Perspective fov _ near far)) aspect =
        PerspectiveProjection $ Perspective fov aspect near far
    updateProjectionAspectRatio p _ = p
