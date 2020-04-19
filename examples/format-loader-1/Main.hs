{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Chronos as Time (now)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (void)
import qualified Data.Vector as BV (head, null)
import Example
import qualified GLW
import qualified Graphics.Format.GLTF as GLTF (Supplement(..),
                                               loadSceneFromFile)
import qualified Graphics.Format.PLY as PLY (loadGeometryFromFile)
import qualified Graphics.Format.STL as STL (loadGeometryFromFile)
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.Light (directionalLight)
import qualified Graphics.Hree.Material.BasicMaterial as Material
import Graphics.Hree.Scene
import qualified Graphics.Hree.SceneTask as SceneTask
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

    proj = perspective 90 defaultAspect 0.0001 10000.0

    la = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        renderer <- newRenderer
        scene <- newScene
        a <- loadScene path scene (FilePath.takeExtension path)
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (renderer, scene, camera, a)

    loadScene path scene ".gltf" = do
        (_, sup) <- GLTF.loadSceneFromFile path scene
        let light = directionalLight (V3 0.5 (-1) (-0.5)) (V3 1 1 1) 1
            animations = GLTF.supplementAnimations sup
        _ <- addLight scene light
        if BV.null animations
            then return Nothing
            else do
                st <- Time.now
                taskBoard <- SceneTask.newSceneTaskBoard scene
                _ <- SceneTask.addSceneTask taskBoard (SceneTask.AnimationTask st (BV.head animations) (SceneTask.AnimationTaskOptions True False))
                return (Just taskBoard)

    loadScene path scene extension = do
        geometry <- case extension of
                        ".stl" -> STL.loadGeometryFromFile path scene
                        ".ply" -> PLY.loadGeometryFromFile path scene
                        _ -> throwIO . userError $ "unknown format. path: " ++ path
        let material = Material.basicMaterial $ V3 0.5 (-1) (-0.5)
            mesh = Mesh geometry material Nothing
        meshId <- addedMeshId <$> addMesh scene mesh
        void $ addNode scene newNode{ nodeMesh = Just meshId } True
        return Nothing

    onDisplay (r, s, c, Just taskBoard) w = do
        render
        threadDelay 20000
        GLFW.pollEvents
        t <- Time.now
        SceneTask.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, Just taskBoard) w

        where
        render = do
            renderScene r s c
            GLFW.swapBuffers w

    onDisplay (r, s, c, Nothing) w = do
        render
        threadDelay 20000
        GLFW.pollEvents
        onDisplay (r, s, c, Nothing) w

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
