{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Example
import qualified GLW
import qualified Graphics.Format.Tiled as Tiled (createNodesFromTiledMap)
import qualified Graphics.Format.Tiled.JSON as Tiled (loadTiledMap)
import qualified Graphics.GL as GL
import Graphics.Hree.Camera as Hree
import Graphics.Hree.Scene as Hree
import qualified Graphics.Hree.Types as Hree (Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import Prelude hiding (init)
import System.Environment (getArgs)
import System.FilePath (dropFileName)

main :: IO ()
main = do
    args <- getArgs
    path <- if null args
                then error "tiled file not specified."
                else return $ head args
    withWindow width height "tiled-1" (init path) onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = perspective 90 defaultAspect 0.0001 10000.0

    la = lookAt (V3 0 0 0.1) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        renderer <- newRenderer
        scene <- newScene
        load scene path
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (renderer, scene, camera)

    load scene path = do
        m <- Tiled.loadTiledMap path
        nodeIds <- Tiled.createNodesFromTiledMap scene (dropFileName path) m
        _ <- Hree.addNode scene Hree.newNode { Hree.nodeChildren = nodeIds } True
        return ()

    onDisplay (r, s, c) w = do
        render
        threadDelay 20000
        GLFW.pollEvents
        onDisplay (r, s, c) w

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
