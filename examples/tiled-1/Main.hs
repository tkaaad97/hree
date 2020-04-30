{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Example
import qualified Graphics.Format.Tiled as Tiled (createNodesFromTiledMap)
import qualified Graphics.Format.Tiled.JSON as Tiled (loadTiledMap)
import qualified Graphics.GL as GL
import Graphics.Hree as Hree
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

    proj = Hree.perspective 90 defaultAspect 0.0001 10000.0

    la = Hree.lookAt (V3 0 0 0.1) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        GL.glEnable GL.GL_BLEND
        GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        load scene path
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
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
            Hree.renderScene r s c
            GLFW.swapBuffers w
