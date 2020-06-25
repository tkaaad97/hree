{-# LANGUAGE OverloadedStrings #-}
module Tiled1 where

import Control.Concurrent (threadDelay)
import Example
import qualified Graphics.Format.Tiled.JSON as Tiled (LoadInfo(..),
                                                      loadTiledMap)
import Graphics.Hree as Hree
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import Prelude hiding (init)
import System.Environment (getArgs)

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

    proj = Hree.perspective 90 defaultAspect 0.1 10000.0

    la = Hree.lookAt (V3 0 0 0.1) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        load scene path
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera)

    load scene path = do
        Tiled.LoadInfo _ nodeIds <- Tiled.loadTiledMap scene path
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
