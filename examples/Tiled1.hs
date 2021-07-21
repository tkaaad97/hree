{-# LANGUAGE OverloadedStrings #-}
module Tiled1 where

import qualified Chronos as Time (now)
import Control.Concurrent (threadDelay)
import qualified Data.Vector as BV (mapM_)
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
    width  = 800
    height = 600
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.perspective 90 defaultAspect 0.01 10000.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        taskBoard <- Hree.newSceneTaskBoard scene
        load scene taskBoard path
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    load scene board path = do
        t <- Time.now
        Tiled.LoadInfo _ nodeIds animations _ <- Tiled.loadTiledMap scene path
        _ <- Hree.addNode scene Hree.node { Hree.nodeChildren = nodeIds } Nothing True
        BV.mapM_ (addAnimationTask board t) animations
        return ()

    addAnimationTask board t animation =
        Hree.addSceneTask board (Hree.AnimationTask t animation (Hree.AnimationTaskOption True False Nothing))

    onDisplay (r, s, c, b) w = do
        t <- Time.now
        Hree.runSceneTasksOnBoard b t
        render
        threadDelay 100000
        GLFW.pollEvents
        onDisplay (r, s, c, b) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
