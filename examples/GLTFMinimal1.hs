{-# LANGUAGE OverloadedStrings #-}
module GLTFMinimal1 where

import Control.Monad (void)
import Example
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import qualified Hree.Loader.GLTF as GLTF (loadSceneFromFile)
import Linear (V3(..))
import Prelude hiding (init)

main :: IO ()
main = do
    let path = "examples/models/gltf/minimal1.gltf"
    withWindow width height "gltf-minimal-1" (init path) onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.perspective 90 defaultAspect 0.1 1000.0

    la = Hree.lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        void $ GLTF.loadSceneFromFile path scene
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera)

    onDisplay (r, s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (r, s, c) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
