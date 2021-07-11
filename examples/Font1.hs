{-# LANGUAGE OverloadedStrings #-}
module Font1 where

import qualified Data.Text as Text (pack, replace)
import Example
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Text as Hree (FontOption_(..),
                                             OriginLocation(..),
                                             TextOption_(..),
                                             createTextWithOption, newFontFace)
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import Prelude hiding (init)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    (fontPath, phrase, originLocation) <- case args of
        [fontPath, phrase] -> return (fontPath, phrase, Hree.OriginLocationTop)
        [fontPath, phrase, originLocation] -> return (fontPath, phrase, read originLocation)
        _ -> error "need two arguments (fontPath, phrase)"
    withWindow width height "font-1" (init fontPath phrase originLocation) onDisplay

    where
    width  = 800
    height = 600
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.orthographic 0 defaultAspect (-1) 0 0.01 10
    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init fontPath phrase originLocation w = do
        let fontOption = mempty
                { Hree.pixelHeight = pure 128
                }
            textOption = mempty
                { Hree.characterHeight = pure (1 / 16)
                , Hree.originLocation = pure originLocation
                }
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        font <- Hree.newFontFace fontPath
        nodeId <- Hree.createTextWithOption scene font (Text.replace "\\n" "\n" . Text.pack $ phrase) fontOption textOption
        Hree.addRootNodes scene (pure nodeId)
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
