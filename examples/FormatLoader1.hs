{-# LANGUAGE OverloadedStrings #-}
module FormatLoader1 where

import qualified Chronos as Time (now)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (void)
import qualified Data.Vector as BV (head, null)
import Example
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import qualified Hree.Loader.GLTF as GLTF (Supplement(..), loadSceneFromFile)
import qualified Hree.Loader.PLY as PLY (loadGeometryFromFile)
import qualified Hree.Loader.STL as STL (loadGeometryFromFile)
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

    proj = Hree.perspective 90 defaultAspect 0.01 10000.0

    la = Hree.lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        a <- loadScene path scene (FilePath.takeExtension path)
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, a)

    loadScene path scene extension
        | extension == ".glb" || extension == ".gltf" = do
            (_, sup) <- GLTF.loadSceneFromFile path scene
            let light = Hree.directionalLight (V3 0.5 (-1) (-0.5)) (V3 1 1 1) 1
                animations = GLTF.supplementAnimations sup
            _ <- Hree.addLight scene light
            if BV.null animations
                then return Nothing
                else do
                    st <- Time.now
                    taskBoard <- Hree.newSceneTaskBoard scene
                    _ <- Hree.addSceneTask taskBoard (Hree.AnimationTask st (BV.head animations) (Hree.AnimationTaskOption True False Nothing))
                    return (Just taskBoard)

        | otherwise = do
            geometry <- case extension of
                            ".stl" -> STL.loadGeometryFromFile path
                            ".ply" -> PLY.loadGeometryFromFile path
                            _ -> throwIO . userError $ "unknown format. path: " ++ path
            let material = Hree.basicMaterial $ V3 0.5 (-1) (-0.5)
            materialId <- Hree.addMaterial scene material
            let mesh = Hree.mesh geometry materialId
            meshId <- Hree.addMesh scene mesh
            void $ Hree.addNode scene Hree.node (Just meshId) True
            return Nothing

    onDisplay (r, s, c, Just taskBoard) w = do
        render
        threadDelay 10000
        GLFW.pollEvents
        t <- Time.now
        Hree.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, Just taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    onDisplay (r, s, c, Nothing) w = do
        render
        threadDelay 10000
        GLFW.pollEvents
        onDisplay (r, s, c, Nothing) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
