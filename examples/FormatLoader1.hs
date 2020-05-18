{-# LANGUAGE OverloadedStrings #-}
module FormatLoader1 where

import qualified Chronos as Time (now)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (void)
import qualified Data.Vector as BV (head, null)
import Example
import qualified Graphics.Format.GLTF as GLTF (Supplement(..),
                                               loadSceneFromFile)
import qualified Graphics.Format.PLY as PLY (loadGeometryFromFile)
import qualified Graphics.Format.STL as STL (loadGeometryFromFile)
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.BasicMaterial as Material
import qualified Graphics.Hree.SceneTask as SceneTask
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

    proj = Hree.perspective 90 defaultAspect 0.0001 10000.0

    la = Hree.lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        a <- loadScene path scene (FilePath.takeExtension path)
        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, a)

    loadScene path scene ".gltf" = do
        (_, sup) <- GLTF.loadSceneFromFile path scene
        let light = Hree.directionalLight (V3 0.5 (-1) (-0.5)) (V3 1 1 1) 1
            animations = GLTF.supplementAnimations sup
        _ <- Hree.addLight scene light
        if BV.null animations
            then return Nothing
            else do
                st <- Time.now
                taskBoard <- SceneTask.newSceneTaskBoard scene
                _ <- SceneTask.addSceneTask taskBoard (SceneTask.AnimationTask st (BV.head animations) (SceneTask.AnimationTaskOption True False Nothing))
                return (Just taskBoard)

    loadScene path scene extension = do
        geometry <- case extension of
                        ".stl" -> STL.loadGeometryFromFile path scene
                        ".ply" -> PLY.loadGeometryFromFile path scene
                        _ -> throwIO . userError $ "unknown format. path: " ++ path
        let material = Material.basicMaterial $ V3 0.5 (-1) (-0.5)
            mesh = Hree.Mesh geometry material Nothing
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        void $ Hree.addNode scene Hree.newNode{ Hree.nodeMesh = Just meshId } True
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
            Hree.renderScene r s c
            GLFW.swapBuffers w

    onDisplay (r, s, c, Nothing) w = do
        render
        threadDelay 20000
        GLFW.pollEvents
        onDisplay (r, s, c, Nothing) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
