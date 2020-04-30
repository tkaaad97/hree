{-# LANGUAGE OverloadedStrings #-}
module StandardMaterial1 where

import Example
import qualified Graphics.Format.STL as STL (loadGeometryFromFile)
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.StandardMaterial as Material
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..))
import Prelude hiding (init)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let path = "examples/models/stl/binary/monkey.stl"
        (metalness, roughness) = case args of
            []        -> (0.5, 0.5)
            [a]       -> (read a, 0.5)
            a : b : _ -> (read a, read b)
    putStrLn $ "metalness: " ++ show metalness
    putStrLn $ "roughness: " ++ show roughness
    withWindow width height "standard-material-1" (init path metalness roughness) onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.perspective 90 defaultAspect 0.1 1000.0

    la = Hree.lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)

    init path metalness roughness w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        geometry <- STL.loadGeometryFromFile path scene
        let material = Material.standardMaterial $
                Material.standardMaterialBlock
                    { Material.metallicFactor = metalness
                    , Material.roughnessFactor = roughness
                    }
            mesh = Hree.Mesh geometry material Nothing
            light = Hree.directionalLight (V3 (-1) 0 (-5)) (V3 1 1 1) 1
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.newNode { Hree.nodeMesh = Just meshId } True
        _ <- Hree.addLight scene light
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
