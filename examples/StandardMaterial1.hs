{-# LANGUAGE OverloadedStrings #-}
module StandardMaterial1 where

import Example
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import qualified Hree.Loader.STL as STL (loadGeometryFromFile)
import qualified Hree.Material.StandardMaterial as Material
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
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        geometry <- STL.loadGeometryFromFile path
        let material = Hree.standardMaterial $
                Hree.standardMaterialBlock
                    { Material.metallicFactor = metalness
                    , Material.roughnessFactor = roughness
                    }
        materialId <- Hree.addMaterial scene material
        let mesh = Hree.mesh geometry materialId
            light = Hree.directionalLight (V3 (-1) 0 (-5)) (V3 1 1 1) 1
        meshId <- Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.node (Just meshId) True
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
