{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Example
import Foreign (Ptr)
import qualified Foreign (castPtr)
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.Format.STL as STL (loadGeometryFromSTLFile)
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.Geometry.Box
import Graphics.Hree.GL.Types
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Mesh (Mesh(..))
import Graphics.Hree.Scene
import qualified Graphics.Hree.Texture as Texture
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let path = if null args then "examples/format-stl-1/monkey.stl" else head args
    withWindow width height "format-stl-1" (init path) onDisplay

    where
    width  = 640
    height = 480
    aspect = fromIntegral width / fromIntegral height

    proj = perspective 90 aspect 0.1 10.0

    la = lookAt (V3 0 (-1) 5) (V3 0 0 0) (V3 0 1 0)

    init path w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- newScene
        (geometry, vs) <- STL.loadGeometryFromSTLFile path scene
        let material = Material.basicMaterial Nothing
            material' = Material.setDirectionalLight material (V3 0.5 (-1) (-0.5))
            mesh = Mesh geometry material' (Vector.length vs) Nothing
        addMesh scene mesh
        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    resizeWindow' camera win w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio p @ Perspective {} aspect =
        p { perspectiveAspect = aspect }
    updateProjectionAspectRatio p _ = p
