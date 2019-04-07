module Main where

import qualified Data.Vector.Storable as Vector
import qualified GLW
import qualified Graphics.GL as GL
import Graphics.Hree.Camera
import Graphics.Hree.CameraControl.SphericalControl
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.GL.Vertex
import qualified Graphics.Hree.Material as Material
import Graphics.Hree.Mesh (Mesh(..))
import Graphics.Hree.Scene
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Sample

main :: IO ()
main = do
    withWindow width height "canvas-sample" init onDisplay

    where
    width  = 640
    height = 480

    x = 0.8
    vs = Vector.fromList
        [ BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , BasicVertex (V3 x 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Material.basicMaterial

    --proj = orthographic 0 1 0 1 (-10) 10
    proj = perspective 90 1.0 0.1 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        scene <- newScene
        geometry <- geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
        let mesh = Mesh geometry material
        addMesh scene mesh
        camera <- newCamera proj la
        control <- newSphericalControlDefault camera
        setMouseButtonEventCallback w (enterSphericalControl control) (leaveSphericalControl control)
        setEnterOrLeaveEventCallback w (const $ return ()) (leaveSphericalControl control)
        setCursorMoveEventCallback w (updateSphericalControl control)
        return (scene, camera)

    onDisplay (s, c) w = do
        render
        GLFW.pollEvents
        onDisplay (s, c) w

        where
        render = do
            renderScene s c
            GLFW.swapBuffers w

    setMouseButtonEventCallback w onPress onRelease =
        let callback w' GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = go w' (onPress SphericalControlModeOrbit)
            callback w' GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _ = go w' (onPress SphericalControlModeZoom)
            callback w' GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = go w' onRelease
            callback w' GLFW.MouseButton'2 GLFW.MouseButtonState'Released _ = go w' onRelease
            callback _ _ _ _ = return ()
            go w' f = do
                (width, height) <- GLFW.getWindowSize w'
                (x, y) <- GLFW.getCursorPos w'
                f (calcControlPosition width height x y)
        in GLFW.setMouseButtonCallback w (Just callback)

    setEnterOrLeaveEventCallback w onEnter onLeave =
        let callback w' GLFW.CursorState'InWindow    = go w' onEnter
            callback w' GLFW.CursorState'NotInWindow = go w' onLeave
            go w' f = do
                (width, height) <- GLFW.getWindowSize w'
                (x, y) <- GLFW.getCursorPos w'
                f (calcControlPosition width height x y)
        in GLFW.setCursorEnterCallback w (Just callback)

    setCursorMoveEventCallback w onMove =
        let callback w' x y = do
                (width, height) <- GLFW.getWindowSize w'
                onMove (calcControlPosition width height x y)
        in GLFW.setCursorPosCallback w (Just callback)

    calcControlPosition :: Int -> Int -> Double -> Double -> V2 Float
    calcControlPosition width height x y =
        let x' = if width > 0
                then max 0 (min (fromIntegral width) (realToFrac x)) / fromIntegral width
                else 0
            y' = if height > 0
                then max 0 (min (fromIntegral height) (realToFrac y)) / fromIntegral height
                else 0
        in V2 x' y'
