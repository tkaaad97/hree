module Example
    ( mkColoredImage
    , mkCircleImage
    , setCameraMouseControl
    , shutdown
    , resizeWindow
    , resizeWindowWithCamera
    , withWindow
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import qualified GLW
import qualified Graphics.Hree.Camera as Hree
import qualified Graphics.Hree.CameraControl.SphericalControl as Hree
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V4(..))
import System.Exit (exitSuccess)

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> (a -> GLFW.Window -> IO ()) -> IO ()
withWindow width height title constructor f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 5
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 32)
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setWindowSizeCallback win (Just resizeWindow)
              GLFW.setWindowCloseCallback win (Just shutdown)
              constructor win >>= liftIO . flip f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
    GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)

resizeWindowWithCamera :: Hree.Camera -> GLFW.WindowSizeCallback
resizeWindowWithCamera camera _ w h = do
    GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
    projection <- Hree.getCameraProjection camera
    let aspect = fromIntegral w / fromIntegral h
        projection' = updateProjectionAspectRatio projection aspect
    Hree.updateProjection camera projection'

updateProjectionAspectRatio :: Hree.Projection -> Float -> Hree.Projection
updateProjectionAspectRatio (Hree.PerspectiveProjection (Hree.Perspective fov _ near far)) aspect =
    Hree.PerspectiveProjection $ Hree.Perspective fov aspect near far
updateProjectionAspectRatio p _ = p

setCameraMouseControl :: GLFW.Window -> Hree.Camera -> IO Hree.SphericalControl
setCameraMouseControl w camera = do
    control <- Hree.newSphericalControlDefault camera
    setMouseButtonEventCallback w (Hree.enterSphericalControl control) (Hree.leaveSphericalControl control)
    setEnterOrLeaveEventCallback w (const $ return ()) (Hree.leaveSphericalControl control)
    setCursorMoveEventCallback w (Hree.updateSphericalControl control)
    return control

    where
    setMouseButtonEventCallback _ onPress onRelease =
        let callback w' GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = go w' (onPress Hree.SphericalControlModeOrbit)
            callback w' GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _ = go w' (onPress Hree.SphericalControlModeZoom)
            callback w' GLFW.MouseButton'1 GLFW.MouseButtonState'Released _ = go w' onRelease
            callback w' GLFW.MouseButton'2 GLFW.MouseButtonState'Released _ = go w' onRelease
            callback _ _ _ _ = return ()
            go w' f = do
                (width, height) <- GLFW.getWindowSize w'
                (x, y) <- GLFW.getCursorPos w'
                f (calcControlPosition width height x y)
        in GLFW.setMouseButtonCallback w (Just callback)

    setEnterOrLeaveEventCallback _ onEnter onLeave =
        let callback w' GLFW.CursorState'InWindow    = go w' onEnter
            callback w' GLFW.CursorState'NotInWindow = go w' onLeave
            go w' f = do
                (width, height) <- GLFW.getWindowSize w'
                (x, y) <- GLFW.getCursorPos w'
                f (calcControlPosition width height x y)
        in GLFW.setCursorEnterCallback w (Just callback)

    setCursorMoveEventCallback _ onMove =
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

mkColoredImage :: Int -> Vector (V4 Word8)
mkColoredImage size = Vector.generate (size * size) gen
    where
    gen i =
        let (y, x) = divMod i size
            r = fromIntegral $ 255 * x `div` size
            g = fromIntegral $ 255 * y `div` size
            b = 64
            a = 255
        in V4 r g b a

mkCircleImage :: Int -> V4 Word8 -> Vector (V4 Word8)
mkCircleImage size color = Vector.generate (size * size) gen
    where
    empty = V4 0 0 0 0
    half = size `div` 2
    r2 = half * half
    gen i =
        let (y, x) = divMod i size
            q = (x - half) * (x - half) + (y - half) * (y - half)
        in if q <= r2 then color else empty
