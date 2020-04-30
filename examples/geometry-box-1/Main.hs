{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Example
import Foreign (Ptr)
import qualified Foreign (castPtr)
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Material.BasicMaterial as Material
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..), V4(..))
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "geometry-box-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    uvs =
        [ Hree.Uv $ V2 0 0
        , Hree.Uv $ V2 1 0
        , Hree.Uv $ V2 0 1
        , Hree.Uv $ V2 1 1
        , Hree.Uv $ V2 0 1
        , Hree.Uv $ V2 1 0
        ]
    vs = Vector.fromList . concat . replicate 6 $ uvs

    proj = Hree.perspective 90 defaultAspect 0.1 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        (geometry, _) <- Hree.createBoxGeometry 0.5 0.5 0.5 scene
        geometry' <- Hree.addVerticesToGeometry geometry vs GL.GL_STATIC_READ scene
        texture <- mkTexture scene
        let material = (Material.basicMaterial (V3 0.5 (-1) (-1)))
                { Material.baseColorTexture = Just texture
                }
            mesh = Hree.Mesh geometry' material Nothing
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh
        _ <- Hree.addNode scene Hree.newNode { Hree.nodeMesh = Just meshId } True
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

    mkTexture :: Hree.Scene -> IO Hree.Texture
    mkTexture scene = do
        let size = 256
            settings = Hree.TextureSettings 1 GL.GL_RGBA8 (fromIntegral size) (fromIntegral size) False
            image = mkColoredImage size
        Vector.unsafeWith image $ \p -> do
            let source = Hree.TextureSourceData (fromIntegral size) (fromIntegral size) PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr (p :: Ptr (V4 Word8)))
            (_, texture) <- Hree.addTexture scene "color" settings source
            (_, sampler) <- Hree.addSampler scene "sampler"
            return $ Hree.Texture (texture, sampler)
