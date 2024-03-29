{-# LANGUAGE OverloadedStrings #-}
module NodeMeshAnimation1 where

import qualified Chronos as Time (now)
import qualified Codec.Picture as Picture (Image(..), PixelRGBA8, convertRGBA8,
                                           readImage)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Int (Int64)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Foreign
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import Linear (V2(..), V3(..))
import Prelude hiding (init)

data CharacterInfo = CharacterInfo
    { meshStillFront     :: !(Hree.MeshId Hree.SpriteMaterialBlock)
    , meshStillBack      :: !(Hree.MeshId Hree.SpriteMaterialBlock)
    , meshStillLeft      :: !(Hree.MeshId Hree.SpriteMaterialBlock)
    , meshStillRight     :: !(Hree.MeshId Hree.SpriteMaterialBlock)
    , animationWalkFront :: !Hree.AnimationClip
    , animationWalkBack  :: !Hree.AnimationClip
    , animationWalkLeft  :: !Hree.AnimationClip
    , animationWalkRight :: !Hree.AnimationClip
    } deriving (Show)

main :: IO ()
main =
    withWindow width height "node-mesh-animation-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    tsize = V2 128 128
    V2 twidth theight = tsize
    V2 twidth' theight' = fmap fromIntegral tsize

    timepoints :: UV.Vector Int64
    timepoints = UV.map (* 1000000) $ UV.fromList
        [ 0
        , 250
        , 500
        , 750
        , 1000
        ]

    walkFrontUVs, walkBackUVs, walkLeftUVs, walkRightUVs :: SV.Vector (V2 (V2 Float))
    walkFrontUVs = SV.fromList
        [ V2 (V2 16 31) (V2 16 (-16))
        , V2 (V2 16 48) (V2 16 (-16))
        , V2 (V2 16 31) (V2 16 (-16))
        , V2 (V2 16 64) (V2 16 (-16))
        , V2 (V2 16 31) (V2 16 (-16))
        ]
    walkBackUVs = SV.fromList
        [ V2 (V2 48 31) (V2 16 (-16))
        , V2 (V2 48 48) (V2 16 (-16))
        , V2 (V2 48 31) (V2 16 (-16))
        , V2 (V2 48 64) (V2 16 (-16))
        , V2 (V2 48 31) (V2 16 (-16))
        ]
    walkLeftUVs = SV.fromList
        [ V2 (V2 48 31) (V2 (-16) (-16))
        , V2 (V2 48 48) (V2 (-16) (-16))
        , V2 (V2 48 31) (V2 (-16) (-16))
        , V2 (V2 48 64) (V2 (-16) (-16))
        , V2 (V2 48 31) (V2 (-16) (-16))
        ]
    walkRightUVs = SV.fromList
        [ V2 (V2 32 31) (V2 16 (-16))
        , V2 (V2 32 48) (V2 16 (-16))
        , V2 (V2 32 31) (V2 16 (-16))
        , V2 (V2 32 64) (V2 16 (-16))
        , V2 (V2 32 31) (V2 16 (-16))
        ]

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene

        materialId <- createMaterial scene

        stillFront <- createMesh scene materialId $ SV.head walkFrontUVs
        stillBack <- createMesh scene materialId $ SV.head walkBackUVs
        stillLeft <- createMesh scene materialId $ SV.head walkLeftUVs
        stillRight <- createMesh scene materialId $ SV.head walkRightUVs
        nodeId <- Hree.addNode scene Hree.node (Just stillFront) True
        walkFront <- createNodeMeshAnimation scene materialId nodeId walkFrontUVs
        walkBack <- createNodeMeshAnimation scene materialId nodeId walkBackUVs
        walkLeft <- createNodeMeshAnimation scene materialId nodeId walkLeftUVs
        walkRight <- createNodeMeshAnimation scene materialId nodeId walkRightUVs
        let characterInfo = CharacterInfo stillFront stillBack stillLeft stillRight walkFront walkBack walkLeft walkRight

        taskBoard <- Hree.newSceneTaskBoard scene
        taskId <- Hree.addSceneTask taskBoard Hree.Nop

        GLFW.setKeyCallback w (Just $ keyCallback scene nodeId characterInfo taskBoard taskId)

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        render
        threadDelay 100000
        GLFW.pollEvents
        t <- Time.now
        Hree.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    -- image source https://opengameart.org/sites/default/files/RPG_assets.png
    imagePath = "examples/images/RPG_assets.png"

    createMaterial scene = do
        image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage imagePath
        mapping <- mkMappingSource image
        let material = Hree.spriteMaterial { Hree.materialMappings = pure (Hree.BaseColorMapping, mapping) }
        Hree.addMaterial scene material

    mkMappingSource :: Picture.Image Picture.PixelRGBA8 -> IO Hree.MappingSource
    mkMappingSource image = do
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 twidth theight False
            byteSize = twidth * theight * 4
            samplerParamValues =
                [ Hree.SamplerParamValue Hree.glTextureMinFilter GL.GL_NEAREST
                , Hree.SamplerParamValue Hree.glTextureMagFilter GL.GL_NEAREST
                ]
        pixels <- Foreign.mallocForeignPtrBytes (fromIntegral byteSize)
        let textureSource = Hree.TextureSourceData twidth theight PixelFormat.glRgba GL.GL_UNSIGNED_BYTE pixels
        Foreign.withForeignPtr pixels $ \dest ->
            SV.unsafeWith (Picture.imageData image) $ \ptr -> do
                Foreign.copyBytes (Foreign.castPtr dest) ptr (fromIntegral byteSize)
        return (Hree.MappingSource settings textureSource samplerParamValues)

    createMesh scene materialId (V2 (V2 x y) (V2 w h)) = do
        let vs = SV.singleton $ Hree.SpriteVertex (V3 0 0 0) (V3 0.5 0.5 0) (V3 0 0 0) 0 (V2 (x / twidth') (y / theight')) (V2 (w / twidth') (h / theight')) GL.GL_FALSE 0
            geo' = Hree.addVerticesToGeometry Hree.spriteGeometry vs GL.GL_STATIC_READ
        Hree.addMesh scene (Hree.mesh geo' materialId) { Hree.meshInstanceCount = Just 1 }

    createMeshes scene materialId uvs = SV.mapM (createMesh scene materialId) uvs

    createNodeMeshAnimation scene materialId nodeId uvs = do
        meshIds <- createMeshes scene materialId uvs
        return $ Hree.stepMesh scene nodeId timepoints meshIds

    keyCallback _ _ characterInfo taskBoard taskId _ key _ GLFW.KeyState'Pressed _ =
        case resolveWalkAnimation characterInfo key of
            Just animation -> do
                st <- Time.now
                Hree.modifySceneTask taskBoard (const $ Hree.AnimationTask st animation (Hree.AnimationTaskOption True False Nothing)) taskId
            Nothing -> return ()

    keyCallback scene nodeId characterInfo taskBoard taskId _ key _ GLFW.KeyState'Released _ =
        case resolveStillMesh characterInfo key of
            Just mesh -> do
                Hree.modifySceneTask taskBoard (const Hree.Nop) taskId
                _ <- Hree.updateNodeMesh scene nodeId (Just mesh)
                return ()
            Nothing -> return ()

    keyCallback _ _ _ _ _ _ _ _ _ _ = return ()

    resolveWalkAnimation characterInfo GLFW.Key'Up = Just $ animationWalkBack characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Down = Just $ animationWalkFront characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Left = Just $ animationWalkLeft characterInfo
    resolveWalkAnimation characterInfo GLFW.Key'Right = Just $ animationWalkRight characterInfo
    resolveWalkAnimation _ _ = Nothing

    resolveStillMesh characterInfo GLFW.Key'Up = Just $ meshStillBack characterInfo
    resolveStillMesh characterInfo GLFW.Key'Down = Just $ meshStillFront characterInfo
    resolveStillMesh characterInfo GLFW.Key'Left = Just $ meshStillLeft characterInfo
    resolveStillMesh characterInfo GLFW.Key'Right = Just $ meshStillRight characterInfo
    resolveStillMesh _ _ = Nothing
