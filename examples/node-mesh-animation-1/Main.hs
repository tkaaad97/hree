{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Chronos as Time (now)
import qualified Codec.Picture as Picture (Image(..), convertRGBA8, readImage)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Int (Int64)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Foreign (castPtr)
import qualified GLW
import qualified GLW.Groups.PixelFormat as PixelFormat
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Animation as Animation
import Graphics.Hree.Camera
import Graphics.Hree.Geometry as Hree (addVerticesToGeometry, newSpriteGeometry)
import qualified Graphics.Hree.GL.Types as Hree (Texture(..))
import qualified Graphics.Hree.GL.Vertex as Hree (SpriteVertex(..))
import qualified Graphics.Hree.Material.SpriteMaterial as Hree (baseColorTexture,
                                                                spriteMaterial)
import qualified Graphics.Hree.Sampler as Hree (glTextureMagFilter,
                                                glTextureMinFilter,
                                                setSamplerParameter)
import qualified Graphics.Hree.Scene as Hree (AddedMesh(..), addMesh, addNode,
                                              addSampler, addTexture, newNode,
                                              newRenderer, newScene,
                                              renderScene, updateNode)
import qualified Graphics.Hree.SceneTask as SceneTask
import qualified Graphics.Hree.Texture as Hree (TextureSettings(..),
                                                TextureSourceData(..))
import Graphics.Hree.Types (Mesh(..), MeshId, Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..), V3(..))
import Prelude hiding (init)

data CharacterInfo = CharacterInfo
    { meshStillFront     :: MeshId
    , meshStillBack      :: MeshId
    , meshStillLeft      :: MeshId
    , meshStillRight     :: MeshId
    , animationWalkFront :: Animation.Animation
    , animationWalkBack  :: Animation.Animation
    , animationWalkLeft  :: Animation.Animation
    , animationWalkRight :: Animation.Animation
    } deriving (Show, Eq)

main :: IO ()
main =
    withWindow width height "node-mesh-animation-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

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

        material <- createMaterial scene

        stillFront <- createMesh scene material $ SV.head walkFrontUVs
        stillBack <- createMesh scene material $ SV.head walkBackUVs
        stillLeft <- createMesh scene material $ SV.head walkLeftUVs
        stillRight <- createMesh scene material $ SV.head walkRightUVs
        let node = Hree.newNode { nodeMesh = Just stillFront }
        nodeId <- Hree.addNode scene node True
        walkFront <- createNodeMeshAnimation scene material nodeId walkFrontUVs
        walkBack <- createNodeMeshAnimation scene material nodeId walkBackUVs
        walkLeft <- createNodeMeshAnimation scene material nodeId walkLeftUVs
        walkRight <- createNodeMeshAnimation scene material nodeId walkRightUVs
        let characterInfo = CharacterInfo stillFront stillBack stillLeft stillRight walkFront walkBack walkLeft walkRight

        taskBoard <- SceneTask.newSceneTaskBoard scene
        taskId <- SceneTask.addSceneTask taskBoard SceneTask.Nop

        GLFW.setKeyCallback w (Just $ keyCallback scene nodeId characterInfo taskBoard taskId)

        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        render
        threadDelay 100000
        GLFW.pollEvents
        t <- Time.now
        SceneTask.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w

    -- image source https://opengameart.org/sites/default/files/RPG_assets.png
    imagePath = "examples/node-mesh-animation-1/RPG_assets.png"

    createMaterial scene = do
        image <- either (throwIO . userError) (return . Picture.convertRGBA8) =<< Picture.readImage imagePath
        let settings = Hree.TextureSettings 1 GL.GL_RGBA8 twidth theight False
        (_, texture) <- SV.unsafeWith (Picture.imageData image) $ \ptr -> do
            let sourceData = Hree.TextureSourceData twidth theight PixelFormat.glRgba GL.GL_UNSIGNED_BYTE (Foreign.castPtr ptr)
            Hree.addTexture scene "material1" settings sourceData
        (_, sampler) <- Hree.addSampler scene "material1"
        Hree.setSamplerParameter sampler Hree.glTextureMinFilter GL.GL_NEAREST
        Hree.setSamplerParameter sampler Hree.glTextureMagFilter GL.GL_NEAREST
        let material = Hree.spriteMaterial { Hree.baseColorTexture = Just $ Hree.Texture (texture, sampler) }
        return material

    createMesh scene material (V2 (V2 x y) (V2 w h)) = do
        let vs = SV.singleton $ Hree.SpriteVertex (V3 0 0 0) (V3 0.5 0.5 0) (V3 0 0 0) 0 (V2 (x / twidth') (y / theight')) (V2 (w / twidth') (h / theight'))
        (geo, _) <- Hree.newSpriteGeometry scene
        geo' <- Hree.addVerticesToGeometry geo vs GL.GL_STATIC_READ scene
        fmap Hree.addedMeshId . Hree.addMesh scene $ Mesh geo' material (Just 1)

    createMeshes scene material uvs = SV.mapM (createMesh scene material) uvs

    createNodeMeshAnimation scene material nodeId uvs = do
        meshIds <- createMeshes scene material uvs
        let track = Animation.stepMesh timepoints meshIds
            animation = Animation.singleAnimation nodeId track
        return animation

    resizeWindow' camera _ w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio (PerspectiveProjection (Perspective fov _ near far)) aspect =
        PerspectiveProjection $ Perspective fov aspect near far
    updateProjectionAspectRatio p _ = p

    keyCallback _ _ characterInfo taskBoard taskId _ key _ GLFW.KeyState'Pressed _ =
        case resolveWalkAnimation characterInfo key of
            Just animation -> do
                st <- Time.now
                SceneTask.modifySceneTask taskBoard (const $ SceneTask.AnimationTask st animation (SceneTask.AnimationTaskOptions True False)) taskId
            Nothing -> return ()

    keyCallback scene nodeId characterInfo taskBoard taskId _ key _ GLFW.KeyState'Released _ =
        case resolveStillMesh characterInfo key of
            Just mesh -> do
                SceneTask.modifySceneTask taskBoard (const SceneTask.Nop) taskId
                _ <- Hree.updateNode scene nodeId (\node -> node { nodeMesh = Just mesh })
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
