{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Chronos as Time (Time(..), now)
import Control.Concurrent (threadDelay)
import Data.Fixed (mod')
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import Foreign (Storable(..), castPtr, plusPtr)
import qualified GLW
import qualified Graphics.GL as GL
import qualified Graphics.Hree.Animation as Animation
import Graphics.Hree.Camera
import qualified Graphics.Hree.Geometry as Geometry
import Graphics.Hree.GL (attribFormat, attribIFormat)
import Graphics.Hree.GL.Types (BindBufferSetting(..), IVec4, Vec3, Vec4)
import Graphics.Hree.GL.Vertex (Vertex(..), VertexField(..), VertexSpec(..))
import qualified Graphics.Hree.Light as Light
import qualified Graphics.Hree.Material as Material
import qualified Graphics.Hree.Scene as Scene
import Graphics.Hree.Types (Geometry(..), Mesh(..), Node(..))
import qualified Graphics.UI.GLFW as GLFW
import Linear (Quaternion(..), V3(..), V4(..))
import Prelude hiding (init)

data SkinVertex = SkinVertex
    { vertexPosition     :: !Vec3
    , vertexJointIndices :: !IVec4
    , vertexJointWeights :: !Vec4
    } deriving (Show, Eq)

instance Storable SkinVertex where
    sizeOf _ = 44

    alignment _ = 4

    peek ptr = do
        p <- peek $ castPtr ptr `plusPtr` 0
        i <- peek $ castPtr ptr `plusPtr` 12
        w <- peek $ castPtr ptr `plusPtr` 28
        return $ SkinVertex p i w

    poke ptr (SkinVertex p i w) = do
        poke (castPtr ptr `plusPtr` 0) p
        poke (castPtr ptr `plusPtr` 12) i
        poke (castPtr ptr `plusPtr` 28) w

instance Vertex SkinVertex where
    vertexSpec _ = VertexSpec bbs fields

        where
        positionField = VertexField "position" (attribFormat 3 GL.GL_FLOAT False 0)
        jointIndicesField = VertexField "jointIndices" (attribIFormat 4 GL.GL_INT 12)
        jointWeightsField = VertexField "jointWeights" (attribFormat 4 GL.GL_FLOAT False 28)
        fields =
            [ positionField
            , jointIndicesField
            , jointWeightsField
            ]
        bbs = BindBufferSetting 0 (sizeOf (undefined :: SkinVertex)) 0

main :: IO ()
main =
    withWindow width height "skinning-1" init onDisplay

    where
    width  = 640
    height = 480

    vs = SV.fromList
        [ SkinVertex (V3 0.0 0.0 0.0) (V4 0 1 0 0) (V4 1.00 0.00 0.0 0.0)
        , SkinVertex (V3 1.0 0.0 0.0) (V4 0 1 0 0) (V4 1.00 0.00 0.0 0.0)
        , SkinVertex (V3 0.0 0.5 0.0) (V4 0 1 0 0) (V4 0.75 0.25 0.0 0.0)
        , SkinVertex (V3 1.0 0.5 0.0) (V4 0 1 0 0) (V4 0.75 0.25 0.0 0.0)
        , SkinVertex (V3 0.0 1.0 0.0) (V4 0 1 0 0) (V4 0.50 0.50 0.0 0.0)
        , SkinVertex (V3 1.0 1.0 0.0) (V4 0 1 0 0) (V4 0.50 0.50 0.0 0.0)
        , SkinVertex (V3 0.0 1.5 0.0) (V4 0 1 0 0) (V4 0.25 0.75 0.0 0.0)
        , SkinVertex (V3 1.0 1.5 0.0) (V4 0 1 0 0) (V4 0.25 0.75 0.0 0.0)
        , SkinVertex (V3 0.0 2.0 0.0) (V4 0 1 0 0) (V4 0.00 1.00 0.0 0.0)
        , SkinVertex (V3 1.0 2.0 0.0) (V4 0 1 0 0) (V4 0.00 1.00 0.0 0.0)
        ]

    indices = SV.fromList [0, 1, 3, 0, 3, 2, 2, 3, 5, 2, 5, 4, 4, 5, 7, 4, 7, 6, 6, 7, 9, 6, 9, 8]

    invMats = SV.fromList
        [ V4 (V4 1.0 0.0 0.0 (-0.5))
             (V4 0.0 1.0 0.0 (-1.0))
             (V4 0.0 0.0 1.0 0.0)
             (V4 0.0 0.0 0.0 1.0)
        , V4 (V4 1.0 0.0 0.0 (-0.5))
             (V4 0.0 1.0 0.0 (-1.0))
             (V4 0.0 0.0 1.0 0.0)
             (V4 0.0 0.0 0.0 1.0)
        ]

    timePoints = UV.fromList [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5]

    rotations = UV.fromList
        [ Quaternion 1.0 (V3 0.0 0.0 0.0)
        , Quaternion 0.924 (V3 0.0 0.0 0.383)
        , Quaternion 0.707 (V3 0.0 0.0 0.707)
        , Quaternion 0.707 (V3 0.0 0.0 0.707)
        , Quaternion 0.924 (V3 0.0 0.0 0.383)
        , Quaternion 1.0 (V3 0.0 0.0 0.0)
        , Quaternion 1.0 (V3 0.0 0.0 0.0)
        , Quaternion 0.924 (V3 0.0 0.0 (-0.383))
        , Quaternion 0.707 (V3 0.0 0.0 (-0.707))
        , Quaternion 0.707 (V3 0.0 0.0 (-0.707))
        , Quaternion 0.924 (V3 0.0 0.0 (-0.383))
        , Quaternion 1.0 (V3 0.0 0.0 0.0)
        ]

    defaultAspect = fromIntegral width / fromIntegral height

    proj = perspective 90 defaultAspect 0.001 1000.0

    la = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        GL.glEnable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        scene <- Scene.newScene

        indexBuffer <- Scene.addIndexBufferUInt scene indices
        geometry <- Geometry.addVerticesToGeometry Geometry.newGeometry { geometryIndexBuffer = Just indexBuffer } vs GL.GL_STREAM_DRAW scene

        nodeId2 <- Scene.addNode scene Scene.newNode { nodeRotation = Quaternion 1 (V3 0 0 0) } False
        nodeId1 <- Scene.addNode scene Scene.newNode { nodeChildren = BV.singleton nodeId2, nodeTranslation = V3 0 1 0 } False
        nodeId0 <- Scene.addNode scene Scene.newNode { nodeChildren = BV.singleton nodeId1 } True

        skinId <- Scene.addSkin scene nodeId0 (SV.fromList [nodeId1, nodeId2]) invMats

        let material = Material.standardMaterial 0 0
                `Material.setBaseColorFactor` V4 0.1 0.1 0.8 1.0
            mesh = Mesh geometry material Nothing
        meshId <- Scene.addSkinnedMesh scene mesh skinId

        _ <- Scene.updateNode scene nodeId0 (\n -> n { nodeMesh = Just meshId })

        let track = Animation.linearRotation timePoints rotations
            channel = Animation.singleChannel nodeId2 track
            animation = Animation.animation (BV.singleton channel) 5.5

        camera <- newCamera proj la
        _ <- setCameraMouseControl w camera

        let light = Light.directionalLight (V3 0.5 (-1) 0.5) (V3 1 1 1) 1
        _ <- Scene.addLight scene light

        GLFW.setWindowSizeCallback w (Just (resizeWindow' camera))
        st <- Time.now
        return (scene, camera, animation, st)

    onDisplay (s, c, animation, st) w = do
        render
        threadDelay 100000
        GLFW.pollEvents
        t <- Time.now
        let duration = Animation.animationDuration animation
            t' = realToFrac $ diffTime t st `mod'` realToFrac duration
        Animation.applyAnimation s animation t'
        onDisplay (s, c, animation, st) w

        where
        render = do
            Scene.renderScene s c
            GLFW.swapBuffers w

    resizeWindow' camera _ w h = do
        GLW.glViewport 0 0 (fromIntegral w) (fromIntegral h)
        projection <- getCameraProjection camera
        let aspect = fromIntegral w / fromIntegral h
            projection' = updateProjectionAspectRatio projection aspect
        updateProjection camera projection'

    updateProjectionAspectRatio (PerspectiveProjection (Perspective fov _ near far)) aspect =
        PerspectiveProjection $ Perspective fov aspect near far
    updateProjectionAspectRatio p _ = p

diffTime :: Time.Time -> Time.Time -> Double
diffTime ta tb =
    let nsa = Time.getTime ta
        nsb = Time.getTime tb
    in fromIntegral (nsa - nsb) * 1.0E-9
