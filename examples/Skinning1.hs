{-# LANGUAGE OverloadedStrings #-}
module Skinning1 where

import qualified Chronos as Time (now)
import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Example
import Foreign (Storable(..), castPtr, plusPtr)
import qualified Graphics.GL as GL
import qualified Graphics.Hree as Hree
import Graphics.Hree.GL (attribFormat, attribIFormat)
import qualified Graphics.Hree.Material.StandardMaterial as Material
import qualified Graphics.UI.GLFW as GLFW
import Linear (Quaternion(..), V3(..), V4(..))
import Prelude hiding (init)

data SkinVertex = SkinVertex
    { vertexPosition     :: !Hree.Vec3
    , vertexJointIndices :: !Hree.IVec4
    , vertexJointWeights :: !Hree.Vec4
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

instance Hree.Vertex SkinVertex where
    vertexSpec _ = Hree.VertexSpec bbs fields

        where
        positionField = Hree.VertexField "position" (attribFormat 3 GL.GL_FLOAT False 0)
        jointIndicesField = Hree.VertexField "jointIndices" (attribIFormat 4 GL.GL_INT 12)
        jointWeightsField = Hree.VertexField "jointWeights" (attribFormat 4 GL.GL_FLOAT False 28)
        fields =
            [ positionField
            , jointIndicesField
            , jointWeightsField
            ]
        bbs = Hree.BindBufferSetting 0 (sizeOf (undefined :: SkinVertex)) 0

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

    ms = 1000000 :: Int64
    timePoints = UV.fromList . map (* ms) $ [0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500]

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

    proj = Hree.perspective 90 defaultAspect 0.001 1000.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene

        let geometry = Hree.addVerticesToGeometry Hree.emptyGeometry vs GL.GL_STREAM_DRAW
                        `Hree.setIndexBufferSourceUInt` indices

        nodeId2 <- Hree.addNode scene Hree.newNode { Hree.nodeRotation = Quaternion 1 (V3 0 0 0) } False
        nodeId1 <- Hree.addNode scene Hree.newNode { Hree.nodeChildren = BV.singleton nodeId2, Hree.nodeTranslation = V3 0 1 0 } False
        nodeId0 <- Hree.addNode scene Hree.newNode { Hree.nodeChildren = BV.singleton nodeId1 } True

        skinId <- Hree.addSkin scene nodeId0 (SV.fromList [nodeId1, nodeId2]) invMats

        let material = Material.standardMaterial $
                Material.standardMaterialBlock
                    { Material.metallicFactor = 0
                    , Material.roughnessFactor = 0
                    , Material.baseColorFactor = V4 0.1 0.1 0.8 1.0
                    }
        materialId <- Hree.addMaterial scene material
        let mesh = Hree.Mesh geometry materialId Nothing
        meshId <- Hree.addedMeshId <$> Hree.addSkinnedMesh scene mesh skinId

        _ <- Hree.updateNode scene nodeId0 (\n -> n { Hree.nodeMesh = Just meshId })

        let track = Hree.linearRotation timePoints rotations
            animation = Hree.singleTransformClip nodeId2 track

        st <- Time.now
        taskBoard <- Hree.newSceneTaskBoard scene
        _ <- Hree.addSceneTask taskBoard (Hree.AnimationTask st animation (Hree.AnimationTaskOption True False Nothing))

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        let light = Hree.directionalLight (V3 0.5 (-1) (-0.5)) (V3 1 1 1) 1
        _ <- Hree.addLight scene light

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
