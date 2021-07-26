{-# LANGUAGE OverloadedStrings #-}
module ChildNode1 where

import qualified Chronos as Time (now)
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Graphics.UI.GLFW as GLFW
import qualified Hree
import Linear (V3(..), V4(..), axisAngle, inv44)
import Prelude hiding (init)

main :: IO ()
main =
    withWindow width height "child-node-1" init onDisplay

    where
    width  = 640
    height = 480
    defaultAspect = fromIntegral width / fromIntegral height

    proj = Hree.orthographic (-defaultAspect) defaultAspect (-1.0) 1.0 0.01 10.0

    la = Hree.lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)

    inverseBindMatrix = Linear.inv44 (V4 (V4 1 0 0 0) (V4 0 1 0 0.05) (V4 0 0 1 0) (V4 0 0 0 1))

    init w = do
        renderer <- Hree.newRenderer
        scene <- Hree.newScene
        let geometry = Hree.boxGeometry 0.5 0.1 0.1
            material = Hree.flatColorMaterial (V4 0.1 0.1 0.95 1)
            childMaterial = Hree.flatColorMaterial (V4 0.95 0.1 0.1 1)
        materialId <- Hree.addMaterial scene material
        childMaterialId <- Hree.addMaterial scene childMaterial
        let childMesh = Hree.mesh geometry childMaterialId
            mesh = Hree.mesh geometry materialId
        childMeshId <- Hree.addMesh scene childMesh
        meshId <- Hree.addMesh scene mesh

        let childNode = Hree.node
                { Hree.nodeTranslation = V3 0.5 0 0
                , Hree.nodeScale = V3 0.5 1 1
                , Hree.nodeInverseBindMatrix = inverseBindMatrix
                }
        childNodeId <- Hree.addNode scene childNode (Just childMeshId) False

        let node = Hree.node
                { Hree.nodeTranslation = V3 0 0 0
                , Hree.nodeChildren = BV.fromList [childNodeId]
                , Hree.nodeInverseBindMatrix = inverseBindMatrix
                }
        nodeId <- Hree.addNode scene node (Just meshId) True

        let ms = 1000000
            timepoints = UV.fromList . map (* ms) $ [0, 5000, 10000]
            rotations = UV.fromList [axisAngle (V3 0 0 1) 0, axisAngle (V3 0 0 1) pi, axisAngle (V3 0 0 1) (2 * pi)]
            track = Hree.linearRotation timepoints rotations
            channel1 = Hree.singleTransformChannel nodeId track
            channel2 = Hree.singleTransformChannel childNodeId track
            animation = Hree.animationClip (BV.fromList [channel1, channel2])

        st <- Time.now
        taskBoard <- Hree.newSceneTaskBoard scene
        _ <- Hree.addSceneTask taskBoard (Hree.AnimationTask st animation (Hree.AnimationTaskOption True False Nothing))

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        render
        GLFW.pollEvents
        t <- Time.now
        Hree.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
