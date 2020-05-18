{-# LANGUAGE OverloadedStrings #-}
module ChildNode1 where

import qualified Chronos as Time (now)
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import Example
import qualified Graphics.Hree as Hree
import qualified Graphics.Hree.Animation as Animation
import qualified Graphics.Hree.Material.FlatColorMaterial as Material
import qualified Graphics.Hree.SceneTask as SceneTask
import qualified Graphics.UI.GLFW as GLFW
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
        (geometry, _) <- Hree.createBoxGeometry 0.5 0.1 0.1 scene
        let material = Material.flatColorMaterial (V4 0.1 0.1 0.95 1)
            childMaterial = Material.flatColorMaterial (V4 0.95 0.1 0.1 1)
            childMesh = Hree.Mesh geometry childMaterial Nothing
            mesh = Hree.Mesh geometry material Nothing
        childMeshId <- Hree.addedMeshId <$> Hree.addMesh scene childMesh
        meshId <- Hree.addedMeshId <$> Hree.addMesh scene mesh

        let childNode = Hree.newNode
                { Hree.nodeMesh = Just childMeshId
                , Hree.nodeTranslation = V3 0.5 0 0
                , Hree.nodeScale = V3 0.5 1 1
                , Hree.nodeInverseBindMatrix = inverseBindMatrix
                }
        childNodeId <- Hree.addNode scene childNode False

        let node = Hree.newNode
                { Hree.nodeMesh = Just meshId
                , Hree.nodeTranslation = V3 0 0 0
                , Hree.nodeChildren = BV.fromList [childNodeId]
                , Hree.nodeInverseBindMatrix = inverseBindMatrix
                }
        nodeId <- Hree.addNode scene node True

        let ms = 1000000
            timepoints = UV.fromList . map (* ms) $ [0, 5000, 10000]
            rotations = UV.fromList [axisAngle (V3 0 0 1) 0, axisAngle (V3 0 0 1) pi, axisAngle (V3 0 0 1) (2 * pi)]
            track = Animation.linearRotation timepoints rotations
            channel1 = Animation.singleTransformChannel nodeId track
            channel2 = Animation.singleTransformChannel childNodeId track
            animation = Animation.animationClip (BV.fromList [channel1, channel2])

        st <- Time.now
        taskBoard <- SceneTask.newSceneTaskBoard scene
        _ <- SceneTask.addSceneTask taskBoard (SceneTask.AnimationTask st animation (SceneTask.AnimationTaskOption True False))

        camera <- Hree.newCamera proj la
        _ <- setCameraMouseControl w camera

        GLFW.setWindowSizeCallback w (Just (resizeWindowWithCamera camera))
        return (renderer, scene, camera, taskBoard)

    onDisplay (r, s, c, taskBoard) w = do
        render
        GLFW.pollEvents
        t <- Time.now
        SceneTask.runSceneTasksOnBoard taskBoard t
        onDisplay (r, s, c, taskBoard) w

        where
        render = do
            Hree.renderScene r s c
            GLFW.swapBuffers w
