{-# LANGUAGE OverloadedStrings #-}
module GLTFSpec
    ( spec
    ) where

import qualified Codec.Picture as Picture
import qualified Data.Map.Strict as Map
import qualified Data.Vector as BV
import qualified Hree.Loader.GLTF as GLTF
import Test.Hspec

emptyNode :: GLTF.Node
emptyNode = GLTF.Node Nothing mempty Nothing Nothing Nothing Nothing Nothing Nothing Nothing

spec :: Spec
spec = do
    describe "parse" $ do
        it "minimal1" $ do
            let expected = GLTF.GLTF
                    { GLTF.gltfScenes = BV.fromList
                        [ GLTF.Scene
                            { GLTF.sceneNodes = BV.fromList [ 0 ]
                            , GLTF.sceneName = Nothing
                            }
                        ]
                    , GLTF.gltfNodes = BV.fromList
                        [ GLTF.Node
                            { GLTF.nodeCamera = Nothing
                            , GLTF.nodeChildren = BV.empty
                            , GLTF.nodeSkin = Nothing
                            , GLTF.nodeMatrix = Nothing
                            , GLTF.nodeMesh = Just 0
                            , GLTF.nodeRotation = Nothing
                            , GLTF.nodeScale = Nothing
                            , GLTF.nodeTranslation = Nothing
                            , GLTF.nodeName = Nothing
                            }
                        ]
                    , GLTF.gltfMeshes = BV.fromList
                        [ GLTF.Mesh
                            { GLTF.meshPrimitives = BV.fromList
                                [ GLTF.Primitive
                                    { GLTF.primitiveAttributes = Map.fromList
                                        [ ("POSITION", 1) ]
                                    , GLTF.primitiveIndices = Just 0
                                    , GLTF.primitiveMaterial = Nothing
                                    , GLTF.primitiveMode = Nothing
                                    , GLTF.primitiveExtensions = Nothing
                                    , GLTF.primitiveExtras = Nothing
                                    }
                                ]
                            , GLTF.meshName = Nothing
                            }
                        ]
                    , GLTF.gltfBuffers = BV.fromList
                        [ GLTF.Buffer
                            { GLTF.bufferUri = Just "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA="
                            , GLTF.bufferByteLength = 44
                            }
                        ]
                    , GLTF.gltfBufferViews = BV.fromList
                        [ GLTF.BufferView
                            { GLTF.bufferViewBuffer = 0
                            , GLTF.bufferViewByteOffset = 0
                            , GLTF.bufferViewByteLength = 6
                            , GLTF.bufferViewByteStride = Nothing
                            , GLTF.bufferViewTarget = Just 34963
                            , GLTF.bufferViewName = Nothing
                            }
                        , GLTF.BufferView
                            { GLTF.bufferViewBuffer = 0
                            , GLTF.bufferViewByteOffset = 8
                            , GLTF.bufferViewByteLength = 36
                            , GLTF.bufferViewByteStride = Nothing
                            , GLTF.bufferViewTarget = Just 34962
                            , GLTF.bufferViewName = Nothing
                            }
                        ]
                    , GLTF.gltfAccessors = BV.fromList
                        [ GLTF.Accessor
                            { GLTF.accessorBufferView = Just 0
                            , GLTF.accessorByteOffset = 0
                            , GLTF.accessorComponentType = GLTF.UnsignedShort'
                            , GLTF.accessorCount = 3
                            , GLTF.accessorType = GLTF.Scalar
                            , GLTF.accessorNormalized = False
                            , GLTF.accessorName = Nothing
                            }
                        , GLTF.Accessor
                            { GLTF.accessorBufferView = Just 1
                            , GLTF.accessorByteOffset = 0
                            , GLTF.accessorComponentType = GLTF.Float'
                            , GLTF.accessorCount = 3
                            , GLTF.accessorType = GLTF.Vec3
                            , GLTF.accessorNormalized = False
                            , GLTF.accessorName = Nothing
                            }
                        ]
                    , GLTF.gltfImages = mempty
                    , GLTF.gltfSamplers = mempty
                    , GLTF.gltfTextures = mempty
                    , GLTF.gltfMaterials = mempty
                    , GLTF.gltfAnimations = mempty
                    , GLTF.gltfSkins = mempty
                    }
            parsed <- GLTF.loadGLTFFile "test/files/minimal1.gltf"
            parsed `shouldBe` expected

    describe "createImageFromUri" $ do
        let gen 0 0 = Picture.PixelRGBA8 255 0 0 255
            gen 1 0 = Picture.PixelRGBA8 0 255 0 255
            gen 0 1 = Picture.PixelRGBA8 0 0 255 255
            gen _ _ = Picture.PixelRGBA8 255 255 255 255
            expected = Picture.generateImage gen 2 2
            expectImage :: Picture.Image Picture.PixelRGBA8 -> Expectation
            expectImage img = do
                Picture.imageWidth img `shouldBe` 2
                Picture.imageHeight img `shouldBe` 2
                Picture.imageData img `shouldBe` Picture.imageData expected
        it "work with file" $ do
            img <- GLTF.createImageFromUri "test/files" "test1.png" Nothing
            expectImage img
        it "work with base64 embeded data" $ do
            let uri = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAABhGlDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AcxV9bpaVUHCwiIpKhOlkQFXHUKhShQqgVWnUwufQLmjQkKS6OgmvBwY/FqoOLs64OroIg+AHi4uqk6CIl/i8ptIjx4Lgf7+497t4B/kaFqWbXOKBqlpFOJoRsblUIviKMCELox7DETH1OFFPwHF/38PH1Ls6zvM/9OXqUvMkAn0A8y3TDIt4gnt60dM77xFFWkhTic+Ixgy5I/Mh12eU3zkWH/TwzamTS88RRYqHYwXIHs5KhEk8RxxRVo3x/1mWF8xZntVJjrXvyF0by2soy12kOIYlFLEGEABk1lFGBhTitGikm0rSf8PAPOn6RXDK5ymDkWEAVKiTHD/4Hv7s1C5MTblIkAXS/2PbHCBDcBZp12/4+tu3mCRB4Bq60tr/aAGY+Sa+3tdgR0LsNXFy3NXkPuNwBBp50yZAcKUDTXygA72f0TTmg7xYIr7m9tfZx+gBkqKvUDXBwCIwWKXvd492hzt7+PdPq7wc0hnKOt4d7nAAAAAlwSFlzAAAuIwAALiMBeKU/dgAAAAd0SU1FB+MKDAgXGTOZGXsAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJTVBXgQ4XAAAAFklEQVQI12P4z8DA8J+BkYHh////DAAe9gT9SMYJHwAAAABJRU5ErkJggg=="
            img <- GLTF.createImageFromUri "" uri Nothing
            expectImage img

    describe "searchCommonRoot" $ do
        it "case 1" $ do
            let rootNode = emptyNode { GLTF.nodeChildren = BV.fromList [1] }
                childNode0 = emptyNode { GLTF.nodeChildren = BV.fromList [2] }
                childNode1 = emptyNode { GLTF.nodeChildren = BV.fromList [3] }
                childNode2 = emptyNode
                nodes = BV.fromList [rootNode, childNode0, childNode1, childNode2]
                joints = BV.fromList [1,2,3]
            result <- GLTF.searchCommonRoot nodes joints
            result `shouldBe` 0

        it "case 2" $ do
            let node0 = emptyNode
                node1 = emptyNode
                node2 = emptyNode { GLTF.nodeChildren = BV.fromList [0] }
                node3 = emptyNode { GLTF.nodeChildren = BV.fromList [1] }
                node4 = emptyNode { GLTF.nodeChildren = BV.fromList [2, 3] }
                node5 = emptyNode { GLTF.nodeChildren = BV.fromList [4] }
                node6 = emptyNode { GLTF.nodeChildren = BV.fromList [5] }
                node7 = emptyNode { GLTF.nodeChildren = BV.fromList [6] }
                nodes = BV.fromList [node0, node1, node2, node3, node4, node5, node6, node7]
                joints = BV.fromList [0,1]
            result <- GLTF.searchCommonRoot nodes joints
            result `shouldBe` 4
