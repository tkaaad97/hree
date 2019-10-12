{-# LANGUAGE OverloadedStrings #-}
module GLTFSpec
    ( spec
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as BV
import qualified Graphics.Format.GLTF as GLTF
import Test.Hspec


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
                                    }
                                ]
                            , GLTF.meshName = Nothing
                            }
                        ]
                    , GLTF.gltfBuffers = BV.fromList
                        [ GLTF.Buffer
                            { GLTF.bufferUri = "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA="
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
                            { GLTF.accessorBufferView = 0
                            , GLTF.accessorByteOffset = 0
                            , GLTF.accessorComponentType = GLTF.UnsignedShort'
                            , GLTF.accessorCount = 3
                            , GLTF.accessorType = GLTF.Scalar
                            , GLTF.accessorNormalized = False
                            , GLTF.accessorName = Nothing
                            }
                        , GLTF.Accessor
                            { GLTF.accessorBufferView = 1
                            , GLTF.accessorByteOffset = 0
                            , GLTF.accessorComponentType = GLTF.Float'
                            , GLTF.accessorCount = 3
                            , GLTF.accessorType = GLTF.Vec3
                            , GLTF.accessorNormalized = False
                            , GLTF.accessorName = Nothing
                            }
                        ]
                    , GLTF.gltfImages = mempty
                    }
            parsed <- GLTF.loadGLTFFile "test/files/minimal1.gltf"
            parsed `shouldBe` expected
