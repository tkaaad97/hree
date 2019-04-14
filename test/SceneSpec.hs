module SceneSpec
    ( spec
    ) where

import Data.IORef (readIORef)
import qualified Data.Vector.Storable as Vector
import GLContext
import qualified GLW
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL.Vertex as Hree
import qualified Graphics.Hree.Material as Hree
import qualified Graphics.Hree.Mesh as Hree (Mesh(..))
import qualified Graphics.Hree.Scene as Hree
import Linear (V2(..), V3(..), V4(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "addMesh" $ do
        it "increment mesh counter" . runOnOSMesaContext width height $ do
            scene <- Hree.newScene
            geometry <- Hree.geometryFromVertexVector (GLW.BindingIndex 0) vs GL.GL_STREAM_DRAW scene
            let mesh = Hree.Mesh geometry material
            meshId <- Hree.addMesh scene mesh
            meshId `shouldBe` 1
            counter <- readIORef . Hree.sceneMeshCounter $ scene
            counter `shouldBe` 2

    where
    width = 640
    height = 480

    x = 1
    vs = Vector.fromList
        [ Hree.BasicVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 x x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        , Hree.BasicVertex (V3 0 x 0) (V3 0 0 0) (V2 0 0) (V4 1 1 1 1)
        ]

    material = Hree.testMaterial
