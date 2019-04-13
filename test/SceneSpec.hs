module SceneSpec
    ( spec
    ) where

import GLContext
import Test.Hspec

spec :: Spec
spec = describe "addMesh" $
    runIO $ runOnOSMesaContext width height (putStrLn "hello")

    where
    width = 640
    height = 480
