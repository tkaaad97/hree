{-# LANGUAGE OverloadedStrings #-}
module GLSpec
    ( spec
    ) where

import Data.Bits (complement)
import Data.Functor.Identity (Identity(..))
import Foreign (alloca, peek)
import GLContext
import GLTypesGen
import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL as Hree
import qualified Graphics.Hree.GL.Types as Hree
import Linear (V4(..))
import Test.Hspec
import Test.QuickCheck

getBooleanv :: GL.GLenum -> IO GL.GLboolean
getBooleanv param = alloca $ \p -> do
    GL.glGetBooleanv param p
    peek p

getIntegerv :: GL.GLenum -> IO GL.GLint
getIntegerv param = alloca $ \p -> do
    GL.glGetIntegerv param p
    peek p

toBoolean :: Bool -> GL.GLboolean
toBoolean True  = GL.GL_TRUE
toBoolean False = GL.GL_FALSE

spec :: Spec
spec = do
    describe "setCullFace" $ do
        runOnOSMesaContext 1 1 . it "enable cull face and set mode" $ do
            GL.glDisable GL.GL_CULL_FACE
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

            Hree.setCullFace Nothing (Just CullFaceMode.glFrontAndBack)

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            getIntegerv GL.GL_CULL_FACE_MODE >>= flip shouldBe GL.GL_FRONT_AND_BACK

        runOnOSMesaContext 1 1 . it "change cull face mode" $ do
            GL.glEnable GL.GL_CULL_FACE
            GL.glCullFace GL.GL_BACK
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            beforeMode <- getIntegerv GL.GL_CULL_FACE_MODE
            beforeMode `shouldBe` GL.GL_BACK

            Hree.setCullFace (Just (Just CullFaceMode.glBack)) (Just CullFaceMode.glFront)

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            getIntegerv GL.GL_CULL_FACE_MODE >>= flip shouldBe GL.GL_FRONT

        runOnOSMesaContext 1 1 . it "disable cull face" $ do
            GL.glEnable GL.GL_CULL_FACE
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE

            Hree.setCullFace (Just (Just CullFaceMode.glFront)) Nothing

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

            GL.glEnable GL.GL_CULL_FACE
            Hree.setCullFace Nothing Nothing
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

    describe "setDepthOption" $ do
        runOnOSMesaContext 1 1 . it "enable depth test" $ do
            GL.glDisable GL.GL_DEPTH_TEST
            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_TRUE

        runOnOSMesaContext 1 1 . it "enable depth mask" $ do
            GL.glDepthMask GL.GL_FALSE
            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_TRUE

        runOnOSMesaContext 1 1 . it "set depth function" $ do
            GL.glDepthFunc GL.GL_LESS
            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_LESS

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glGreater)

            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_GREATER

        runOnOSMesaContext 1 1 . it "set values if changed" . property $
            \(DepthOptionGen beforeOption, DepthOptionGen option) -> ioProperty $ do
                Hree.setDepthOption Nothing beforeOption
                Hree.getDepthOption >>= flip shouldBe beforeOption

                Hree.setDepthOption (Just option) option
                Hree.getDepthOption >>= flip shouldBe beforeOption

                Hree.setDepthOption (Just beforeOption) option
                Hree.getDepthOption >>= flip shouldBe option

    describe "setBlendingOption" $ do
        runOnOSMesaContext 1 1 . it "enable blend" $ do
            GL.glDisable GL.GL_BLEND
            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_FALSE

            Hree.setBlendingOption Nothing
                (Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO)))

            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_TRUE

        runOnOSMesaContext 1 1 . it "set blend equation" $ do
            GL.glBlendEquation GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_ADD

            Hree.setBlendingOption Nothing
                (Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_REVERSE_SUBTRACT (GL.GL_ONE, GL.GL_ZERO)))

            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_SUBTRACT
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_REVERSE_SUBTRACT

        runOnOSMesaContext 1 1 . it "set blend function" $ do
            GL.glBlendFunc GL.GL_ONE GL.GL_ZERO
            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_ZERO

            Hree.setBlendingOption Nothing
                (Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_COLOR))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_ALPHA)))

            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_SRC_COLOR
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_SRC_ALPHA

        runOnOSMesaContext 1 1 . it "set values if changed" . property $
                \(BlendingOptionGen beforeOption_, BlendingOptionGen option) -> ioProperty $ do
                    let beforeOption = beforeOption_ { Hree.blendingOptionEnabled = True }
                    Hree.setBlendingOption Nothing beforeOption
                    Hree.getBlendingOption >>= flip shouldBe beforeOption

                    Hree.setBlendingOption (Just option) option
                    Hree.getBlendingOption >>= flip shouldBe beforeOption

                    Hree.setBlendingOption (Just beforeOption) option
                    if Hree.blendingOptionEnabled option
                        then Hree.getBlendingOption >>= flip shouldBe option
                        else Hree.getBlendingOption >>= flip shouldBe False . Hree.blendingOptionEnabled

    describe "setStencilOption" $ do
        runOnOSMesaContext 1 1 . it "enable stencil test" $ do
            GL.glDisable GL.GL_STENCIL_TEST
            getBooleanv GL.GL_STENCIL_TEST >>= flip shouldBe GL.GL_FALSE

            let front = Hree.FaceStencilOption
                    (Hree.StencilFuncArgs StencilFunction.glAlways 0 (complement 0))
                    (Hree.StencilOpArgs StencilOp.glKeep StencilOp.glKeep StencilOp.glKeep)
                    (complement 0)
                back = front
                option = Hree.StencilOption True front back
            Hree.setStencilOption Nothing option

            getBooleanv GL.GL_STENCIL_TEST >>= flip shouldBe GL.GL_TRUE

        runOnOSMesaContext 1 1 . it "set each face stencil option" $ do
            let front = Hree.FaceStencilOption
                    (Hree.StencilFuncArgs StencilFunction.glAlways 0 0xF)
                    (Hree.StencilOpArgs StencilOp.glKeep StencilOp.glKeep StencilOp.glKeep)
                    0xF00F
                back = Hree.FaceStencilOption
                    (Hree.StencilFuncArgs StencilFunction.glNever 2 0xFF)
                    (Hree.StencilOpArgs StencilOp.glReplace StencilOp.glZero StencilOp.glInvert)
                    0xFF0F
                option = Hree.StencilOption True front back
            Hree.setStencilOption Nothing option
            Hree.getStencilOption >>= flip shouldBe option

        runOnOSMesaContext 1 1 . it "set values if changed" . property $
                \(StencilOptionGen beforeOption, StencilOptionGen option) -> ioProperty $ do
                    Hree.setStencilOption Nothing beforeOption
                    Hree.getStencilOption >>= flip shouldBe beforeOption

                    Hree.setStencilOption (Just option) option
                    Hree.getStencilOption >>= flip shouldBe beforeOption

                    Hree.setStencilOption (Just beforeOption) option
                    Hree.getStencilOption >>= flip shouldBe option

    describe "setColorMask" $ do
        runOnOSMesaContext 1 1 . it "set values if changed" . property $
                \((r0, g0, b0, a0), (r1, g1, b1, a1)) -> ioProperty $ do
                    let beforeOption = V4 (toBoolean r0) (toBoolean g0) (toBoolean b0) (toBoolean a0)
                        option = V4 (toBoolean r1) (toBoolean g1) (toBoolean b1) (toBoolean a1)
                    Hree.setColorMask Nothing beforeOption
                    Hree.getColorMask >>= flip shouldBe beforeOption

                    Hree.setColorMask (Just option) option
                    Hree.getColorMask >>= flip shouldBe beforeOption

                    Hree.setColorMask (Just beforeOption) option
                    Hree.getColorMask >>= flip shouldBe option

    describe "setRenderOption" $ do
        let assertRenderOption expected actual = do
                let Hree.RenderOption (Identity cullFace0) (Identity flipSided0) (Identity depth0) (Identity blending0) (Identity stencil0) (Identity cmask0) = expected
                    Hree.RenderOption (Identity cullFace1) (Identity flipSided1) (Identity depth1) (Identity blending1) (Identity stencil1) (Identity cmask1) = actual
                cullFace1 `shouldBe` cullFace0
                flipSided1 `shouldBe` flipSided0
                depth1 `shouldBe` depth0
                if Hree.blendingOptionEnabled blending0
                    then blending1 `shouldBe` blending0
                    else Hree.blendingOptionEnabled blending1 `shouldBe` False
                stencil1 `shouldBe` stencil0
                cmask1 `shouldBe` cmask0

        runOnOSMesaContext 1 1 . it "set values if changed" . withMaxSuccess 1000 . property $
            \(RenderOptionGen beforeOption, RenderOptionGen option) -> ioProperty $ do
                Hree.setRenderOption Nothing beforeOption
                result1 <- Hree.getRenderOption
                assertRenderOption beforeOption result1

                Hree.setRenderOption (Just option) option
                result2 <- Hree.getRenderOption
                assertRenderOption beforeOption result2

                Hree.setRenderOption (Just beforeOption) option
                result3 <- Hree.getRenderOption
                assertRenderOption option result3
