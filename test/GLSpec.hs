{-# LANGUAGE OverloadedStrings #-}
module GLSpec
    ( spec
    ) where

import Data.Bits (complement)
import Foreign (alloca, peek)
import GLContext
import qualified GLW
import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.EnableCap as EnableCap
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import qualified Graphics.Hree.GL as Hree
import qualified Graphics.Hree.GL.Types as Hree
import Test.Hspec

getBooleanv :: GL.GLenum -> IO GL.GLboolean
getBooleanv param = alloca $ \p -> do
    GL.glGetBooleanv param p
    peek p

getIntegerv :: GL.GLenum -> IO GL.GLint
getIntegerv param = alloca $ \p -> do
    GL.glGetIntegerv param p
    peek p

spec :: Spec
spec = do
    describe "setCullFace" $ do
        it "enable cull face and set mode" . runOnOSMesaContext 1 1 $ do
            GL.glDisable GL.GL_CULL_FACE
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

            Hree.setCullFace Nothing (Just CullFaceMode.glFrontAndBack)

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            getIntegerv GL.GL_CULL_FACE_MODE >>= flip shouldBe GL.GL_FRONT_AND_BACK

        it "change cull face mode" . runOnOSMesaContext 1 1 $ do
            GL.glEnable GL.GL_CULL_FACE
            GL.glCullFace GL.GL_BACK
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            beforeMode <- getIntegerv GL.GL_CULL_FACE_MODE
            beforeMode `shouldBe` GL.GL_BACK

            Hree.setCullFace (Just CullFaceMode.glBack) (Just CullFaceMode.glFront)

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE
            getIntegerv GL.GL_CULL_FACE_MODE >>= flip shouldBe GL.GL_FRONT

        it "disable cull face" . runOnOSMesaContext 1 1 $ do
            GL.glEnable GL.GL_CULL_FACE
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_TRUE

            Hree.setCullFace (Just CullFaceMode.glFront) Nothing

            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

            GL.glEnable GL.GL_CULL_FACE
            Hree.setCullFace Nothing Nothing
            getBooleanv GL.GL_CULL_FACE >>= flip shouldBe GL.GL_FALSE

    describe "setDepthOption" $ do
        it "enable depth test" . runOnOSMesaContext 1 1 $ do
            GL.glDisable GL.GL_DEPTH_TEST
            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_TRUE

        it "enable depth mask" . runOnOSMesaContext 1 1 $ do
            GL.glDepthMask GL.GL_FALSE
            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_TRUE

        it "set depth function" . runOnOSMesaContext 1 1 $ do
            GL.glDepthFunc GL.GL_LESS
            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_LESS

            Hree.setDepthOption Nothing (Hree.DepthOption True True DepthFunction.glGreater)

            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_GREATER

        it "set depth func if option changed" . runOnOSMesaContext 1 1 $ do
            GL.glDepthFunc GL.GL_NEVER
            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_NEVER

            Hree.setDepthOption (Just (Hree.DepthOption True True DepthFunction.glNever)) (Hree.DepthOption True True DepthFunction.glLess)

            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_LESS

        it "should not enable depth test if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glDisable GL.GL_DEPTH_TEST
            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption (Just (Hree.DepthOption True True DepthFunction.glLess)) (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_TEST >>= flip shouldBe GL.GL_FALSE

        it "should not enable depth mask if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glDepthMask GL.GL_FALSE
            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_FALSE

            Hree.setDepthOption (Just (Hree.DepthOption True True DepthFunction.glLess)) (Hree.DepthOption True True DepthFunction.glLess)

            getBooleanv GL.GL_DEPTH_WRITEMASK >>= flip shouldBe GL.GL_FALSE

        it "should not set depth func if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glDepthFunc GL.GL_NEVER
            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_NEVER

            Hree.setDepthOption (Just (Hree.DepthOption True True DepthFunction.glLess)) (Hree.DepthOption True True DepthFunction.glLess)

            getIntegerv GL.GL_DEPTH_FUNC >>= flip shouldBe GL.GL_NEVER

    describe "setBlendingOption" $ do
        it "enable blend" . runOnOSMesaContext 1 1 $ do
            GL.glDisable GL.GL_BLEND
            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_FALSE

            Hree.setBlendingOption Nothing
                (Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO)))

            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_TRUE

        it "set blend equation" . runOnOSMesaContext 1 1 $ do
            GL.glBlendEquation GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_ADD

            Hree.setBlendingOption Nothing
                (Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_REVERSE_SUBTRACT (GL.GL_ONE, GL.GL_ZERO)))

            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_SUBTRACT
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_REVERSE_SUBTRACT

        it "set blend function" . runOnOSMesaContext 1 1 $ do
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

        it "should not enable blend if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glDisable GL.GL_BLEND
            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_FALSE

            let option = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
            Hree.setBlendingOption (Just option) option

            getBooleanv GL.GL_BLEND >>= flip shouldBe GL.GL_FALSE

        it "set blend equation if option changed" . runOnOSMesaContext 1 1 $ do
            GL.glBlendEquation GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_ADD

            let beforeOption = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                option = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_REVERSE_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
            Hree.setBlendingOption (Just beforeOption) option

            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_SUBTRACT
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_REVERSE_SUBTRACT

        it "should not set blend equation if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glBlendEquation GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_ADD

            let option = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_REVERSE_SUBTRACT (GL.GL_ONE, GL.GL_ZERO))
            Hree.setBlendingOption (Just option) option

            getIntegerv GL.GL_BLEND_EQUATION_RGB >>= flip shouldBe GL.GL_FUNC_ADD
            getIntegerv GL.GL_BLEND_EQUATION_ALPHA >>= flip shouldBe GL.GL_FUNC_ADD

        it "set blend function if option changed" . runOnOSMesaContext 1 1 $ do
            GL.glBlendFunc GL.GL_ONE GL.GL_ZERO
            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_ZERO

            let beforeOption = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ONE, GL.GL_ZERO))
                option = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_COLOR))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_ALPHA))
            Hree.setBlendingOption (Just beforeOption) option

            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_SRC_COLOR
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_SRC_ALPHA

        it "should not set blend function if option not changed" . runOnOSMesaContext 1 1 $ do
            GL.glBlendFunc GL.GL_ONE GL.GL_ZERO
            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_ZERO

            let option = Hree.BlendingOption True
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_COLOR))
                    (Hree.BlendingSeparateOption GL.GL_FUNC_ADD (GL.GL_ZERO, GL.GL_SRC_ALPHA))
            Hree.setBlendingOption (Just option) option

            getIntegerv GL.GL_BLEND_SRC_RGB >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_SRC_ALPHA >>= flip shouldBe GL.GL_ONE
            getIntegerv GL.GL_BLEND_DST_RGB >>= flip shouldBe GL.GL_ZERO
            getIntegerv GL.GL_BLEND_DST_ALPHA >>= flip shouldBe GL.GL_ZERO

    describe "setStencilOption" $ do
        it "enable stencil test" . runOnOSMesaContext 1 1 $ do
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

        it "set each face stencil option" . runOnOSMesaContext 1 1 $ do
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
