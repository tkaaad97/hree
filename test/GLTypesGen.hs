module GLTypesGen
    ( DepthOptionGen(..)
    , BlendingSeparateOptionGen(..)
    , BlendingOptionGen(..)
    , StencilFuncArgsGen(..)
    , StencilOpArgsGen(..)
    , StencilOptionGen(..)
    , RenderOptionGen(..)
    ) where

import qualified GLW.Groups.CullFaceMode as CullFaceMode
import qualified GLW.Groups.DepthFunction as DepthFunction
import qualified GLW.Groups.StencilFunction as StencilFunction
import qualified GLW.Groups.StencilOp as StencilOp
import qualified Graphics.GL as GL
import Graphics.Hree.GL.Types
import Linear (V4(..))
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QuickCheck (choose, elements)

newtype DepthOptionGen = DepthOptionGen
    { unDepthOptionGen :: DepthOption
    } deriving (Show, Eq)

newtype BlendingSeparateOptionGen = BlendingSeparateOptionGen
    { unBlendingSeparateOptionGen :: BlendingSeparateOption
    } deriving (Show, Eq)

newtype BlendingOptionGen = BlendingOptionGen
    { unBlendingOptionGen :: BlendingOption
    } deriving (Show, Eq)

newtype StencilFuncArgsGen = StencilFuncArgsGen
    { unStencilFuncArgsGen :: StencilFuncArgs
    } deriving (Show, Eq)

newtype StencilOpArgsGen = StencilOpArgsGen
    { unStencilOpArgsGen :: StencilOpArgs
    } deriving (Show, Eq)

newtype FaceStencilOptionGen = FaceStencilOptionGen
    { unFaceStencilOptionGen :: FaceStencilOption
    } deriving (Show, Eq)

newtype StencilOptionGen = StencilOptionGen
    { unStencilOptionGen :: StencilOption
    } deriving (Show, Eq)

newtype RenderOptionGen = RenderOptionGen
    { unRenderOptionGen :: RenderOption
    } deriving (Show, Eq)

instance Arbitrary DepthOptionGen where
    arbitrary = do
        depthTest <- arbitrary
        depthMask <- arbitrary
        depthFunction <- QuickCheck.elements
            [ DepthFunction.glAlways
            , DepthFunction.glEqual
            , DepthFunction.glGequal
            , DepthFunction.glGreater
            , DepthFunction.glLequal
            , DepthFunction.glLess
            , DepthFunction.glNever
            , DepthFunction.glNotequal
            ]
        return . DepthOptionGen $ DepthOption depthTest depthMask depthFunction

instance Arbitrary BlendingSeparateOptionGen where
    arbitrary = do
        equation <- QuickCheck.elements
            [ GL.GL_FUNC_ADD
            , GL.GL_FUNC_SUBTRACT
            , GL.GL_FUNC_REVERSE_SUBTRACT
            , GL.GL_MIN
            , GL.GL_MAX
            ]
        src <- blendingFactor
        dst <- blendingFactor
        return . BlendingSeparateOptionGen $ BlendingSeparateOption equation (src, dst)

        where
            blendingFactor = QuickCheck.elements
                [ GL.GL_ZERO
                , GL.GL_ONE
                , GL.GL_SRC_COLOR
                , GL.GL_ONE_MINUS_SRC_COLOR
                , GL.GL_DST_COLOR
                , GL.GL_ONE_MINUS_DST_COLOR
                , GL.GL_SRC_ALPHA
                , GL.GL_ONE_MINUS_SRC_ALPHA
                , GL.GL_DST_ALPHA
                , GL.GL_ONE_MINUS_DST_ALPHA
                , GL.GL_CONSTANT_COLOR
                , GL.GL_ONE_MINUS_CONSTANT_COLOR
                , GL.GL_CONSTANT_ALPHA
                , GL.GL_ONE_MINUS_CONSTANT_ALPHA
                , GL.GL_SRC_ALPHA_SATURATE
                , GL.GL_SRC1_COLOR
                , GL.GL_ONE_MINUS_SRC1_COLOR
                , GL.GL_SRC1_ALPHA
                , GL.GL_ONE_MINUS_SRC1_ALPHA
                ]

instance Arbitrary BlendingOptionGen where
    arbitrary = do
        enabled <- arbitrary
        rgb <- unBlendingSeparateOptionGen <$> arbitrary
        alpha <- unBlendingSeparateOptionGen <$> arbitrary
        return . BlendingOptionGen $ BlendingOption enabled rgb alpha

instance Arbitrary StencilFuncArgsGen where
    arbitrary = do
        func <- QuickCheck.elements
            [ StencilFunction.glAlways
            , StencilFunction.glEqual
            , StencilFunction.glGequal
            , StencilFunction.glGreater
            , StencilFunction.glLequal
            , StencilFunction.glLess
            , StencilFunction.glNever
            , StencilFunction.glNotequal
            ]
        ref <- QuickCheck.choose (0, 0xFF)
        fmask <- arbitrary
        return . StencilFuncArgsGen $ StencilFuncArgs func ref fmask

instance Arbitrary StencilOpArgsGen where
    arbitrary = do
        sfail <- stencilOp
        dpfail <- stencilOp
        dppass <- stencilOp
        return . StencilOpArgsGen $ StencilOpArgs sfail dpfail dppass
        where
            stencilOp = QuickCheck.elements
                [ StencilOp.glDecr
                , StencilOp.glIncr
                , StencilOp.glInvert
                , StencilOp.glKeep
                , StencilOp.glReplace
                , StencilOp.glZero
                ]

instance Arbitrary FaceStencilOptionGen where
    arbitrary = do
        funcArgs <- unStencilFuncArgsGen <$> arbitrary
        opArgs <- unStencilOpArgsGen <$> arbitrary
        writeMask <- arbitrary
        return . FaceStencilOptionGen $ FaceStencilOption funcArgs opArgs writeMask

instance Arbitrary StencilOptionGen where
    arbitrary = do
        stencilTest <- arbitrary
        front <- unFaceStencilOptionGen <$> arbitrary
        back <- unFaceStencilOptionGen <$> arbitrary
        return . StencilOptionGen $ StencilOption stencilTest front back

instance Arbitrary RenderOptionGen where
    arbitrary = do
        hasCullFaceMode <- arbitrary
        cullFaceMode <- QuickCheck.elements
            [ CullFaceMode.glBack
            , CullFaceMode.glFront
            , CullFaceMode.glFrontAndBack
            ]
        flipSided <- arbitrary
        depthOption <- unDepthOptionGen <$> arbitrary
        blendingOption <- unBlendingOptionGen <$> arbitrary
        stencilOption <- unStencilOptionGen <$> arbitrary
        cmaskRed <- boolean
        cmaskGreen <- boolean
        cmaskBlue <- boolean
        cmaskAlpha <- boolean
        let cullFaceModeOption = if hasCullFaceMode
                then Just cullFaceMode
                else Nothing
            cmask = V4 cmaskRed cmaskGreen cmaskBlue cmaskAlpha
        return . RenderOptionGen $ RenderOption cullFaceModeOption flipSided depthOption blendingOption stencilOption cmask
        where
            boolean = QuickCheck.elements [GL.GL_FALSE, GL.GL_TRUE]
