{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Graphics.Hree.Light
    ( Light(..)
    , LightBlock(..)
    , LightStruct(..)
    , MaxLightCount
    , DirectionalLight(..)
    , PointLight(..)
    , SpotLight(..)
    , directionalLight
    , directionalLightType
    , lightsByteSize
    , marshalLight
    , maxLightCount
    , pointLight
    , pointLightType
    , spotLight
    , spotLightType
    , unmarshalLight
    ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable as SV (slice)
import qualified Data.Vector.Storable.Sized as Sized (Vector, fromSized)
import GHC.TypeNats (Nat, natVal)
import Graphics.Hree.GL.Block (Block(..), Elem(..), Element(..))
import Graphics.Hree.GL.Types (Vec3)
import Linear (Additive(zero))

data Light =
    DirectionalLight' !DirectionalLight |
    PointLight' !PointLight |
    SpotLight' !SpotLight |
    UnknownLight !LightStruct
    deriving (Show, Eq)

data DirectionalLight = DirectionalLight
    { dlDirection :: !Vec3
    , dlColor     :: !Vec3
    , dlIntensity :: !Float
    } deriving (Show, Eq)

data PointLight = PointLight
    { plPosition  :: !Vec3
    , plRange     :: !Float
    , plColor     :: !Vec3
    , plIntensity :: !Float
    } deriving (Show, Eq)

data SpotLight = SpotLight
    { slPosition       :: !Vec3
    , slDirection      :: !Vec3
    , slRange          :: !Float
    , slInnerConeAngle :: !Float
    , slOuterConeAngle :: !Float
    , slColor          :: !Vec3
    , slIntensity      :: !Float
    } deriving (Show, Eq)

data LightStruct = LightStruct
    { lightDirection      :: !Vec3
    , lightRange          :: !Float
    , lightColor          :: !Vec3
    , lightIntensity      :: !Float
    , lightPosition       :: !Vec3
    , lightInnerConeAngle :: !Float
    , lightOuterConeAngle :: !Float
    , lightType           :: !Int32
    , lightPadding1       :: !Float
    , lightPadding2       :: !Float
    } deriving (Show, Eq)

type MaxLightCount = (16 :: Nat)

data LightBlock = LightBlock
    { lightBlockLightCount :: !Int32
    , lightBlockLights     :: !(Sized.Vector MaxLightCount (Elem LightStruct))
    } deriving (Show)

directionalLight :: Vec3 -> Vec3 -> Float -> Light
directionalLight dir color intensity = DirectionalLight' (DirectionalLight dir color intensity)

pointLight :: Vec3 -> Float -> Vec3 -> Float -> Light
pointLight pos range color intensity = PointLight' (PointLight pos range color intensity)

spotLight :: Vec3 -> Vec3 -> Float -> Float -> Float -> Vec3 -> Float -> Light
spotLight pos dir range inner outer color intensity = SpotLight' (SpotLight pos dir range inner outer color intensity)

directionalLightType, pointLightType, spotLightType :: Int32
directionalLightType = 0
pointLightType = 1
spotLightType = 2

marshalLight :: Light -> LightStruct
marshalLight (DirectionalLight' (DirectionalLight direction color intensity)) =
    LightStruct direction 0 color intensity Linear.zero 0 0 directionalLightType 0 0
marshalLight (PointLight' (PointLight position range color intensity)) =
    LightStruct Linear.zero range color intensity position 0 0 pointLightType 0 0
marshalLight (SpotLight' (SpotLight position direction range inner outer color intensity)) =
    LightStruct direction range color intensity position inner outer spotLightType 0 0
marshalLight (UnknownLight a) = a

unmarshalLight :: LightStruct -> Light
unmarshalLight a
    | lightType a == directionalLightType = DirectionalLight' $ DirectionalLight (lightDirection a) (lightColor a) (lightIntensity a)
    | lightType a == pointLightType = PointLight' $ PointLight (lightPosition a) (lightRange a) (lightColor a) (lightIntensity a)
    | lightType a == spotLightType = SpotLight' $ SpotLight (lightPosition a) (lightDirection a) (lightRange a) (lightInnerConeAngle a) (lightOuterConeAngle a) (lightColor a) (lightIntensity a)
    | otherwise = UnknownLight a

instance Block LightStruct where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = 64

    peekByteOffStd140 ptr off = do
        direction <- peekByteOffStd140 ptr off
        range <- peekByteOffStd140 ptr (off + 12)
        color <- peekByteOffStd140 ptr (off + 16)
        intensity <- peekByteOffStd140 ptr (off + 28)
        position <- peekByteOffStd140 ptr (off + 32)
        inner <- peekByteOffStd140 ptr (off + 44)
        outer <- peekByteOffStd140 ptr (off + 48)
        type' <- peekByteOffStd140 ptr (off + 52)
        padding1 <- peekByteOffStd140 ptr (off + 56)
        padding2 <- peekByteOffStd140 ptr (off + 60)
        return $ LightStruct direction range color intensity position inner outer type' padding1 padding2

    pokeByteOffStd140 ptr off (LightStruct direction range color intensity position inner outer type' padding1 padding2) = do
        pokeByteOffStd140 ptr off direction
        pokeByteOffStd140 ptr (off + 12) range
        pokeByteOffStd140 ptr (off + 16) color
        pokeByteOffStd140 ptr (off + 28) intensity
        pokeByteOffStd140 ptr (off + 32) position
        pokeByteOffStd140 ptr (off + 44) inner
        pokeByteOffStd140 ptr (off + 48) outer
        pokeByteOffStd140 ptr (off + 52) type'
        pokeByteOffStd140 ptr (off + 56) padding1
        pokeByteOffStd140 ptr (off + 60) padding2

instance Element LightStruct where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

instance Block LightBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = lightsByteSize + 16

    peekByteOffStd140 ptr off = do
        count <- peekByteOffStd140 ptr off
        lights <- peekByteOffStd140 ptr (off + 16)
        return $ LightBlock count lights

    pokeByteOffStd140 ptr off (LightBlock count lights) = do
        pokeByteOffStd140 ptr off count
        pokeByteOffStd140 ptr (off + 16) lights

instance Eq LightBlock where
    a == b =
        let counta = fromIntegral . lightBlockLightCount $ a
            countb = fromIntegral . lightBlockLightCount $ b
            slicea = SV.slice 0 counta . Sized.fromSized . lightBlockLights $ a
            sliceb = SV.slice 0 countb . Sized.fromSized . lightBlockLights $ b
        in counta == countb && slicea == sliceb

maxLightCount :: Int
maxLightCount = fromIntegral . natVal $ (Proxy :: Proxy MaxLightCount)

lightsByteSize :: Int
lightsByteSize = sizeOfStd140 (Proxy :: Proxy LightStruct) * maxLightCount
