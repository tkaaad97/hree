{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Graphics.Hree.Light
    ( Light(..)
    , DirectionalLight(..)
    , PointLight(..)
    , SpotLight(..)
    , LightBlock(..)
    , MaxLightCount
    , directionalLightType
    , pointLightType
    , spotLightType
    , marshalLight
    , unmarshalLight
    , lightsByteSize
    ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Storable.Sized as SV (Vector)
import GHC.TypeNats (Nat, natVal)
import Graphics.Hree.GL.Block (Block(..), Elem(..), Element(..))
import Graphics.Hree.GL.Types (Vec2, Vec3)
import Linear (Additive(zero))

data Light =
    DirectionalLight' DirectionalLight |
    PointLight' PointLight |
    SpotLight' SpotLight
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
    , lightPadding        :: !Vec2
    } deriving (Show, Eq)

type MaxLightCount = (16 :: Nat)

data LightBlock = LightBlock
    { lightBlockLights :: !(SV.Vector MaxLightCount (Elem LightStruct))
    , lightBlockCount  :: !Int32
    } deriving (Show, Eq)

directionalLightType, pointLightType, spotLightType :: Int32
directionalLightType = 0
pointLightType = 1
spotLightType = 2

marshalLight :: Light -> LightStruct
marshalLight (DirectionalLight' (DirectionalLight direction color intensity)) =
    LightStruct direction 0 color intensity Linear.zero 0 0 directionalLightType Linear.zero
marshalLight (PointLight' (PointLight position range color intensity)) =
    LightStruct Linear.zero range color intensity position 0 0 pointLightType Linear.zero
marshalLight (SpotLight' (SpotLight position direction range inner outer color intensity)) =
    LightStruct direction range color intensity position inner outer spotLightType Linear.zero

unmarshalLight :: LightStruct -> Maybe Light
unmarshalLight a
    | lightType a == directionalLightType = Just . DirectionalLight' $ DirectionalLight (lightDirection a) (lightColor a) (lightIntensity a)
    | lightType a == pointLightType = Just . PointLight' $ PointLight (lightPosition a) (lightRange a) (lightColor a) (lightIntensity a)
    | lightType a == spotLightType = Just . SpotLight' $ SpotLight (lightPosition a) (lightDirection a) (lightRange a) (lightInnerConeAngle a) (lightOuterConeAngle a) (lightColor a) (lightIntensity a)
    | otherwise = Nothing

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
        padding <- peekByteOffStd140 ptr (off + 56)
        return $ LightStruct direction range color intensity position inner outer type' padding

    pokeByteOffStd140 ptr off (LightStruct direction range color intensity position inner outer type' padding) = do
        pokeByteOffStd140 ptr off direction
        pokeByteOffStd140 ptr (off + 12) range
        pokeByteOffStd140 ptr (off + 16) color
        pokeByteOffStd140 ptr (off + 28) intensity
        pokeByteOffStd140 ptr (off + 32) position
        pokeByteOffStd140 ptr (off + 44) inner
        pokeByteOffStd140 ptr (off + 48) outer
        pokeByteOffStd140 ptr (off + 52) type'
        pokeByteOffStd140 ptr (off + 56) padding

instance Element LightStruct where
    elemAlignmentStd140 _ = 16
    elemStrideStd140 _ = 64

instance Block LightBlock where
    alignmentStd140 _ = 16
    sizeOfStd140 _ = lightsByteSize + 4

    peekByteOffStd140 ptr off = do
        lights <- peekByteOffStd140 ptr off
        count <- peekByteOffStd140 ptr (off + lightsByteSize)
        return $ LightBlock lights count

    pokeByteOffStd140 ptr off (LightBlock lights count) = do
        pokeByteOffStd140 ptr off lights
        pokeByteOffStd140 ptr (off + lightsByteSize) count

lightsByteSize :: Int
lightsByteSize = sizeOfStd140 (Proxy :: Proxy LightStruct) * (fromIntegral . natVal $ (Proxy :: Proxy MaxLightCount))
