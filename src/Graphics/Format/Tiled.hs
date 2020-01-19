module Graphics.Format.Tiled
    ( tileBoundingUpLeft
    ) where

import Graphics.Format.Tiled.Types
import Linear (V2(..))

tileBoundingUpLeft :: Orientation -> V2 Int -> V2 Int -> V2 Int -> V2 Int -> Int -> StaggerAxis -> StaggerIndex -> Int -> V2 Float
tileBoundingUpLeft OrientationOrthogonal (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit _ _ _ =
    let x = fromIntegral (ox + ix * width + offsetX) / fromIntegral unit
        y = fromIntegral (oy - iy * height - offsetY) / fromIntegral unit
    in V2 x y
tileBoundingUpLeft OrientationIsometric (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit _ _ _ =
    let x = fromIntegral (ox * 2 + (ix - iy) * width + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 - (ix + iy) * height - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y
tileBoundingUpLeft OrientationStaggered (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit staggerAxis staggerIndex _ =
    let V2 sx sy = stagger2 staggerAxis staggerIndex 0 (V2 ix iy) (V2 width height)
        x = fromIntegral (ox * 2 + sx + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 + sy - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y
tileBoundingUpLeft OrientationHexagonal (V2 ox oy) (V2 ix iy) (V2 width height) (V2 offsetX offsetY) unit staggerAxis staggerIndex hexSide =
    let V2 sx sy = stagger2 staggerAxis staggerIndex hexSide (V2 ix iy) (V2 width height)
        x = fromIntegral (ox * 2 + sx + offsetX * 2) / fromIntegral (unit * 2)
        y = fromIntegral (oy * 2 + sy - offsetY * 2) / fromIntegral (unit * 2)
    in V2 x y

stagger2 :: StaggerAxis -> StaggerIndex -> Int -> V2 Int -> V2 Int -> V2 Int
stagger2 StaggerAxisX staggerIndex hexSide (V2 ix iy) (V2 width height) =
    let x = ix * (width + hexSide)
        y = case (staggerIndex, ix `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> - iy * height * 2
                (StaggerIndexEven, False) -> - (iy + 1) * height * 2
                (StaggerIndexOdd, False)  -> - iy * height * 2
                (StaggerIndexOdd, True)   -> - (iy + 1) * height * 2
    in V2 x y
stagger2 StaggerAxisY staggerIndex hexSide (V2 ix iy) (V2 width height) =
    let x = case (staggerIndex, iy `mod` 2 == 0) of
                (StaggerIndexEven, True)  -> - ix * width * 2
                (StaggerIndexEven, False) -> - (ix + 1) * width * 2
                (StaggerIndexOdd, False)  -> - ix * width * 2
                (StaggerIndexOdd, True)   -> - (ix + 1) * width * 2
        y = - iy * (height + hexSide)
    in V2 x y
