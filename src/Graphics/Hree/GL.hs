module Graphics.Hree.GL
    ( mkBuffer
    ) where

import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(..))
import Graphics.Hree.GL.Types
import qualified Graphics.Rendering.OpenGL as GL

mkBuffer :: GL.BufferTarget -> BufferSource -> IO GL.BufferObject
mkBuffer target (BufferSource vec usage) = do
    let n = V.length vec
        size = fromIntegral $ n * sizeOf (V.head vec)
    buffer <- GL.genObjectName
    GL.bindBuffer target GL.$= Just buffer
    V.unsafeWith vec $ \ptr -> GL.bufferData target GL.$= (size, ptr, usage)
    GL.bindBuffer target GL.$= Nothing
    return buffer
