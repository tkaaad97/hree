{-# LANGUAGE ForeignFunctionInterface #-}
module GLContext
    ( runOnOSMesaContext
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Foreign (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr,
                           mallocForeignPtrBytes, withForeignPtr)
import qualified Graphics.GL as GL

foreign import ccall "OSMesaCreateContextExt" ffiOSMesaCreateContextExt
    :: GL.GLenum -> GL.GLint -> GL.GLint -> GL.GLint -> Ptr () -> IO (Ptr ())

foreign import ccall "OSMesaMakeCurrent" ffiOSMesaMakeCurrent
    :: Ptr () -> Ptr () -> GL.GLenum -> GL.GLsizei -> GL.GLsizei -> IO GL.GLboolean

foreign import ccall "OSMesaDestroyContext" ffiOSMesaDestroyContext
    :: Ptr () -> IO ()

depthBits :: GL.GLint
depthBits = 24

stencilBits :: GL.GLint
stencilBits = 0

accumBits :: GL.GLint
accumBits = 0


runOnOSMesaContext :: GL.GLsizei -> GL.GLsizei -> IO a -> IO a
runOnOSMesaContext width height action =
    bracket first last mid
    where
    first = do
        ctx <- ffiOSMesaCreateContextExt GL.GL_RGBA depthBits stencilBits accumBits nullPtr
        unless (ctx /= nullPtr) $
            error "OSMesaCreateContextExt failed"
        buffer <- mallocForeignPtrBytes (fromIntegral $ width * height * 4)
        return (ctx, buffer)

    mid (ctx, buffer) = withForeignPtr (buffer :: ForeignPtr GL.GLubyte) $ \p -> do
        r <- ffiOSMesaMakeCurrent ctx (castPtr p) GL.GL_UNSIGNED_BYTE width height
        unless (r == GL.GL_TRUE) $
            error "OSMesaMakeCurrent failed"
        action

    last (ctx, buffer) = do
        ffiOSMesaDestroyContext ctx
        finalizeForeignPtr buffer
