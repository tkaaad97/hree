{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
module GLContext
    ( runOnOSMesaContext
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Foreign (Ptr, castPtr, nullPtr, pokeByteOff, withArray)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr,
                           mallocForeignPtrBytes, withForeignPtr)
import qualified Graphics.GL as GL

foreign import ccall "OSMesaCreateContextExt" ffiOSMesaCreateContextExt
    :: GL.GLenum -> GL.GLint -> GL.GLint -> GL.GLint -> Ptr () -> IO (Ptr ())

foreign import ccall "OSMesaCreateContextAttribs" ffiOSMesaCreateContextAttribs
    :: Ptr GL.GLint -> Ptr () -> IO (Ptr ())

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
        ctx <- createContext width height
        unless (ctx /= nullPtr) $
            error "OSMesaCreateContextAttribs failed"
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

createContext :: GL.GLint -> GL.GLint -> IO (Ptr ())
createContext width height =
    withArray attribList $ flip ffiOSMesaCreateContextAttribs nullPtr
    where
    writeAttrib p (i, a) = pokeByteOff p i a
    attribList =
        [ OSMESA_FORMAT, GL.GL_RGBA
        , OSMESA_DEPTH_BITS, depthBits
        , OSMESA_STENCIL_BITS, stencilBits
        , OSMESA_ACCUM_BITS, accumBits
        , 0
        ]

pattern OSMESA_WIDTH                 = 0x20
pattern OSMESA_HEIGHT                = 0x21
pattern OSMESA_FORMAT                = 0x22
pattern OSMESA_TYPE                  = 0x23
pattern OSMESA_MAX_WIDTH             = 0x24
pattern OSMESA_MAX_HEIGHT            = 0x25
pattern OSMESA_DEPTH_BITS            = 0x30
pattern OSMESA_STENCIL_BITS          = 0x31
pattern OSMESA_ACCUM_BITS            = 0x32
pattern OSMESA_PROFILE               = 0x33
pattern OSMESA_CORE_PROFILE          = 0x34
pattern OSMESA_COMPAT_PROFILE        = 0x35
pattern OSMESA_CONTEXT_MAJOR_VERSION = 0x36
pattern OSMESA_CONTEXT_MINOR_VERSION = 0x37
