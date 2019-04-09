{-# LANGUAGE ForeignFunctionInterface #-}
module SceneSpec
    ( spec
    ) where

import Foreign (Ptr, nullPtr)
import qualified Graphics.GL as GL
import Test.Hspec

foreign import ccall "OSMesaCreateContextExt" ffiOSMesaCreateContextExt
    :: GL.GLenum -> GL.GLint -> GL.GLint -> GL.GLint -> Ptr () -> IO (Ptr ())

foreign import ccall "OSMesaMakeCurrent" ffiOSMesaMakeCurrent
    :: Ptr () -> Ptr () -> GL.GLenum -> GL.GLsizei -> GL.GLsizei -> IO GL.GLboolean

spec :: Spec
spec = describe "addMesh" $
    runIO $ do
        ctx <- ffiOSMesaCreateContextExt GL.GL_RGBA 16 0 0 nullPtr
        if ctx == nullPtr
            then putStrLn "failure"
            else putStrLn "success"
        putStrLn "hello"
