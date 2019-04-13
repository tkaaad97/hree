{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE TypeApplications          #-}
module SceneSpec
    ( spec
    ) where

import Data.Proxy (Proxy(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Ptr, Storable(..), alloca, castPtr, nullPtr)
import Foreign.C.String (peekCAString)
import Foreign.C.Types (CChar(..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import qualified GLW
import qualified Graphics.GL as GL
import Test.Hspec

foreign import ccall "OSMesaCreateContextExt" ffiOSMesaCreateContextExt
    :: GL.GLenum -> GL.GLint -> GL.GLint -> GL.GLint -> Ptr () -> IO (Ptr ())

foreign import ccall "OSMesaMakeCurrent" ffiOSMesaMakeCurrent
    :: Ptr () -> Ptr () -> GL.GLenum -> GL.GLsizei -> GL.GLsizei -> IO GL.GLboolean

foreign import ccall "glGetString" ffiglGetString
    :: GL.GLenum -> IO (Ptr GL.GLubyte)

data BufferSource = forall a. Storable a => BufferSource !(Vector a) !GL.GLenum

mkBuffer :: BufferSource -> IO GLW.Buffer
mkBuffer (BufferSource vec usage) = do
    let n = Vector.length vec
        size = fromIntegral $ n * Foreign.sizeOf (Vector.head vec)
    buffer <- GLW.createObject (Proxy @ GLW.Buffer)
    Vector.unsafeWith vec $ \ptr -> GLW.glNamedBufferData buffer size (Foreign.castPtr ptr) usage
    return buffer

spec :: Spec
spec = describe "addMesh" $
    runIO $ do
        ctx <- ffiOSMesaCreateContextExt GL.GL_RGBA 16 0 0 nullPtr
        if ctx == nullPtr
            then putStrLn "failure"
            else putStrLn "success"

        buffer <- mallocForeignPtrBytes (fromIntegral $ width * height * 4)
        withForeignPtr (buffer :: ForeignPtr GL.GLubyte) $ \p -> do
            r <- ffiOSMesaMakeCurrent ctx (castPtr p) GL.GL_UNSIGNED_BYTE width height
            if r == GL.GL_FALSE
                then putStrLn "OSMesaMakeCurrent failure"
                else putStrLn "OSMesaMakeCurrent success"
            putStrLn "hello"

            Foreign.alloca $ \q -> do
                GL.glGetIntegerv GL.GL_MAJOR_VERSION q
                x <- Foreign.peek q
                putStrLn $ "Major Version: " ++ show x

            putStrLn =<< peekCAString . castPtr =<< ffiglGetString GL.GL_VERSION
            putStrLn =<< peekCAString . castPtr =<< GL.glGetString GL.GL_VERSION

            b <- mkBuffer (BufferSource vec GL.GL_STREAM_READ)
            print b

    where
    width = 640
    height = 480
    vec = Vector.replicate 100 (0 :: GL.GLubyte)
