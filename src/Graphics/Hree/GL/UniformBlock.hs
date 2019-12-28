module Graphics.Hree.GL.UniformBlock
    ( UniformBlockBinder
    , bindUniformBuffer
    , newUniformBlockBinder
    , updateAndBindUniformBuffer
    , updateUniformBlock
    , updateUniformBlock_
    , updateUniformBlockWith
    ) where

import Control.Monad (when)
import Data.Proxy (asProxyTypeOf)
import qualified Foreign (ForeignPtr, Ptr, Storable(..), castPtr,
                          mallocForeignPtr, withForeignPtr)
import qualified GLW (Buffer, glBindBufferBase, glNamedBufferData)
import qualified Graphics.GL as GL
import Graphics.Hree.GL (updateBuffer)
import Graphics.Hree.GL.Block (Block(..), Std140(..))
import Graphics.Hree.GL.Types (BufferBindingIndex(..), BufferSource(..))

data UniformBlockBinder a = UniformBlockBinder
    { uniformBlockBinderBuffer :: !GLW.Buffer
    , uniformBlockBinderPtr    :: !(Foreign.ForeignPtr (Std140 a))
    } deriving (Show, Eq)

newUniformBlockBinder :: (Block a) => GLW.Buffer -> a -> IO (UniformBlockBinder a)
newUniformBlockBinder buffer a = do
    ptr <- Foreign.mallocForeignPtr
    Foreign.withForeignPtr ptr (`Foreign.poke` Std140 a)
    Foreign.withForeignPtr ptr $ \p -> updateBuffer buffer (BufferSourcePtr p GL.GL_DYNAMIC_DRAW)
    return $ UniformBlockBinder buffer ptr

updateUniformBlock :: (Eq a, Block a) => UniformBlockBinder a -> a -> IO ()
updateUniformBlock ubb @ (UniformBlockBinder _ ptr) a = do
    prev <- unStd140 <$> Foreign.withForeignPtr ptr Foreign.peek
    when (a /= prev) $ updateUniformBlock_ ubb a

updateUniformBlock_ :: (Block a) => UniformBlockBinder a -> a -> IO ()
updateUniformBlock_ (UniformBlockBinder buffer ptr) a =
    Foreign.withForeignPtr ptr $ \p -> do
        Foreign.poke p (Std140 a)
        let size = fromIntegral $ Foreign.sizeOf (Std140 a)
        GLW.glNamedBufferData buffer size (Foreign.castPtr p) GL.GL_DYNAMIC_DRAW

updateUniformBlockWith :: (Block a) => UniformBlockBinder a -> (Foreign.Ptr () -> IO ()) -> IO ()
updateUniformBlockWith (UniformBlockBinder buffer ptr) f =
    Foreign.withForeignPtr ptr $ \p -> do
        f (Foreign.castPtr p)
        let size = fromIntegral $ Foreign.sizeOf (asProxyTypeOf undefined ptr)
        GLW.glNamedBufferData buffer size (Foreign.castPtr p) GL.GL_DYNAMIC_DRAW

bindUniformBuffer :: UniformBlockBinder a -> GL.GLuint -> IO ()
bindUniformBuffer (UniformBlockBinder buffer _) bindingIndex =
    GLW.glBindBufferBase GL.GL_UNIFORM_BUFFER bindingIndex buffer

updateAndBindUniformBuffer :: (Eq a, Block a) => UniformBlockBinder a -> a -> BufferBindingIndex -> IO ()
updateAndBindUniformBuffer ubb a (BufferBindingIndex bindingIndex) = do
    updateUniformBlock ubb a
    bindUniformBuffer ubb bindingIndex
