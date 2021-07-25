{-# LANGUAGE ScopedTypeVariables #-}
module Hree.GL.UniformBlock
    ( UniformBlockBinder(..)
    , bindUniformBuffer
    , newUniformBlockBinder
    , modifyUniformBlock
    , updateAndBindUniformBuffer
    , updateUniformBlock
    , updateUniformBlock_
    , updateUniformBlockWith
    ) where

import Control.Monad (when)
import qualified Foreign (ForeignPtr, Ptr, Storable(..), castForeignPtr,
                          castPtr, mallocForeignPtr, withForeignPtr)
import qualified GLW (Buffer, glBindBufferBase)
import qualified Graphics.GL as GL
import Hree.GL (updateBuffer)
import Hree.GL.Block (Block(..), Std140(..))
import Hree.GL.Types (BufferSource(..), UniformBufferBindingIndex(..))

data UniformBlockBinder a = UniformBlockBinder
    { uniformBlockBinderBuffer :: !GLW.Buffer
    , uniformBlockBinderPtr    :: !(Foreign.ForeignPtr (Std140 a))
    } deriving (Show, Eq)

newUniformBlockBinder :: (Block a) => GLW.Buffer -> a -> IO (UniformBlockBinder a)
newUniformBlockBinder buffer a = do
    ptr <- Foreign.mallocForeignPtr
    Foreign.withForeignPtr ptr (`Foreign.poke` Std140 a)
    updateBuffer buffer (BufferSourcePtr (Foreign.castForeignPtr ptr) (Foreign.sizeOf (Std140 a)) GL.GL_DYNAMIC_DRAW)
    return $ UniformBlockBinder buffer ptr

updateUniformBlock :: (Eq a, Block a) => UniformBlockBinder a -> a -> IO ()
updateUniformBlock ubb @ (UniformBlockBinder _ ptr) a = do
    prev <- unStd140 <$> Foreign.withForeignPtr ptr Foreign.peek
    when (a /= prev) $ updateUniformBlock_ ubb a

updateUniformBlock_ :: forall a. (Block a) => UniformBlockBinder a -> a -> IO ()
updateUniformBlock_ (UniformBlockBinder buffer ptr) a =
    Foreign.withForeignPtr ptr $ \p -> do
        Foreign.poke p (Std140 a)
        writeBuffer (Foreign.castPtr p :: Foreign.Ptr a) buffer

modifyUniformBlock :: (Eq a, Block a) => (a -> a) -> UniformBlockBinder a -> IO ()
modifyUniformBlock f ubb @ (UniformBlockBinder _ ptr) = do
    prev <- unStd140 <$> Foreign.withForeignPtr ptr Foreign.peek
    let modified = f prev
    when (modified /= prev) $ updateUniformBlock_ ubb modified

updateUniformBlockWith :: forall a. (Block a) => UniformBlockBinder a -> (Foreign.Ptr () -> IO ()) -> IO ()
updateUniformBlockWith (UniformBlockBinder buffer ptr) f =
    Foreign.withForeignPtr ptr $ \p -> do
        f (Foreign.castPtr p)
        writeBuffer (Foreign.castPtr p :: Foreign.Ptr a) buffer

bindUniformBuffer :: UniformBlockBinder a -> GL.GLuint -> IO ()
bindUniformBuffer (UniformBlockBinder buffer _) bindingIndex =
    GLW.glBindBufferBase GL.GL_UNIFORM_BUFFER bindingIndex buffer

updateAndBindUniformBuffer :: (Eq a, Block a) => UniformBlockBinder a -> a -> UniformBufferBindingIndex -> IO ()
updateAndBindUniformBuffer ubb a (UniformBufferBindingIndex bindingIndex) = do
    updateUniformBlock ubb a
    bindUniformBuffer ubb bindingIndex
