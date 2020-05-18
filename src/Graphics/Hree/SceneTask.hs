{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Hree.SceneTask
    ( SceneTask(..)
    , SceneTaskId
    , SceneTaskBoard(..)
    , AnimationTaskOption(..)
    , addSceneTask
    , newSceneTaskBoard
    , runSceneTask
    , removeSceneTask
    , modifySceneTask
    , runSceneTasksOnBoard
    ) where

import Chronos (Time, TimeInterval(..), Timespan(..), timeIntervalToTimespan)
import Control.Monad (when)
import Data.Atomics.Counter (AtomicCounter)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import Graphics.Hree.Animation (AnimationClip(..), applyAnimationClip)
import Graphics.Hree.Scene (removeNode)
import Graphics.Hree.Types (NodeId, Scene)

data SceneTask =
    Nop |
    AnimationTask !Time !AnimationClip !AnimationTaskOption |
    RemoveNodeTask !Time !NodeId
    deriving (Show)

data AnimationTaskOption = AnimationTaskOption
    { animationTaskOptionLoop        :: !Bool
    , animationTaskOptionRemoveAtEnd :: !Bool
    } deriving (Show, Eq)

type HashTable k v = HT.BasicHashTable k v

newtype SceneTaskId = SceneTaskId
    { unSceneTaskId :: Int
    } deriving (Show, Eq, Hashable)

data SceneTaskBoard = SceneTaskBoard
    { sceneTaskBoardScene   :: !Scene
    , sceneTaskBoardCounter :: !AtomicCounter
    , sceneTaskBoardItems   :: !(HashTable SceneTaskId SceneTask)
    }

newSceneTaskBoard :: Scene -> IO SceneTaskBoard
newSceneTaskBoard scene = do
    counter <- Counter.newCounter 1
    items <- HT.new
    return $ SceneTaskBoard scene counter items

addSceneTask :: SceneTaskBoard -> SceneTask -> IO SceneTaskId
addSceneTask (SceneTaskBoard _ counter items) task = do
    taskId <- SceneTaskId <$> Counter.incrCounter 1 counter
    HT.insert items taskId task
    return taskId

removeSceneTask :: SceneTaskBoard -> SceneTaskId -> IO ()
removeSceneTask (SceneTaskBoard _ _ items) taskId =
    HT.delete items taskId

modifySceneTask :: SceneTaskBoard -> (SceneTask -> SceneTask) -> SceneTaskId -> IO ()
modifySceneTask (SceneTaskBoard _ _ items) f taskId =
    HT.mutate items taskId g
    where
    g a = (fmap f a, ())

runSceneTask :: Scene -> SceneTask -> Time -> IO Bool
runSceneTask _ Nop _ = return False
runSceneTask scene (AnimationTask start animation options) t = do
    let loop = animationTaskOptionLoop options
        removeAtEnd = animationTaskOptionRemoveAtEnd options
        duration = animationClipDuration animation
        dt = timeIntervalToTimespan $ TimeInterval start t
        remove = removeAtEnd && (duration > dt)
    when (t > start) $ do
        let at = if loop && dt > duration
                    then Timespan $ getTimespan dt `mod` getTimespan duration
                    else dt
        applyAnimationClip scene animation at
    return remove
runSceneTask scene (RemoveNodeTask at nodeId) t =
    if t >= at
        then do
            removeNode scene nodeId
            return True
        else return False

runSceneTasksOnBoard :: SceneTaskBoard -> Time -> IO ()
runSceneTasksOnBoard (SceneTaskBoard scene _ items) t = do
    removeTaskIds <- HT.foldM run [] items
    mapM_ (HT.delete items) removeTaskIds
    where
    run removeTaskIds (taskId, task) = do
        remove <- runSceneTask scene task t
        return $ if remove
                then taskId : removeTaskIds
                else removeTaskIds
