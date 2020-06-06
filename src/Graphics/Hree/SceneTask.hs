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
    , terminateSceneTask
    ) where

import Chronos (Time(..), TimeInterval(..), Timespan(..),
                timeIntervalToTimespan)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Atomics.Counter (AtomicCounter)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
    , animationTaskOptionEndAction   :: !(Maybe EndAction)
    } deriving (Show, Eq)

data EndAction =
    SeekHead |
    ClampEnd |
    RemoveNode !NodeId
    deriving (Show, Eq)

type HashTable k v = HT.BasicHashTable k v

newtype SceneTaskId = SceneTaskId
    { unSceneTaskId :: Int
    } deriving (Show, Eq, Hashable)

data SceneTaskBoard = SceneTaskBoard
    { sceneTaskBoardScene   :: !Scene
    , sceneTaskBoardCounter :: !AtomicCounter
    , sceneTaskBoardItems   :: !(HashTable SceneTaskId SceneTask)
    , sceneTaskBoardLastRun :: !(IORef (Maybe Time))
    }

newSceneTaskBoard :: Scene -> IO SceneTaskBoard
newSceneTaskBoard scene = do
    counter <- Counter.newCounter 1
    items <- HT.new
    lastRunRef <- newIORef Nothing
    return $ SceneTaskBoard scene counter items lastRunRef

addSceneTask :: SceneTaskBoard -> SceneTask -> IO SceneTaskId
addSceneTask (SceneTaskBoard _ counter items _) task = do
    taskId <- SceneTaskId <$> Counter.incrCounter 1 counter
    HT.insert items taskId task
    return taskId

removeSceneTask :: SceneTaskBoard -> SceneTaskId -> IO ()
removeSceneTask (SceneTaskBoard _ _ items _) taskId =
    HT.delete items taskId

modifySceneTask :: SceneTaskBoard -> (SceneTask -> SceneTask) -> SceneTaskId -> IO ()
modifySceneTask (SceneTaskBoard _ _ items _) f taskId =
    HT.mutate items taskId g
    where
    g a = (fmap f a, ())

runEndAction :: Scene -> AnimationClip -> Maybe EndAction -> IO ()
runEndAction _ _ Nothing = return ()
runEndAction scene animation (Just SeekHead) = applyAnimationClip scene animation (Timespan 0)
runEndAction _ _ (Just ClampEnd) = return ()
runEndAction scene _ (Just (RemoveNode nodeId)) = removeNode scene nodeId

runSceneTask :: Scene -> SceneTask -> Maybe Time -> Time -> IO Bool
runSceneTask _ Nop _ _ = return False
runSceneTask scene (AnimationTask start animation options) Nothing t = do
    let AnimationTaskOption loop removeAtEnd endAction = options
        duration = animationClipDuration animation
        dt = timeIntervalToTimespan $ TimeInterval start t
        remove = removeAtEnd && (duration >= dt)
    when (t >= start) $
        if loop
            then applyAnimationClip scene animation . Timespan $ getTimespan dt `mod` getTimespan duration
            else do
                applyAnimationClip scene animation dt
                when (dt >= duration) $ runEndAction scene animation endAction
    return remove
runSceneTask scene (AnimationTask start animation options) (Just lastRun) t
    | lastRun == t = return False
    | otherwise = do
        let AnimationTaskOption loop removeAtEnd endAction = options
            duration = animationClipDuration animation
            dt = timeIntervalToTimespan $ TimeInterval start t
            lastDt = timeIntervalToTimespan $ TimeInterval start lastRun
            remove = removeAtEnd && (duration >= dt)
        when (t >= start) $
            if loop
                then applyAnimationClip scene animation . Timespan $ getTimespan dt `mod` getTimespan duration
                else do
                    when (lastDt < duration) $ applyAnimationClip scene animation dt
                    when (lastDt < duration && dt >= duration) $ runEndAction scene animation endAction
        return remove
runSceneTask scene (RemoveNodeTask at nodeId) _ t =
    if t >= at
        then do
            removeNode scene nodeId
            return True
        else return False

runSceneTasksOnBoard :: SceneTaskBoard -> Time -> IO ()
runSceneTasksOnBoard (SceneTaskBoard scene _ items lastRunRef) t = do
    lastRun <- readIORef lastRunRef
    removeTaskIds <- HT.foldM (run lastRun) [] items
    mapM_ (HT.delete items) removeTaskIds
    writeIORef lastRunRef (Just t)
    where
    run lastRun removeTaskIds (taskId, task) = do
        remove <- runSceneTask scene task lastRun t
        return $ if remove
                then taskId : removeTaskIds
                else removeTaskIds

terminateSceneTask :: SceneTaskBoard -> SceneTaskId -> IO ()
terminateSceneTask (SceneTaskBoard scene _ items _) taskId = do
    task <- maybe (throwIO . userError $ "task not found. taskId: " ++ show taskId) return
                =<< HT.lookup items taskId
    remove <- terminateSceneTask_ scene task
    when remove $ HT.delete items taskId

terminateSceneTask_ :: Scene -> SceneTask -> IO Bool
terminateSceneTask_ _ Nop = return False
terminateSceneTask_ scene task @ (AnimationTask start animation _) = do
    let Timespan duration = animationClipDuration animation
        Time st = start
        endAt = Time (st + duration)
    runSceneTask scene task Nothing endAt
terminateSceneTask_ scene task @ (RemoveNodeTask at _) =
    runSceneTask scene task Nothing at
