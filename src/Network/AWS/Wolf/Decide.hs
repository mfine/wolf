{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Wolf.Decide
  ( decide
  , decideMain
  ) where

import Data.Aeson
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types
import           Network.AWS.SWF

findEvent :: [HistoryEvent] -> Integer -> Maybe HistoryEvent
findEvent hes ei =
  flip find hes $
    (== ei) . (view heEventId)

nextEvent :: [HistoryEvent] -> [EventType] -> Maybe HistoryEvent
nextEvent hes ets =
  flip find hes $
    (`elem` ets) . (view heEventType)

schedule :: MonadCtx c m => Maybe Text -> Maybe Task -> m [Decision]
schedule = undefined

start :: MonadCtx c m => Plan -> HistoryEvent -> m [Decision]
start plan he = do
  input <- maybeThrowIO "No Start Information" $
    view weseaInput <$> he ^. heWorkflowExecutionStartedEventAttributes
  schedule input $ listToMaybe (plan ^. pTasks)

completed he = undefined

failed he = undefined

select :: MonadCtx c m => Plan -> [HistoryEvent] -> m [Decision]
select plan hes = do
  he <- maybeThrowIO "No Next Event" $ nextEvent hes
          [ WorkflowExecutionStarted
          , ActivityTaskCompleted
          , ActivityTaskFailed
          ]
  case he ^. heEventType of
    WorkflowExecutionStarted -> start plan he
    ActivityTaskCompleted    -> completed he
    ActivityTaskFailed       -> failed he
    _et                      -> liftIO $ throwIO $ userError "Unknown Select Event"

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadConf c m => Plan -> m ()
decide plan =
  preConfCtx [ "label" .= LabelDecide ] $
    runAmazonCtx $
      runAmazonWorkCtx (plan ^. pStart ^. ptQueue) $ do
        traceInfo "poll" mempty
        (token, hes) <- pollDecision
        maybe_ token $ \token' -> do
          traceInfo "start" mempty
          decisions <- select plan hes
          completeDecision token' decisions
          traceInfo "finish" mempty

-- | Run decider from main with config file.
--
decideMain :: MonadMain m => FilePath -> FilePath -> m ()
decideMain cf pf =
  runCtx $ do
    conf <- readYaml cf
    runConfCtx conf $ do
      plans <- readYaml pf
      runConcurrent $
        (forever . decide) <$> plans
