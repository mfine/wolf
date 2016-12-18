{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Calls.
--
module Network.AWS.Wolf.SWF
  ( pollActivity
  , pollDecision
  , completeActivity
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.List        hiding (concatMap, map)
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

pollActivity :: MonadAmazon c m => Text -> m (Maybe Text, Maybe Text, Maybe Text)
pollActivity queue = do
  d      <- view cDomain <$> view ccConf
  pfatrs <- send (pollForActivityTask d (taskList queue))
  return
    ( pfatrs ^. pfatrsTaskToken
    , view weWorkflowId <$> pfatrs ^. pfatrsWorkflowExecution
    , pfatrs ^. pfatrsInput
    )

pollDecision :: MonadAmazon c m => Text -> m (Maybe Text, [HistoryEvent])
pollDecision queue = do
  d      <- view cDomain <$> view ccConf
  pfdtrs <- paginate (pollForDecisionTask d (taskList queue)) $$ consume
  return
    ( join $ listToMaybe $ map (view pfdtrsTaskToken) pfdtrs
    , reverse $ concatMap (view pfdtrsEvents) pfdtrs
    )

completeActivity :: MonadAmazon c m => Text -> Maybe Text -> m ()
completeActivity token output =
  void $ send $ set ratcResult output $ respondActivityTaskCompleted token

