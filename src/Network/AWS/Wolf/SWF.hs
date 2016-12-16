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

runSWF :: MonadConf c m => (Text -> AWST m b) -> m b
runSWF action = do
  env <- newEnv Oregon $ FromEnv mempty mempty mempty
  conf <- view cConf
  runAWST env $ action (conf ^. cDomain)

pollActivity :: MonadConf c m => Text -> m (Maybe Text, Maybe Text, Maybe Text)
pollActivity queue =
  runSWF $ \d -> do
    pfatrs <- send (pollForActivityTask d (taskList queue))
    return
      ( pfatrs ^. pfatrsTaskToken
      , view weWorkflowId <$> pfatrs ^. pfatrsWorkflowExecution
      , pfatrs ^. pfatrsInput
      )

pollDecision :: MonadConf c m => Text -> m (Maybe Text, [HistoryEvent])
pollDecision queue =
  runSWF $ \d -> do
    pfdtrs <- paginate (pollForDecisionTask d (taskList queue)) $$ consume
    return
      ( join $ listToMaybe $ map (view pfdtrsTaskToken) pfdtrs
      , reverse $ concatMap (view pfdtrsEvents) pfdtrs
      )

completeActivity :: MonadConf c m => Text -> Maybe Text -> m ()
completeActivity token output =
  runSWF $ const $
    void $ send $ set ratcResult output $ respondActivityTaskCompleted token
