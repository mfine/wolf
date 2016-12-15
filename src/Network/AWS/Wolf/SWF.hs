{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Calls.
--
module Network.AWS.Wolf.SWF
  ( pollActivity
  , pollDecision
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.List        hiding (concatMap, map)
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

runAWS :: (MonadIO m, MonadCatch m) => AWST m b -> m b
runAWS action = do
  e <- newEnv Oregon $ FromEnv mempty mempty mempty
  runAWST e action

pollActivity :: MonadConf c m => Text -> m (Maybe Text, Maybe Text, Maybe Text)
pollActivity queue = do
  conf <- view cConf
  runAWS $ do
    r <- send (pollForActivityTask (conf ^. cDomain) (taskList queue))
    return
      ( r ^. pfatrsTaskToken
      , view weWorkflowId <$> r ^. pfatrsWorkflowExecution
      , r ^. pfatrsInput
      )

pollDecision :: MonadConf c m => Text -> m (Maybe Text, [HistoryEvent])
pollDecision queue = do
  conf <- view cConf
  runAWS $ do
    rs <- paginate (pollForDecisionTask (conf ^. cDomain) (taskList queue)) $$ consume
    return
      ( join $ listToMaybe $ map (view pfdtrsTaskToken) rs
      , reverse $ concatMap (view pfdtrsEvents) rs
      )
