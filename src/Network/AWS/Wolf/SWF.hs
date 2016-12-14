{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Calls.
--
module Network.AWS.Wolf.SWF
  ( pollActivity
  , pollDecision
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.AWS
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

run :: (MonadIO m, MonadCatch m) => AWST m b -> m b
run action = do
  e <- newEnv Oregon $ FromEnv mempty mempty mempty
  runAWST e action

pollActivity :: MonadConf c m => Text -> m (Maybe Text, Maybe Text)
pollActivity queue = do
  conf <- view cConf
  run $ do
    r <- send $ pollForActivityTask (conf ^. cDomain) (taskList queue)
    return $
      ( r ^. pfatrsTaskToken
      , r ^. pfatrsInput
      )

pollDecision :: MonadConf c m => Text -> m (Maybe Text, [HistoryEvent])
pollDecision queue = do
  conf <- view cConf
  run $ do
    send $ pollForDecisionTask (conf ^. cDomain) (taskList queue)
    undefined

