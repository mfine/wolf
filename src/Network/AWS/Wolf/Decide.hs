{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Decider logic.
--
module Network.AWS.Wolf.Decide
  ( decide
  , decideMain
  ) where

import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

decide :: MonadConf c m => Plan -> m ()
decide plan =
  runAmazonCtx $
    runAmazonWorkCtx (plan ^. pStart ^. ptQueue) $ do
      (token, _events) <- pollDecision
      maybe_ token $ const undefined

decideMain :: MonadMain m => FilePath -> FilePath -> m ()
decideMain cf pf =
  runCtx $ do
    conf <- fromYaml cf
    runConfCtx conf $ do
      plans <- fromYaml pf
      runConcurrent $
        decide <$> plans
