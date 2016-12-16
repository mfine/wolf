{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Decider logic.
--
module Network.AWS.Wolf.Decide
  ( decide
  ) where

import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

run :: MonadConf c m => Plan -> m ()
run plan = do
  (token, _events) <- pollDecision (plan ^. pStart ^. ptQueue)
  maybe_ token $ const undefined

decide :: MonadMain m => FilePath -> FilePath -> m ()
decide cf pf =
  runCtx $ do
    conf <- fromYaml cf
    runConfCtx conf $ do
      plans <- fromYaml pf
      runConcurrent $
        run <$> plans
