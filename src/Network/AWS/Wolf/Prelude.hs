{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Wolf.Prelude
  ( module Exports
  , eitherThrowIO
  , runConcurrent
  , MonadMain
  ) where

import BasicPrelude                    as Exports
import Control.Concurrent.Async.Lifted
import Control.Lens                    as Exports hiding (uncons, (.=), (<.>))
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

-- | Throw userError on either error.
--
eitherThrowIO :: MonadIO m => Either String a -> m a
eitherThrowIO = either (liftIO . throwIO . userError) return

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . sequenceA . map Concurrently

type MonadMain m =
  ( MonadBaseControl IO m
  , MonadIO m
  , MonadResource m
  , MonadCatch m
  )
