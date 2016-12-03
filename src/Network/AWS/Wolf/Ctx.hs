{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Wolf.Ctx
  ( runCtx
  , preCtx
  , runConfCtx
  , preConfCtx
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Trace
import Network.AWS.Wolf.Types

-- | Run monad transformer, picking up logger from context.
--
runTransT :: HasCtx c => c -> TransT c m a -> m a
runTransT c m =
  runReaderT (runLoggingT (unTransT m) (c ^. cTrace)) c

-- | Run base context.
--
runCtx :: MonadIO m => TransT Ctx m a -> m a
runCtx action = do
  t <- liftIO $ newStderrTrace LevelInfo
  runTransT (Ctx mempty t) action

-- | Update base context's preamble.
--
preCtx :: MonadCtx c m => Pairs -> TransT Ctx m a -> m a
preCtx preamble action = do
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run configuration context.
--
runConfCtx :: MonadCtx c m => Conf -> TransT ConfCtx m a -> m a
runConfCtx conf action = do
  let preamble =
        [ "domain" .= (conf ^. cDomain)
        , "bucket" .= (conf ^. cBucket)
        , "prefix" .= (conf ^. cPrefix)
        ]
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT (ConfCtx c conf) action

-- | Update configuration context's preamble.
--
preConfCtx :: MonadConf c m => Pairs -> TransT ConfCtx m a -> m a
preConfCtx preamble action = do
  c <- view confCtx <&> cPreamble <>~ preamble
  runTransT c action
