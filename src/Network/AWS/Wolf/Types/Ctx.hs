{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Network.AWS.Wolf.Types.Ctx where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Network.AWS.Wolf.Lens
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types.Alias
import Network.AWS.Wolf.Types.Product

-- | Ctx
--
-- Base context, supports tracing.
--
data Ctx = Ctx
  { _cPreamble :: Pairs
    -- ^ Object to encode on every trace line.
  , _cTrace    :: Trace
    -- ^ Configurable tracing function.
  }

$(makeClassy ''Ctx)

type MonadCtx c m =
  ( MonadBaseControl IO m
  , MonadIO m
  , MonadReader c m
  , MonadLogger m
  , MonadCatch m
  , MonadResource m
  , HasCtx c
  )

-- | ConfCtx
--
-- Configuration context.
--
data ConfCtx = ConfCtx
  { _cCtx  :: Ctx
    -- ^ Parent context.
  , _cConf :: Conf
    -- ^ Configuration parameters.
  }

$(makeClassyConstraints ''ConfCtx [''HasCtx])

instance HasCtx ConfCtx where
  ctx = cCtx

type MonadConf c m =
  ( MonadCtx c m
  , HasConfCtx c
  )
