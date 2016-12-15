{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Actor logic.
--
module Network.AWS.Wolf.Act
  ( act
  ) where

import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.S3
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Types

run :: MonadConf c m => Text -> Text -> Bool -> m ()
run queue _command _gzip = do
  (token, uid, input) <- pollActivity queue
  maybe_ token $ \token' ->
    maybe_ uid $ \uid' -> do
      artifacts <- listArtifacts uid'
      undefined

act :: MonadMain m => FilePath -> Text -> Text -> Bool -> m ()
act cf queue command gzip =
  runCtx $ do
    conf <- fromFile EncodeYaml cf
    runConfCtx conf $
      run queue command gzip
