{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Actor logic.
--
module Network.AWS.Wolf.Act
  ( act
  , actMain
  ) where

import Data.Aeson
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.File
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.S3
import Network.AWS.Wolf.SWF
import Network.AWS.Wolf.Trace
import Network.AWS.Wolf.Types
import System.Process

download :: MonadAmazonStore c m => FilePath -> m ()
download dir = do
  ks <- listArtifacts
  forM_ ks $ \k -> do
    traceInfo "get-artifact" [ "key" .= k ]
    getArtifact (dir </> textToString k) k

upload :: MonadAmazonStore c m => FilePath -> m ()
upload dir = do
  fs <- findRegularFiles dir
  forM_ fs $ \f -> do
    let k = stripPrefix' (textFromString dir) (textFromString f)
    traceInfo "put-artifact" [ "key" .= k ]
    traverse (putArtifact f) k

run :: MonadCtx c m => String -> m ()
run command =
  preCtx [ "command" .= command ] $ do
    traceInfo "start" mempty
    liftIO $ callCommand command
    traceInfo "finish" mempty

act :: MonadConf c m => Text -> String -> m ()
act queue command =
  runAmazonCtx $
    runAmazonWorkCtx queue $ do
      traceInfo "act" mempty
      (token, uid, input) <- pollActivity
      maybe_ token $ \token' ->
        maybe_ uid $ \uid' ->
          withCurrentWorkDirectory uid' $ \wd ->
            runAmazonStoreCtx uid' $ do
              traceInfo "start" [ "input" .= input, "dir" .= wd ]
              dd  <- dataDirectory wd
              sd  <- storeDirectory wd
              isd <- inputDirectory sd
              osd <- outputDirectory sd
              writeText (dd </> "input.json") input
              download isd
              run command
              upload osd
              output <- readText (dd </> "output.json")
              completeActivity token' output
              traceInfo "finish" [ "output" .= output ]

actMain :: MonadMain m => FilePath -> Text -> String -> m ()
actMain cf queue command =
  runCtx $ do
    conf <- fromYaml cf
    runConfCtx conf $
      act queue command
