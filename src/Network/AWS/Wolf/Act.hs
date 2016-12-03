{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Actor logic.
--
module Network.AWS.Wolf.Act
  ( act
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

run :: MonadConf c m => Text -> Text -> m ()
run queue command = do
  traceInfo "act" [ "queue" .= queue, "command" .= command ]
  (token, uid, input) <- pollActivity queue
  maybe_ token $ \token' ->
    maybe_ uid $ \uid' ->
      preConfCtx [ "uid" .= uid' ] $
        withCurrentWorkDirectory uid' $ \wd -> do
          traceInfo "start" [ "input" .= input, "dir" .= wd ]
          dd  <- dataDirectory wd
          sd  <- storeDirectory wd
          isd <- inputDirectory sd
          osd <- outputDirectory sd
          writeText (dd </> "input.json") input
          ks  <- listArtifacts uid'
          traceInfo "list-artifacts" [ "keys" .= ks ]
          forM_ ks $ \k -> do
            traceInfo "get-artifact" [ "key" .= k ]
            getArtifact uid' k $ isd </> textToString k
          traceInfo "start-command" mempty
          liftIO $ callCommand $ textToString command
          traceInfo "finish-command" mempty
          fs <- findRegularFiles osd
          forM_ fs $ \f -> do
            let k = stripPrefix' (textFromString osd) (textFromString f)
            traceInfo "put-artifact" [ "key" .= k ]
            traverse (putArtifact uid' f) k
          output <- readText (dd </> "output.json")
          completeActivity token' output
          traceInfo "finish" [ "output" .= output ]

act :: MonadMain m => FilePath -> Text -> Text -> m ()
act cf queue command =
  runCtx $ do
    conf <- fromYaml cf
    runConfCtx conf $
      run queue command
