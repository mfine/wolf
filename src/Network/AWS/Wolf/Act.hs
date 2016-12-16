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
import System.Process

run :: MonadConf c m => Text -> Text -> m ()
run queue command = do
  (token, uid, input) <- pollActivity queue
  maybe_ token $ \token' ->
    maybe_ uid $ \uid' ->
      withCurrentWorkDirectory uid' $ \wd -> do
        dd <- dataDirectory wd
        writeText (dd </> "input.json") input
        ks  <- listArtifacts uid'
        sd  <- storeDirectory wd
        isd <- inputDirectory sd
        forM_ ks $ \k ->
          getArtifact uid' k $ isd </> textToString k
        liftIO $ callCommand $ textToString command
        osd <- outputDirectory sd
        fs  <- findRegularFiles osd
        forM_ fs $ \f ->
          traverse (putArtifact uid' f) $ textFromString <$> stripPrefix osd f
        output <- readText (dd </> "output.json")
        completeActivity token' output

act :: MonadMain m => FilePath -> Text -> Text -> m ()
act cf queue command =
  runCtx $ do
    conf <- fromYaml cf
    runConfCtx conf $
      run queue command
