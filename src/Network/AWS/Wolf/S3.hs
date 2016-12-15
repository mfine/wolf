{-# LANGUAGE NoImplicitPrelude #-}

-- | S3 Calls.
--
module Network.AWS.Wolf.S3
  ( listArtifacts
  , getArtifact
  , putArtifact
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.AWS
import Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.Combinators hiding (sourceFile, sinkFile)
import Data.Conduit.List hiding (map, mapMaybe)
import Network.AWS.Wolf.Prelude hiding (stripPrefix)
import Network.AWS.Wolf.Types
import Network.AWS.S3 hiding (cBucket)
import Network.AWS.Data.Text
import Data.Text
import           Network.AWS.Data.Body

runAWS :: (MonadIO m, MonadCatch m) => AWST m b -> m b
runAWS action = do
  e <- newEnv Oregon $ FromEnv mempty mempty mempty
  runAWST e action

listArtifacts :: MonadConf c m => Text -> m [Text]
listArtifacts uid = do
  conf <- view cConf
  let prefix = (conf ^. cPrefix) <\> uid
  runAWS $ do
    lors <- paginate (set loPrefix (return prefix) $ listObjects (BucketName (conf ^. cBucket))) $$ consume
    return $ mapMaybe (stripPrefix prefix) $ toText . view oKey <$> (join $ view lorsContents <$> lors)

getArtifact :: MonadConf c m => Text -> Text -> FilePath -> m ()
getArtifact uid key file = do
  conf <- view cConf
  let prefix = (conf ^. cPrefix) <\> uid
  runAWS $ do
    gors <- send $ getObject (BucketName (conf ^. cBucket)) (ObjectKey (prefix <\> key))
    sinkBody (gors ^. gorsBody) (sinkFile file)

putArtifact :: MonadConf c m => Text -> Text -> FilePath -> m ()
putArtifact uid key file = do
  conf <- view cConf
  let prefix = (conf ^. cPrefix) <\> uid
  (sha, len) <- sourceFile file $$ getZipSink $ (,) <$> ZipSink sinkSHA256 <*> ZipSink lengthE
  runAWS $ do
    void $ send $ putObject (BucketName (conf ^. cBucket)) (ObjectKey (prefix <\> key)) $
      Hashed $ HashedStream sha len $ sourceFile file
