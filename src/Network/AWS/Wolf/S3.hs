{-# LANGUAGE NoImplicitPrelude #-}

-- | S3 Calls.
--
module Network.AWS.Wolf.S3
  ( listArtifacts
  , getArtifact
  , putArtifact
  ) where


import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Combinators hiding (sinkFile, sourceFile)
import Data.Conduit.List        hiding (map, mapMaybe)
import Data.Text
import Network.AWS.Data.Body
import Network.AWS.Data.Text
import Network.AWS.S3           hiding (cBucket)
import Network.AWS.Wolf.Constant
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

runS3 :: MonadConf c m => Text -> (Text -> Text -> AWST m b) -> m b
runS3 uid action = do
  env  <- newEnv Oregon $ FromEnv awsAccessKey awsSecretKey mempty
  conf <- view cConf
  runAWST env $ action (conf ^. cBucket) $ (conf ^. cPrefix) <\> uid

listArtifacts :: MonadConf c m => Text -> m [Text]
listArtifacts uid =
  runS3 uid $ \b p -> do
    lors <- paginate (set loPrefix (return p) $ listObjects (BucketName b)) $$ consume
    return $ mapMaybe (stripPrefix' p) $ toText . view oKey <$> join (view lorsContents <$> lors)

getArtifact :: MonadConf c m => Text -> Text -> FilePath -> m ()
getArtifact uid key file =
  runS3 uid $ \b p -> do
    gors <- send $ getObject (BucketName b) (ObjectKey (p <\> key))
    sinkBody (gors ^. gorsBody) (sinkFile file)

putArtifact :: MonadConf c m => Text -> FilePath -> Text -> m ()
putArtifact uid file key = do
  (sha, len) <- sourceFile file $$ getZipSink $ (,) <$> ZipSink sinkSHA256 <*> ZipSink lengthE
  runS3 uid $ \b p ->
    void $ send $ putObject (BucketName b) (ObjectKey (p <\> key)) $
      Hashed $ HashedStream sha len $ sourceFile file

