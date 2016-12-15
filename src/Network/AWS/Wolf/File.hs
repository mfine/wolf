{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Files, directories, and encoding / decoding file functions.
--
module Network.AWS.Wolf.File
  ( fromFile
  , toFile
  , withCurrentWorkDirectory
  ) where

import Control.Monad.Trans.Control
import Data.Aeson                  as A
import Data.ByteString             as BS hiding (filter, notElem)
import Data.ByteString.Lazy        as LBS hiding (filter, notElem)
import Data.Time
import Data.Yaml                   as Y
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import System.Directory
import System.IO

-- | Read file and decode it depending on encoding type.
--
fromFile :: (MonadIO m, FromJSON a) => EncodeType -> FilePath -> m a
fromFile et path =
  liftIO $ withFile path ReadMode $ \h -> do
    body <- BS.hGetContents h
    case et of
      EncodeAeson ->
        eitherThrowIO $ eitherDecode $ fromStrict body
      EncodeYaml ->
        eitherThrowIO $ decodeEither body

-- | Encode it and write to file depending on encoding type.
--
toFile :: (MonadIO m, ToJSON a) => EncodeType -> FilePath -> a -> m ()
toFile et path item =
  liftIO $ withFile path WriteMode $ \h ->
    case et of
      EncodeAeson ->
        BS.hPut h $ toStrict $ A.encode item
      EncodeYaml ->
        BS.hPut h $ Y.encode item

-- | Get a temporary timestamped work directory.
--
getWorkDirectory :: MonadIO m => Text -> m FilePath
getWorkDirectory uid =
  liftIO $ do
    td   <- getTemporaryDirectory
    time <- getCurrentTime
    return $ td </> formatTime defaultTimeLocale "%FT%T%z" time </> textToString uid

-- | Copy directory contents recursively.
--
copyDirectoryRecursive :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectoryRecursive fd td =
  liftIO $ do
    createDirectoryIfMissing False td
    cs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fd
    forM_ cs $ \c -> do
      let fc = fd </> c
          tc = td </> c
      bool (copyDirectoryRecursive fc tc) (copyFile fc tc) <$> doesDirectoryExist fc

-- | Setup a temporary work directory.
--
withWorkDirectory :: (MonadBaseControl IO m, MonadIO m) => Text -> (FilePath -> m a) -> m a
withWorkDirectory uid =
  bracket (getWorkDirectory uid) (liftIO . removeDirectoryRecursive)

-- | Change to directory and then return to current directory.
--
withCurrentDirectory :: (MonadBaseControl IO m, MonadIO m) => FilePath -> (FilePath -> m a) -> m a
withCurrentDirectory wd action =
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \cd -> do
    liftIO $ setCurrentDirectory wd
    action cd

-- | Setup a temporary work directory and copy current directory files to it.
--
withCurrentWorkDirectory :: (MonadBaseControl IO m, MonadIO m) => Text -> (FilePath -> m a) -> m a
withCurrentWorkDirectory uid action =
  withWorkDirectory uid $ \wd ->
    withCurrentDirectory wd $ \cd -> do
      copyDirectoryRecursive cd wd
      action wd
