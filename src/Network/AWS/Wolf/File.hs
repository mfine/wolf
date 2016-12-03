{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Files, directories, and encoding / decoding file functions.
--
module Network.AWS.Wolf.File
  ( findRegularFiles
  , dataDirectory
  , storeDirectory
  , inputDirectory
  , outputDirectory
  , writeText
  , readText
  , fromYaml
  , withCurrentWorkDirectory
  ) where


import Data.Aeson               as A
import Data.ByteString          as BS hiding (putStrLn, filter, find, notElem, readFile, writeFile)
import Data.Time
import Data.Yaml                as Y
import Network.AWS.Wolf.Prelude hiding (find)
import System.Directory
import System.FilePath.Find
import System.IO                hiding (readFile, writeFile)

-- | Recursively find all files under a directory.
--
findRegularFiles :: MonadIO m => FilePath -> m [FilePath]
findRegularFiles =
  liftIO . find always (fileType ==? RegularFile)

-- | Determine path to data directory and create it.
--
dataDirectory :: MonadIO m => FilePath -> m FilePath
dataDirectory dir = do
  let dir' = dir </> "data"
  liftIO $ createDirectoryIfMissing True dir'
  return dir'

-- | Determine path to store directory and create it.
--
storeDirectory :: MonadIO m => FilePath -> m FilePath
storeDirectory dir = do
  let dir' = dir </> "store"
  liftIO $ createDirectoryIfMissing True dir'
  return dir'

-- | Determine path to store input directory and create it.
--
inputDirectory :: MonadIO m => FilePath -> m FilePath
inputDirectory dir = do
  let dir' = dir </> "input"
  liftIO $ createDirectoryIfMissing True dir'
  return dir'

-- | Determine path to store output directory and create it.
--
outputDirectory :: MonadIO m => FilePath -> m FilePath
outputDirectory dir = do
  let dir' = dir </> "output"
  liftIO $ createDirectoryIfMissing True dir'
  return dir'

-- | Maybe write text to a file.
--
writeText :: MonadIO m => FilePath -> Maybe Text -> m ()
writeText file contents =
  liftIO $ void $ traverse (writeFile file) contents

-- | Maybe read text from a file.
--
readText :: MonadIO m => FilePath -> m (Maybe Text)
readText file =
 liftIO $ do
   b <- doesFileExist file
   if not b then return mempty else
     return <$> readFile file

-- | Read file and decode it from YAML.
--
fromYaml :: (MonadIO m, FromJSON a) => FilePath -> m a
fromYaml path =
  liftIO $ withFile path ReadMode $ \h -> do
    body <- BS.hGetContents h
    eitherThrowIO $ decodeEither body

-- | Get a temporary timestamped work directory.
--
getWorkDirectory :: MonadIO m => Text -> m FilePath
getWorkDirectory uid =
  liftIO $ do
    td   <- getTemporaryDirectory
    time <- getCurrentTime
    let dir = td </> formatTime defaultTimeLocale "%FT%T%z" time </> textToString uid
    createDirectoryIfMissing True dir
    return dir

-- | Copy directory contents recursively.
--
copyDirectoryRecursive :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectoryRecursive fd td =
  liftIO $ do
    createDirectoryIfMissing True td
    cs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fd
    forM_ cs $ \c -> do
      let fc = fd </> c
          tc = td </> c
      b <- doesDirectoryExist fc
      if b then do
        copyDirectoryRecursive fc tc
      else do
        copyFile fc tc
--      bool (copyDirectoryRecursive fc tc) (copyFile fc tc) <$> doesDirectoryExist fc

-- | Setup a temporary work directory.
--
withWorkDirectory :: MonadBaseControlIO m => Text -> (FilePath -> m a) -> m a
withWorkDirectory uid =
  bracket (getWorkDirectory uid) (liftIO . removeDirectoryRecursive)

-- | Change to directory and then return to current directory.
--
withCurrentDirectory :: MonadBaseControlIO m => FilePath -> (FilePath -> m a) -> m a
withCurrentDirectory wd action = do
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \cd -> do
    liftIO $ setCurrentDirectory wd
    action cd

-- | Setup a temporary work directory and copy current directory files to it.
--
withCurrentWorkDirectory :: MonadBaseControlIO m => Text -> (FilePath -> m a) -> m a
withCurrentWorkDirectory uid action =
  withWorkDirectory uid $ \wd ->
    withCurrentDirectory wd $ \cd -> do
      copyDirectoryRecursive cd wd
      action wd

