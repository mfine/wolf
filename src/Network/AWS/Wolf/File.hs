{-# LANGUAGE NoImplicitPrelude #-}

-- | Files, directories, and encoding / decoding file functions.
--
module Network.AWS.Wolf.File
  ( fromFile
  , toFile
  ) where

import Data.Aeson               as A
import Data.ByteString          as BS
import Data.ByteString.Lazy     as LBS
import Data.Yaml                as Y
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
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

