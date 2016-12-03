{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run actor.
--
import Options.Generic
import Network.AWS.Wolf

-- | Args
--
-- Program arguments.
--
data Args = Args
  { config  :: FilePath
    -- ^ Configuration file.
  , queue   :: Text
    -- ^ Queue to listen to act on.
  , command :: Text
    -- ^ Command to run.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run actor.
--
main :: IO ()
main = do
  args <- getRecord "Actor"
  runResourceT $ act
    (config args)
    (queue args)
    (command args)
