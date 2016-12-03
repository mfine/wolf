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
  { config :: FilePath
    -- ^ Configuration file.
  , plan   :: FilePath
    -- ^ Plan file to decide on.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run decider.
--
main :: IO ()
main = do
  args <- getRecord "Decider"
  runResourceT $ decide
    (config args)
    (plan args)
