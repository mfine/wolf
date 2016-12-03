{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Program constants and defaults
--
module Network.AWS.Wolf.Constant where

import Network.AWS.Wolf.Prelude

-- | Environment variable name of AWS access key.
--
awsAccessKey :: Text
awsAccessKey = "AWS_ACCESS_KEY_ID"

-- | Environment variable name of AWS secret key.
--
awsSecretKey :: Text
awsSecretKey = "AWS_SECRET_ACCESS_KEY"
