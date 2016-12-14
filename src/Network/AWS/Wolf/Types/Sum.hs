{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various sum types.
--
module Network.AWS.Wolf.Types.Sum where

import Data.Aeson.TH
import Network.AWS.Wolf.Aeson
import Network.AWS.Wolf.Prelude

-- | EncodeType
--
-- Types of file encodings.
--
data EncodeType
  = EncodeAeson
  | EncodeYaml
  deriving (Show, Eq)

$(deriveJSON spinalOptions ''EncodeType)

