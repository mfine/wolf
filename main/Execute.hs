{-# LANGUAGE ScopedTypeVariables #-}
module Execute
  ( main
  ) where

import BasicPrelude hiding ( readFile )
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Data.Text.IO
import Data.Yaml hiding ( Parser )
import Network.AWS.Flow
import Options
import Options.Applicative

data Args = Args
  { aConfig :: FilePath
  , aPlan   :: FilePath
  , aInput  :: Maybe FilePath
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args <$> configFile <*> planFile <*> inputFile

parser :: ParserInfo Args
parser =
  info ( helper <*> args ) $ fullDesc
    <> header   "execute: Execute a workflow"
    <> progDesc "Execute a workflow"

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  plan :: [Plan] <- decodeFile aPlan >>= maybeThrow (userError "Bad Plan")
  input <- readFileMaybe aInput
  env <- flowEnv config
  void $ runConcurrently $ sequenceA $ flip map plan $ \p ->
    Concurrently $ runResourceT $ runFlowT env $
      execute (strtTask $ plnStart p) input where
        readFileMaybe =
          maybe (return Nothing) ((>>= return . Just) . readFile)

main :: IO ()
main = execParser parser >>= call
