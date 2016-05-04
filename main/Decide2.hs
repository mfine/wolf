module Decide
  ( main
  ) where

import BasicPrelude
import Control.Monad.Trans.Resource
import Data.Text ( pack )
import Data.Yaml hiding ( Parser )
import Network.AWS.Flow
import Options
import Options.Applicative

data Args = Args
  { aConfig :: FilePath
  , aQueue  :: Queue
  } deriving ( Eq, Read, Show )

args :: Parser Args
args = Args <$> configFile <*> (pack <$> queue)

parser :: ParserInfo Args
parser =
  info ( helper <*> args ) $ fullDesc
    <> header   "decide2: Decide a workflow"
    <> progDesc "Decide a workflow"

call :: Args -> IO ()
call Args{..} = do
  config <- decodeFile aConfig >>= maybeThrow (userError "Bad Config")
  env <- flowEnv config
  forever $ runResourceT $ runFlowT env $
    decide2 aQueue

main :: IO ()
main = execParser parser >>= call
