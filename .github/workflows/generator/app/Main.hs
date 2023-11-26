module Main (main) where

import AutoDeliveryDev qualified
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Aeson.Yaml (encode)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (listToMaybe)
import DocumentDeployment qualified
import IntegrityTest.PullRequestTriggered
import IntegrityTest.Weekly
import SDKBuild qualified
import System.Environment (getArgs)
import System.FilePath ((</>))
import Workflow.GitHub.Actions qualified as GHA

targets :: [(FilePath, GHA.Workflow)]
targets =
  [ ("integrity-test.yml", integrityTest),
    ("weekly-integrity-test.yml", weeklyIntegrityTest),
    ("docs-cd.yml", DocumentDeployment.workflow),
    ("auto-delivery-dev.yml", AutoDeliveryDev.workflow),
    ("sdk-build.yml", SDKBuild.workflow)
  ]

data MissingArgumentException = BasePathRequired

instance Show MissingArgumentException where
  show BasePathRequired = "BasePath argument required"

instance Exception MissingArgumentException

throwIfNothing :: (Exception e) => e -> Maybe a -> IO a
throwIfNothing e = maybe (throwIO e) pure

main :: IO ()
main = do
  base <- getArgs >>= throwIfNothing BasePathRequired . listToMaybe
  forM_ targets $ uncurry LBS8.writeFile . bimap (base </>) encode
