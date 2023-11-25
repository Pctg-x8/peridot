module Main (main) where

import AutoDeliveryDev qualified
import Control.Monad (forM_)
import Data.Aeson.Yaml (encode)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import DocumentDeployment qualified
import IntegrityTest.PullRequestTriggered
import IntegrityTest.Weekly
import SDKBuild qualified
import System.Environment (getArgs)
import System.FilePath ((</>))
import Workflow.GitHub.Actions qualified as GHA

basePath :: IO FilePath
basePath = head <$> getArgs

main :: IO ()
main =
  basePath
    >>= buildWorkflows
      [ ("integrity-test.yml", integrityTest),
        ("weekly-integrity-test.yml", weeklyIntegrityTest),
        ("docs-cd.yml", DocumentDeployment.workflow),
        ("auto-delivery-dev.yml", AutoDeliveryDev.workflow),
        ("sdk-build.yml", SDKBuild.workflow)
      ]

buildWorkflows :: (Foldable f) => f (FilePath, GHA.Workflow) -> FilePath -> IO ()
buildWorkflows xs base = forM_ xs $ uncurry LBS8.writeFile . bimap (base </>) encode
