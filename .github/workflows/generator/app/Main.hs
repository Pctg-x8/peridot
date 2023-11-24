module Main (main) where

import AutoDeliveryDev qualified
import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (traverse_)
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
buildWorkflows xs base = traverse_ (\(p, w) -> LBS8.writeFile (base </> p) $ encode w) xs
