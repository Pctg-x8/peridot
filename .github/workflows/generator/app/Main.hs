module Main (main) where

import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import DocumentDeployment qualified
import IntegrityTest.PullRequestTriggered
import IntegrityTest.Weekly
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  dest <- head <$> getArgs
  LBS8.writeFile (dest </> "integrity-test.yml") $ encode integrityTest
  LBS8.writeFile (dest </> "weekly-integrity-test.yml") $ encode weeklyIntegrityTest
  LBS8.writeFile (dest </> "docs-cd.yml") $ encode DocumentDeployment.workflow
