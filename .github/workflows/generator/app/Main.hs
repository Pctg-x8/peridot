module Main (main) where

import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import IntegrityTest.PullRequestTriggered
import IntegrityTest.Weekly

main :: IO ()
main = do
  LBS8.writeFile "./integrity-test.yml" $ encode integrityTest
  LBS8.writeFile "./weekly-integrity-test.yml" $ encode weeklyIntegrityTest
