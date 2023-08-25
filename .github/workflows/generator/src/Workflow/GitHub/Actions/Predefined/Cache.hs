module Workflow.GitHub.Actions.Predefined.Cache (step) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (intercalate)
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: [String] -> String -> GHA.Step
step paths key = GHA.actionStep "actions/cache@v2" params
  where
    params = M.fromList [("path", toJSON $ intercalate "\n" paths), ("key", toJSON key)]
