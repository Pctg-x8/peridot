module Workflow.GitHub.Actions.Predefined.Cache (step, restoreKeys, refCacheHit) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (intercalate)
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.ExpressionBuilder (mkRefStepOutputExpression)

step :: [String] -> String -> GHA.Step
step paths key = GHA.actionStep "actions/cache@v4" params
  where
    params = M.fromList [("path", toJSON $ intercalate "\n" paths), ("key", toJSON key)]

restoreKeys :: [String] -> GHA.StepModifier
restoreKeys = GHA.stepSetWithParam "restore-keys" . toJSON . intercalate "\n"

refCacheHit :: String -> String
refCacheHit stepId = mkRefStepOutputExpression stepId "cache-hit"
