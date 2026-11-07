module Workflow.GitHub.Actions.Predefined.InstallLLVM (step, isCached) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions.Step (Step, StepModifier, actionStep, stepSetWithParam)

step :: String -> Step
step = actionStep "KyleMayes/install-llvm-action@v1" . M.singleton "version" . toJSON

isCached :: (ToJSON v) => v -> StepModifier
isCached = stepSetWithParam "cached"
