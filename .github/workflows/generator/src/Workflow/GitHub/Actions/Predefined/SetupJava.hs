module Workflow.GitHub.Actions.Predefined.SetupJava (step, javaVersion) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

-- | specify distribution, java-version is optional if java-version-file is provided
step :: String -> GHA.Step
step = GHA.actionStep "actions/setup-java@v3" . M.singleton "distribution" . toJSON

javaVersion :: String -> GHA.StepModifier
javaVersion = GHA.stepSetWithParam "java-version"
