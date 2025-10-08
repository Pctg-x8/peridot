module Workflow.GitHub.Actions.Predefined.Checkout (step, Submodules(..), submodules) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: Maybe String -> GHA.Step
step = GHA.actionStep "actions/checkout@v4" . maybe mempty (M.singleton "ref" . toJSON)

data Submodules = SubmodulesFalse | SubmodulesTrue | SubmodulesRecursive
instance ToJSON Submodules where
  toJSON SubmodulesFalse = toJSON False
  toJSON SubmodulesTrue = toJSON True
  toJSON SubmodulesRecursive = toJSON ("recursive" :: String)

submodules :: Submodules -> GHA.StepModifier
submodules = GHA.stepSetWithParam "submodules"
