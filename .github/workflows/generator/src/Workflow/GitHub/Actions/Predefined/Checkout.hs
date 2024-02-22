module Workflow.GitHub.Actions.Predefined.Checkout (step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: Maybe String -> GHA.Step
step = GHA.actionStep "actions/checkout@v3" . maybe mempty (M.singleton "ref" . toJSON)
