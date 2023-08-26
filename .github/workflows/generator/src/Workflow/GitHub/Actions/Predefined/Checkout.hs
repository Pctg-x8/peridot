module Workflow.GitHub.Actions.Predefined.Checkout (step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: Maybe String -> GHA.Step
step h = GHA.actionStep "actions/checkout@v3" $ maybe M.empty (M.singleton "ref" . toJSON) h
