module Workflow.GitHub.Actions.Predefined.Checkout (Params (..), defaultParams, step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions qualified as GHA

newtype Params = Params {ref :: Maybe String}

defaultParams :: Params
defaultParams = Params {ref = Nothing}

step :: Params -> GHA.Step
step p =
  GHA.step
    { GHA.stepName = Just "Checking out",
      GHA.uses = Just "actions/checkout@v3",
      GHA.with = M.fromList $ catMaybes [("ref",) . toJSON <$> ref p]
    }
