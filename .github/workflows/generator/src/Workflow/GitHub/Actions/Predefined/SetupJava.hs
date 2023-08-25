module Workflow.GitHub.Actions.Predefined.SetupJava (step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions qualified as GHA

-- | distribution, version(optional when using java-version-file)
step :: String -> Maybe String -> GHA.Step
step dist ver =
  GHA.actionStep "actions/setup-java@v3" $
    M.fromList $
      catMaybes
        [ Just ("distribution", toJSON dist),
          ("java-version",) . toJSON <$> ver
        ]
