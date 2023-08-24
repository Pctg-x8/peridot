module CustomAction.CheckBuildSubdirectory (step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

step :: String -> GHA.Step
step path = GHA.namedAs "Building as Checking" $ GHA.actionStep "./.github/actions/checkbuild-subdir" $ M.singleton "path" $ toJSON path
