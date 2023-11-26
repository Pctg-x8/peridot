module CustomAction.CheckBuildSubdirectory (step) where

import Workflow.GitHub.Actions qualified as GHA

step :: String -> GHA.Step
step path =
  GHA.namedAs "Building as Checking" $
    GHA.env "TARGET_PATH" path $
      GHA.actionStep "./.github/actions/checkbuild-subdir" mempty
