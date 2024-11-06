module CustomAction.CheckBuildBaseLayer (step) where

import Workflow.GitHub.Actions qualified as GHA

step :: GHA.Step
step = GHA.actionStep "./.github/actions/checkbuild-baselayer" mempty
