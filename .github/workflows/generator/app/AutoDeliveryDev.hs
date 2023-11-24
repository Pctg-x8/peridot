module AutoDeliveryDev (workflow) where

import CustomAction.CreateDevDeliverPRs qualified as CreateDevDeliverPRsAction
import Data.Function ((&))
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout

fetchAllBranchesStep :: GHA.Step
fetchAllBranchesStep = GHA.namedAs "Fetching all branches" $ GHA.runStep "git fetch --no-tags -p --depth=1 origin +refs/heads/*:refs/remotes/origin/*"

workflow :: GHA.Workflow
workflow =
  GHA.buildWorkflow
    [ GHA.namedAs "SubProject-dev Auto Deliveries",
      GHA.workflowJob "make-pr-for-deliver" deliverJob
    ]
    trigger
  where
    trigger = GHA.onPush $ GHA.workflowPushTrigger & GHA.filterBranch "dev"
    deliverJob =
      GHA.namedAs "Make Delivering Diffs" $
        GHA.job
          [Checkout.step Nothing, fetchAllBranchesStep, CreateDevDeliverPRsAction.step]
