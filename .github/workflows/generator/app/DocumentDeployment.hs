module DocumentDeployment (workflow) where

import Data.Function ((&))
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Google.Auth qualified as GoogleAuthAction

authStep :: GHA.Step
authStep =
  GoogleAuthAction.viaWorkloadIdentityStep
    "projects/488336166113/locations/global/workloadIdentityPools/github-actions-oidc-federation/providers/github-actions"
    "dev-autocd-deployer@docs-peridot.iam.gserviceaccount.com"

buildStep :: GHA.Step
buildStep = GHA.namedAs "Build docs" $ GHA.actionStep "./.github/actions/build-doc" mempty

deployStep :: GHA.Step
deployStep = GHA.namedAs "Deploy" $ GHA.actionStep "./.github/actions/deployment-dev" mempty

workflow :: GHA.Workflow
workflow =
  GHA.buildWorkflow
    [ GHA.namedAs "Document Continuous Deployment (for dev)",
      GHA.workflowJob "doc-gen-deploy" job
    ]
    $ GHA.onPush trigger
  where
    trigger = GHA.workflowPushTrigger & GHA.filterBranch "dev" & GHA.filterPath "**.rs" & GHA.filterPath "**.toml"
    job =
      applyModifiers
        [ GHA.namedAs "Doc Generate and Deploy",
          GHA.grantWritable GHA.IDTokenPermission,
          GHA.grantReadable GHA.ContentsPermission,
          GHA.runInEnvironment $ GHA.RepositoryEnvironment "dev-document"
        ]
        $ GHA.job [Checkout.step $ Just "dev", authStep, buildStep, deployStep]
