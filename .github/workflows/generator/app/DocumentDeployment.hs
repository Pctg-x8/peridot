module DocumentDeployment (workflow) where

import Data.Map qualified as M
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
buildStep =
  applyModifiers
    [ GHA.namedAs "Build docs"
    ]
    $ GHA.actionStep "./.github/actions/build-doc" M.empty

deploymentStep :: GHA.Step
deploymentStep =
  applyModifiers [GHA.namedAs "Deployment"] $ GHA.actionStep "./.github/actions/deployment-dev" M.empty

workflow :: GHA.Workflow
workflow =
  GHA.buildWorkflow
    [ GHA.namedAs "Document Continuous Deployment (for dev)",
      GHA.workflowJob "doc-gen-deploy" job
    ]
    $ GHA.onPush trigger
  where
    trigger =
      applyModifiers
        [ GHA.filterBranch "dev",
          GHA.filterPath "**.rs",
          GHA.filterPath "**.toml"
        ]
        GHA.workflowPushTrigger
    job =
      applyModifiers
        [ GHA.namedAs "Doc Generate and Deploy",
          GHA.grantWritable GHA.IDTokenPermission,
          GHA.grantReadable GHA.ContentsPermission,
          GHA.runInEnvironment $ GHA.RepositoryEnvironment "dev-document"
        ]
        $ GHA.job
          [ Checkout.step $ Just "dev",
            authStep,
            buildStep,
            deploymentStep
          ]
