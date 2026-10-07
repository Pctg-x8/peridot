module Workflow.GitHub.Actions.Predefined.Google.Auth (viaWorkloadIdentityStep) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Workflow.GitHub.Actions qualified as GHA

viaWorkloadIdentityStep :: String -> String -> GHA.Step
viaWorkloadIdentityStep providerID serviceAccountName =
  GHA.actionStep "google-github-actions/auth@v1" $
    M.fromList
      [ ("workload_identity_provider", toJSON providerID),
        ("service_account", toJSON serviceAccountName)
      ]
