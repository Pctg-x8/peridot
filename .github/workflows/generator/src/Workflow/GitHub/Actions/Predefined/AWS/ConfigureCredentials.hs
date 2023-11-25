module Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials
  ( step,
    awsRegion,
    roleToAssume,
  )
where

import Workflow.GitHub.Actions qualified as GHA

-- data Params = Params
--   { audience :: Maybe String,
--     awsAccessKeyID :: Maybe String,
--     awsSecretAccessKey :: Maybe String,
--     awsSessionToken :: Maybe String,
--     awsRegion :: Maybe String,
--     maskAWSAccountID :: Maybe Bool,
--     roleToAssume :: Maybe String,
--     webIdentityTokenFile :: Maybe String,
--     roleDurationSeconds :: Maybe Int,
--     roleSessionName :: Maybe String,
--     roleExternalID :: Maybe String,
--     roleSkipSessionTagging :: Maybe Bool
--   }

-- defaultParams :: Params
-- defaultParams =
--   Params
--     { audience = Nothing,
--       awsAccessKeyID = Nothing,
--       awsSecretAccessKey = Nothing,
--       awsSessionToken = Nothing,
--       awsRegion = Nothing,
--       maskAWSAccountID = Nothing,
--       roleToAssume = Nothing,
--       webIdentityTokenFile = Nothing,
--       roleDurationSeconds = Nothing,
--       roleSessionName = Nothing,
--       roleExternalID = Nothing,
--       roleSkipSessionTagging = Nothing
--     }

step :: GHA.Step
step = GHA.actionStep "aws-actions/configure-aws-credentials@v1" mempty

awsRegion :: String -> GHA.StepModifier
awsRegion = GHA.stepSetWithParam "aws-region"

roleToAssume :: String -> GHA.StepModifier
roleToAssume = GHA.stepSetWithParam "role-to-assume"
