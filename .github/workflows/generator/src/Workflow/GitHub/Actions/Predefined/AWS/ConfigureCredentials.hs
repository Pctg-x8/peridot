{-# LANGUAGE ImportQualifiedPost #-}

module Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials (Params (..), defaultParams, step) where

import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Workflow.GitHub.Actions qualified as GHA

data Params = Params
  { audience :: Maybe String,
    awsAccessKeyID :: Maybe String,
    awsSecretAccessKey :: Maybe String,
    awsSessionToken :: Maybe String,
    awsRegion :: Maybe String,
    maskAWSAccountID :: Maybe Bool,
    roleToAssume :: Maybe String,
    webIdentityTokenFile :: Maybe String,
    roleDurationSeconds :: Maybe Int,
    roleSessionName :: Maybe String,
    roleExternalID :: Maybe String,
    roleSkipSessionTagging :: Maybe Bool
  }

defaultParams :: Params
defaultParams =
  Params
    { audience = Nothing,
      awsAccessKeyID = Nothing,
      awsSecretAccessKey = Nothing,
      awsSessionToken = Nothing,
      awsRegion = Nothing,
      maskAWSAccountID = Nothing,
      roleToAssume = Nothing,
      webIdentityTokenFile = Nothing,
      roleDurationSeconds = Nothing,
      roleSessionName = Nothing,
      roleExternalID = Nothing,
      roleSkipSessionTagging = Nothing
    }

step :: Params -> GHA.Step
step p =
  GHA.step
    { GHA.stepName = Just "Configure AWS Credentials",
      GHA.uses = Just "aws-actions/configure-aws-credentials@v1",
      GHA.with =
        M.fromList $
          catMaybes
            [ ("audience",) . toJSON <$> audience p,
              ("awsAccessKeyID",) . toJSON <$> awsAccessKeyID p,
              ("awsSecretAccessKey",) . toJSON <$> awsSecretAccessKey p,
              ("awsSessionToken",) . toJSON <$> awsSessionToken p,
              ("awsRegion",) . toJSON <$> awsRegion p,
              ("maskAWSAccountID",) . toJSON <$> maskAWSAccountID p,
              ("roleToAssume",) . toJSON <$> roleToAssume p,
              ("webIdentityTokenFile",) . toJSON <$> webIdentityTokenFile p,
              ("roleDurationSeconds",) . toJSON <$> roleDurationSeconds p,
              ("roleSessionName",) . toJSON <$> roleSessionName p,
              ("roleExternalID",) . toJSON <$> roleExternalID p,
              ("roleSkipSessionTagging",) . toJSON <$> roleSkipSessionTagging p
            ]
    }
