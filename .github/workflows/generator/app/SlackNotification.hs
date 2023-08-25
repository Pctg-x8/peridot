{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module SlackNotification
  ( SlackNotification (..),
    SlackNotificationProvider (..),
    SlackNotifyContext,
    slackNotifySteps,
    reportJobFailure,
  )
where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Strict (Reader, reader)
import Data.Maybe (fromMaybe)
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials

configureSlackNotification :: GHA.Step
configureSlackNotification =
  applyModifiers
    [ GHA.namedAs "Configure for Slack Notification",
      AWSConfigureCredentials.awsRegion "ap-northeast-1",
      AWSConfigureCredentials.roleToAssume "arn:aws:iam::208140986057:role/GHALambdaInvoker"
    ]
    AWSConfigureCredentials.step

data SlackNotification = ReportSuccess | ReportFailure String

data SlackNotificationProvider = SlackNotificationProvider {buildSuccessReportStep :: GHA.Step, buildFailureReportStep :: String -> GHA.Step}

type SlackNotifyContext = Reader SlackNotificationProvider

slackNotifySteps :: (Member SlackNotifyContext r) => SlackNotification -> Eff r [GHA.Step]
slackNotifySteps ReportSuccess = reader $ \provider ->
  [configureSlackNotification, buildSuccessReportStep provider]
slackNotifySteps (ReportFailure jobName) = reader $ \provider ->
  [configureSlackNotification, buildFailureReportStep provider jobName]

reportJobFailure :: (Member SlackNotifyContext r) => GHA.Job -> Eff r GHA.Job
reportJobFailure job = do
  reportSteps <- slackNotifySteps $ ReportFailure $ fromMaybe "<unknown job>" $ GHA.nameOf job
  pure $
    GHA.grantWritable GHA.IDTokenPermission $
      GHA.jobModifySteps (<> fmap runOnFailure reportSteps) job
