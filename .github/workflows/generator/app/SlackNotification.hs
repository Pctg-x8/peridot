{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module SlackNotification
  ( SlackReport (..),
    SlackNotificationProvider (..),
    SlackNotifyContext,
    withSlackNotification,
    slackNotifySteps,
    reportJobFailure,
  )
where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Strict (Reader, reader, runReader)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials

configureSlackNotification :: GHA.Step
configureSlackNotification =
  GHA.namedAs "Configure for Slack Notification" $
    AWSConfigureCredentials.step
      & AWSConfigureCredentials.awsRegion "ap-northeast-1"
      & AWSConfigureCredentials.roleToAssume "arn:aws:iam::208140986057:role/GHALambdaInvoker"

data SlackReport = ReportSuccess | ReportFailure String

data SlackNotificationProvider = SlackNotificationProvider
  { buildSuccessReportStep :: GHA.Step,
    buildFailureReportStep :: String -> GHA.Step
  }

type SlackNotifyContext = Reader SlackNotificationProvider

withSlackNotification :: SlackNotificationProvider -> Eff (SlackNotifyContext : r) a -> Eff r a
withSlackNotification = runReader

slackNotifySteps :: (Member SlackNotifyContext r) => SlackReport -> Eff r [GHA.Step]
slackNotifySteps ReportSuccess = reader \p -> [configureSlackNotification, buildSuccessReportStep p]
slackNotifySteps (ReportFailure jobName) = reader \p -> [configureSlackNotification, buildFailureReportStep p jobName]

reportJobFailure :: (Member SlackNotifyContext r) => GHA.Job -> Eff r GHA.Job
reportJobFailure job =
  let jobName = fromMaybe "<unknown job>" $ GHA.nameOf job
   in slackNotifySteps (ReportFailure jobName) <&> \reportSteps ->
        GHA.grantWritable GHA.IDTokenPermission $ GHA.jobAppendSteps (runOnFailure <$> reportSteps) job
