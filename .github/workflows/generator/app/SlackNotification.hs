{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module SlackNotification
  ( SlackNotificationProvider (..),
    SlackReportContext (..),
    SlackReporter (..),
    withSlackReport,
    reportJobFailure,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials
import Workflow.GitHub.Actions.Predefined.Checkout qualified as CheckoutAction

configureSlackNotification :: GHA.Step
configureSlackNotification =
  GHA.namedAs "Configure for Slack Notification" $
    AWSConfigureCredentials.step
      & AWSConfigureCredentials.awsRegion "ap-northeast-1"
      & AWSConfigureCredentials.roleToAssume "arn:aws:iam::208140986057:role/GHALambdaInvoker"

class SlackReportContext m where
  reportSuccessSteps :: m [GHA.Step]
  reportFailureSteps :: String -> m [GHA.Step]

reportJobFailure :: (SlackReportContext m) => (Functor m) => GHA.Job -> m GHA.Job
reportJobFailure job =
  let jobName = fromMaybe "<unknown job>" $ GHA.nameOf job
   in reportFailureSteps jobName <&> \reportSteps ->
        GHA.grantWritable GHA.IDTokenPermission $ GHA.jobAppendSteps (runOnFailure <$> reportSteps) job

data SlackNotificationProvider = SlackNotificationProvider
  { buildSuccessReportStep :: GHA.Step,
    buildFailureReportStep :: String -> GHA.Step
  }

newtype SlackReporter m a = SlackReporter {runSlackReporter :: SlackNotificationProvider -> m a}

instance (Applicative m) => SlackReportContext (SlackReporter m) where
  reportSuccessSteps = SlackReporter \p ->
    pure
      [ CheckoutAction.step (Just $ GHA.mkExpression "github.event.pull_request.head.sha"),
        configureSlackNotification,
        buildSuccessReportStep p
      ]
  reportFailureSteps jobName = SlackReporter \p ->
    pure
      [ CheckoutAction.step (Just $ GHA.mkExpression "github.event.pull_request.head.sha"),
        configureSlackNotification,
        buildFailureReportStep p jobName
      ]

instance (Functor m) => Functor (SlackReporter m) where
  fmap f a = SlackReporter $ fmap f . runSlackReporter a

instance (Applicative m) => Applicative (SlackReporter m) where
  pure a = SlackReporter \_ -> pure a
  f <*> a = SlackReporter \p -> runSlackReporter f p <*> runSlackReporter a p

instance (Monad m) => Monad (SlackReporter m) where
  return = pure
  a >>= f = SlackReporter \p -> runSlackReporter a p >>= \a' -> runSlackReporter (f a') p

withSlackReport :: SlackNotificationProvider -> SlackReporter m a -> m a
withSlackReport = flip runSlackReporter
