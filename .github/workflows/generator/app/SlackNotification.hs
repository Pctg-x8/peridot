{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module SlackNotification
  ( SlackNotificationProvider (..),
    SlackReportContext (..),
    SlackReporter (..),
    withSlackReport,
    reportJobFailure,
    reportSuccessSteps,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials
import Workflow.GitHub.Actions.Predefined.Checkout qualified as CheckoutAction

checkoutHeadStep :: GHA.Step
checkoutHeadStep = CheckoutAction.step $ Just $ GHA.mkExpression "github.event.pull_request.head.sha"

configureSlackNotification :: GHA.Step
configureSlackNotification =
  GHA.namedAs "Configure for Slack Notification" $
    AWSConfigureCredentials.step
      & AWSConfigureCredentials.awsRegion "ap-northeast-1"
      & AWSConfigureCredentials.roleToAssume "arn:aws:iam::208140986057:role/GHALambdaInvoker"

class SlackReportContext m where
  successReportStep :: m GHA.Step
  failureReportStep :: String -> m GHA.Step

reportSuccessSteps :: (SlackReportContext m, Functor m) => m [GHA.Step]
reportSuccessSteps = successReportStep <&> \reportStep -> [checkoutHeadStep, configureSlackNotification, reportStep]

reportJobFailure :: (SlackReportContext m) => (Functor m) => GHA.Job -> m GHA.Job
reportJobFailure job =
  let jobName = fromMaybe "<unknown job>" $ GHA.nameOf job
   in failureReportStep jobName <&> \reportStep ->
        GHA.grantWritable GHA.IDTokenPermission $
          GHA.jobAppendSteps
            (runOnFailure <$> [checkoutHeadStep, configureSlackNotification, reportStep])
            job

data SlackNotificationProvider = SlackNotificationProvider
  { buildSuccessReportStep :: GHA.Step,
    buildFailureReportStep :: String -> GHA.Step
  }

newtype SlackReporter m a = SlackReporter {runSlackReporter :: SlackNotificationProvider -> m a}

instance (Applicative m) => SlackReportContext (SlackReporter m) where
  successReportStep = SlackReporter $ pure . buildSuccessReportStep
  failureReportStep jobName = SlackReporter \p -> pure $ buildFailureReportStep p jobName

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
