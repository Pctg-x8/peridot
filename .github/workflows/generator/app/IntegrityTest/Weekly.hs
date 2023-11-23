{-# LANGUAGE NoOverloadedStrings #-}

module IntegrityTest.Weekly (weeklyIntegrityTest) where

import Control.Eff (run)
import CustomAction.PostCINotifications qualified as PostCINotificationsAction
import Data.Map qualified as M
import IntegrityTest.Shared
import SlackNotification
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.JobGroupComposer ((~=>))

weeklySlackNotifyProvider :: SlackNotificationProvider
weeklySlackNotifyProvider = SlackNotificationProvider succ' fail'
  where
    mkParams status =
      PostCINotificationsAction.Params
        { PostCINotificationsAction.status = status,
          PostCINotificationsAction.beginTime = GHA.mkNeedsOutputExpression "preconditions" "begintime",
          PostCINotificationsAction.reportName = "Weekly Check",
          PostCINotificationsAction.mode = PostCINotificationsAction.BranchMode
        }
    fail' jobName =
      GHA.namedAs "Notify as Failure" $
        PostCINotificationsAction.step $
          mkParams $
            PostCINotificationsAction.FailureStatus jobName
    succ' =
      GHA.namedAs "Notify as Success" $
        PostCINotificationsAction.step $
          mkParams PostCINotificationsAction.SuccessStatus

weeklyIntegrityTest :: GHA.Workflow
weeklyIntegrityTest = run $ withSlackNotification weeklySlackNotifyProvider $ do
  let preconditions' =
        M.singleton "preconditions" $
          applyModifiers [GHA.namedAs "Preconditions", preconditionBeginTimestampOutputDef] $
            GHA.job [preconditionRecordBeginTimeStamp]
  reportSuccessJob' <- M.singleton "report-success" <$> reportSuccessJob

  checkJobs <- do
    checkFormats' <- M.singleton "check-formats" <$> checkFormats "true"
    checkBaseLayer' <- M.singleton "check-baselayer" <$> checkBaseLayer "true"
    checkTools' <- M.singleton "check-tools" <$> checkTools "true"
    checkModules' <- M.singleton "check-modules" <$> checkModules "true"
    checkExamples' <- M.singleton "check-examples" <$> checkExamples "true"
    checkCradleWindows' <- M.singleton "check-cradle-windows" <$> checkCradleWindows "true"
    checkCradleMacos' <- M.singleton "check-cradle-macos" <$> checkCradleMacos "true"
    checkCradleLinux' <- M.singleton "check-cradle-linux" <$> checkCradleLinux "true"
    checkCradleAndroid' <- M.singleton "check-cradle-android" <$> checkCradleAndroid "true"

    pure
      [ checkFormats',
        checkBaseLayer'
          ~=> [checkTools', checkModules' ~=> checkExamples']
          ~=> [checkCradleWindows', checkCradleMacos', checkCradleLinux', checkCradleAndroid']
      ]

  pure $
    (GHA.emptyWorkflow $ GHA.scheduled [GHA.CronTimer "0 12 * * wed"])
      { GHA.workflowName = Just "Integrity Check (Weekly)",
        GHA.workflowJobs = preconditions' ~=> checkJobs ~=> reportSuccessJob'
      }
