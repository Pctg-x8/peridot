{-# LANGUAGE NoOverloadedStrings #-}

module IntegrityTest.Weekly (weeklyIntegrityTest) where

import CustomAction.PostCINotifications qualified as PostCINotificationsAction
import Data.Functor.Identity (runIdentity)
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
weeklyIntegrityTest = runIdentity $ withSlackReport weeklySlackNotifyProvider $ do
  let preconditions' =
        M.singleton "preconditions" $
          applyModifiers [GHA.namedAs "Preconditions", preconditionBeginTimestampOutputDef] $
            GHA.job $
              flattenSteps
                [ Step preconditionRecordBeginTimeStamp
                ]
  reportSuccessJob' <- M.singleton "report-success" <$> reportSuccessJob

  checkJobs <- do
    checkFormats' <- M.singleton "check-formats" <$> checkFormats "true"
    preciseCheck' <- M.singleton "check-precise" <$> preciseCheck "true"
    checkCradleAll' <- checkCradleAll "true"
    checkEditorMP' <- checkEditorMP "true"

    pure
      [ checkFormats',
        preciseCheck' ~=> [checkCradleAll', checkEditorMP']
      ]

  pure
    $ GHA.buildWorkflow
      [ GHA.namedAs "Integrity Check (Weekly)",
        GHA.workflowReplaceJobs $ preconditions' ~=> checkJobs ~=> reportSuccessJob'
      ]
    $ GHA.scheduled [GHA.CronTimer "0 12 * * wed"]
