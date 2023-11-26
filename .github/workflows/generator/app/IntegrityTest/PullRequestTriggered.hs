{-# LANGUAGE NoOverloadedStrings #-}

module IntegrityTest.PullRequestTriggered (integrityTest) where

import Control.Eff (run)
import CustomAction.PostCINotifications qualified as PostCINotificationsAction
import Data.Function ((&))
import Data.Map qualified as M
import IntegrityTest.Shared
import SlackNotification
import Utils
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.JobGroupComposer ((~=>))

repositoryOwnerLoginExpr, repositoryNameExpr :: String
repositoryOwnerLoginExpr = GHA.mkExpression "github.event.repository.owner.login"
repositoryNameExpr = GHA.mkExpression "github.event.repository.name"

preconditionOutputHasChanges :: String
preconditionOutputHasChanges = GHA.mkExpression $ GHA.mkNeedsOutputPath "preconditions" "has_code_changes" <> " == 1"

-- preconditionOutputHasWorkflowChanges :: String
-- preconditionOutputHasWorkflowChanges = GHA.mkExpression $ GHA.mkNeedsOutputPath "preconditions" "has_workflow_changes" <> " == 1"

slackNotifyProvider :: SlackNotificationProvider
slackNotifyProvider = SlackNotificationProvider succ' fail'
  where
    mkParams status =
      PostCINotificationsAction.Params
        { PostCINotificationsAction.status = status,
          PostCINotificationsAction.beginTime = GHA.mkNeedsOutputExpression "preconditions" "begintime",
          PostCINotificationsAction.reportName = "PR Integrity Check",
          PostCINotificationsAction.mode = PostCINotificationsAction.currentPullRequestDiffMode
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

escapeDoubleQuote :: String -> String
escapeDoubleQuote org = org >>= repr
  where
    repr '"' = "\\\""
    repr x = pure x

preconditions :: GHA.Job
preconditions =
  applyModifiers
    [ GHA.namedAs "Preconditions",
      preconditionBeginTimestampOutputDef,
      GHA.jobForwardingStepOutput "fileck" "has_code_changes",
      GHA.jobForwardingStepOutput "fileck" "has_workflow_changes"
    ]
    $ GHA.job [preconditionRecordBeginTimeStamp, collectChangesStep]
  where
    collectChangesStep =
      applyModifiers [GHA.namedAs "Checking Changed Filenames", GHA.identifiedAs "fileck"] $
        GHA.runStep $
          "\
          \HAS_CODE_CHANGES=0\n\
          \HAS_WORKFLOW_CHANGES=0\n\
          \QUERY_STRING='"
            <> escapeDoubleQuote queryString
            <> "'\n\
               \QUERY_CURSOR='null'\n\
               \while :; do\n\
               \  POSTDATA=\"{ \\\"query\\\": \\\"$QUERY_STRING\\\", \\\"variables\\\": { \\\"cursor\\\": $QUERY_CURSOR }\\\" } }\"\n\
               \  echo $POSTDATA\n\
               \  API_RESPONSE=$("
            <> apiRequest
            <> ")\n\
               \  echo $API_RESPONSE\n\
               \  PATHS=$(echo $API_RESPONSE | jq \".data.repository.pullRequest.files.nodes[].path\")\n\
               \  echo $PATHS\n\
               \  echo $PATHS | grep -qE '\\.rs\"|Cargo(\\.template)?\\.toml\"' && :\n\
               \  if [[ $? == 0 ]]; then HAS_CODE_CHANGES=1; fi\n\
               \  echo $PATHS | grep -qE '\\.dhall\"' && :\n\
               \  if [[ $? == 0 ]]; then HAS_WORKFLOW_CHANGES=1; fi\n\
               \  if [[ $HAS_CODE_CHANGES == 1 && $HAS_WORKFLOW_CHANGES == 1 ]]; then break; fi\n\
               \  HAS_NEXT_PAGE=$(echo $API_RESPONSE | jq \".data.repository.pullRequest.files.pageInfo.hasNextPage\")\n\
               \  if [[ \"$HAS_NEXT_PAGE\" == \"true\" ]]; then\n\
               \    QUERY_CURSOR=$(echo $API_RESPONSE | jq \".data.repository.pullRequest.files.pageInfo.endCursor\")\n\
               \  else\n\
               \    break\n\
               \  fi\n\
               \done < <(cat)\n\
               \echo \"HAS_CODE_CHANGES?$HAS_CODE_CHANGES\"\n\
               \echo \"HAS_WORKFLOW_CHANGES?$HAS_WORKFLOW_CHANGES\"\n\
               \echo \"has_code_changes=$HAS_CODE_CHANGES\" >> $GITHUB_OUTPUT\n\
               \echo \"has_workflow_changes=$HAS_WORKFLOW_CHANGES\" >> $GITHUB_OUTPUT\n\
               \"
    queryString = "query($cursor: String) { repository(owner: \"" <> repositoryOwnerLoginExpr <> "\", name: \"" <> repositoryNameExpr <> "\") { pullRequest(number: " <> pullRequestNumberExpr <> ") { files(first: 50, after: $cursor) { nodes { path } pageInfo { hasNextPage endCursor } } } } }"
    apiRequestAuthHeader = "\"Authorization: Bearer " <> GHA.githubToken <> "\""
    apiRequest = "curl -s -H " <> apiRequestAuthHeader <> " -X POST -d \"$POSTDATA\" https://api.github.com/graphql"

integrityTest :: GHA.Workflow
integrityTest = run $ withSlackNotification slackNotifyProvider do
  let preconditions' = M.singleton "preconditions" preconditions
  reportSuccessJob' <- M.singleton "report-success" <$> reportSuccessJob

  checkJobs <- do
    checkFormats' <- M.singleton "check-formats" <$> checkFormats preconditionOutputHasChanges
    checkBaseLayer' <- M.singleton "check-baselayer" <$> checkBaseLayer preconditionOutputHasChanges
    checkTools' <- M.singleton "check-tools" <$> checkTools preconditionOutputHasChanges
    checkModules' <- M.singleton "check-modules" <$> checkModules preconditionOutputHasChanges
    checkExamples' <- M.singleton "check-examples" <$> checkExamples preconditionOutputHasChanges
    checkCradleWindows' <- M.singleton "check-cradle-windows" <$> checkCradleWindows preconditionOutputHasChanges
    checkCradleMacos' <- M.singleton "check-cradle-macos" <$> checkCradleMacos preconditionOutputHasChanges
    checkCradleLinux' <- M.singleton "check-cradle-linux" <$> checkCradleLinux preconditionOutputHasChanges
    checkCradleAndroid' <- M.singleton "check-cradle-android" <$> checkCradleAndroid preconditionOutputHasChanges

    pure
      [ checkFormats',
        checkBaseLayer'
          ~=> [checkTools', checkModules' ~=> checkExamples']
          ~=> [checkCradleWindows', checkCradleMacos', checkCradleLinux', checkCradleAndroid']
      ]

  pure
    $ GHA.buildWorkflow
      [ GHA.namedAs "Integrity Check",
        GHA.concurrentPolicy $ GHA.ConcurrentCancelledGroup $ GHA.mkExpression "github.ref",
        GHA.workflowReplaceJobs $ preconditions' ~=> checkJobs ~=> reportSuccessJob'
      ]
    $ GHA.onPullRequest
    $ GHA.workflowPullRequestTrigger & GHA.filterType "opened" & GHA.filterType "synchronize"
