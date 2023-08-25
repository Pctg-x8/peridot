{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main (main) where

import Control.Eff
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import CustomAction.CheckBuildSubdirectory qualified as CheckBuildSubdirAction
import CustomAction.CodeFormChecker qualified as CodeFormCheckerAction
import CustomAction.PostCINotifications qualified as PostCINotificationsAction
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.JobGroupComposer ((~=>))
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction
import Workflow.GitHub.Actions.Predefined.SetupJava qualified as SetupJavaAction

pullRequestHeadHashExpr, pullRequestNumberExpr :: String
pullRequestHeadHashExpr = GHA.mkExpression "github.event.pull_request.head.sha"
pullRequestNumberExpr = GHA.mkExpression "github.event.number"

repositoryOwnerLoginExpr, repositoryNameExpr :: String
repositoryOwnerLoginExpr = GHA.mkExpression "github.event.repository.owner.login"
repositoryNameExpr = GHA.mkExpression "github.event.repository.name"

secretGitHubTokenExpr :: String
secretGitHubTokenExpr = GHA.mkExpression "secrets.GITHUB_TOKEN"

applyModifiers :: (Foldable f) => f (a -> a) -> a -> a
applyModifiers = flip $ foldl (flip ($))

runOnFailure :: (GHA.Conditional c) => c -> c
runOnFailure = GHA.withCondition "failure()"

configureSlackNotification :: GHA.Step
configureSlackNotification =
  applyModifiers
    [ GHA.namedAs "Configure for Slack Notification",
      AWSConfigureCredentials.awsRegion "ap-northeast-1",
      AWSConfigureCredentials.roleToAssume "arn:aws:iam::208140986057:role/GHALambdaInvoker"
    ]
    AWSConfigureCredentials.step

preconditionRecordBeginTimeStamp :: GHA.Step
preconditionRecordBeginTimeStamp =
  applyModifiers [GHA.identifiedAs "begintime", GHA.namedAs "Getting begintime"] $
    GHA.runStep "echo \"begintime=$(date +%s)\" >> $GITHUB_OUTPUT"

preconditionBeginTimestampOutputDef :: GHA.Job -> GHA.Job
preconditionBeginTimestampOutputDef = GHA.jobOutput "begintime" $ GHA.mkRefStepOutputExpression "begintime" "begintime"

checkoutStep, checkoutHeadStep :: GHA.Step
checkoutStep = Checkout.step Nothing
checkoutHeadStep =
  GHA.namedAs "Checking out (HEAD commit)" $
    Checkout.step $
      Just pullRequestHeadHashExpr

cacheStep :: [String] -> String -> GHA.Step
cacheStep paths key =
  GHA.actionStep "actions/cache@v2" $
    M.fromList
      [ ("path", toJSON $ intercalate "\n" paths),
        ("key", toJSON key)
      ]

rustCacheStep, llvmCacheStep :: GHA.Step
rustCacheStep =
  GHA.namedAs "Initialize Cache" $
    cacheStep ["~/.cargo/registry", "~/.cargo/git", "target"] $
      os <> "-cargo-" <> hash
  where
    os = GHA.mkExpression "runner.os"
    hash = GHA.mkExpression "hashFiles('**/Cargo.lock')"
llvmCacheStep =
  GHA.namedAs "Initialize LLVM Cache" $
    cacheStep ["./llvm"] $
      GHA.mkExpression "runner.os" <> "-llvm-11"

data SlackNotification = ReportSuccess | ReportFailure String

data SlackNotificationProvider = SlackNotificationProvider {buildSuccessReportStep :: GHA.Step, buildFailureReportStep :: String -> GHA.Step}

type SlackNotifyContext = Reader SlackNotificationProvider

slackNotifySteps :: (Member SlackNotifyContext r) => SlackNotification -> Eff r [GHA.Step]
slackNotifySteps ReportSuccess = do
  provider <- ask
  pure [configureSlackNotification, buildSuccessReportStep provider]
slackNotifySteps (ReportFailure jobName) = do
  provider <- ask
  pure [configureSlackNotification, buildFailureReportStep provider jobName]

reportJobFailure :: (Member SlackNotifyContext r) => GHA.Job -> Eff r GHA.Job
reportJobFailure job = do
  reportSteps <- slackNotifySteps $ ReportFailure $ fromMaybe "<unknown job>" $ GHA.nameOf job
  pure $ GHA.jobModifySteps (<> fmap runOnFailure reportSteps) job

checkFormats :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkFormats precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Code Formats", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  GHA.namedAs "Running Check - Line Width" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptCodeFormCheck,
                  GHA.namedAs "Running Check - Debugging Weaks" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptVulnerabilitiesEliminator,
                  GHA.namedAs "Running Check - Trailing Newline for Source Code Files" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptTrailingNewlineChecker
                ]
        )

checkBaseLayer :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkBaseLayer precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Base Layer", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  GHA.namedAs "Building as Checking" $ GHA.actionStep "./.github/actions/checkbuild-baselayer" M.empty
                ]
        )

checkTools :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkTools precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Tools", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./tools"
                ]
        )

checkModules :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkModules precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Modules", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./modules"
                ]
        )

checkExamples :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkExamples precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Examples", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./examples"
                ]
        )

cliBuildStep, archiverBuildStep :: GHA.Step
cliBuildStep =
  GHA.namedAs "Build CLI" $
    GHA.workAt "./tools/cli" $
      GHA.runStep "cargo build"
archiverBuildStep =
  GHA.namedAs "Build archiver" $
    GHA.workAt "./tools/archiver" $
      GHA.runStep "cargo build"

setupBuilderEnv :: GHA.StepModifier
setupBuilderEnv =
  applyModifiers
    [ GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)",
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"
    ]

checkCradleWindows :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleWindows precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Windows)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  cliBuildStep,
                  GHA.namedAs "cargo check" $ integratedTestStep integratedTestNormalScript,
                  GHA.namedAs "cargo check for transparent-back" $ integratedTestStep integratedTestTransparentScript
                ]
        )
  where
    integratedTestStep = applyModifiers [GHA.env "VK_SDK_PATH" "", setupBuilderEnv] . GHA.runStep

    integratedTestNormalScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot test examples/image-plane -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"
    integratedTestTransparentScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot test examples/image-plane -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"

checkCradleMacos :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleMacos precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(macOS)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  cliBuildStep,
                  archiverBuildStep,
                  GHA.namedAs "Install requirements" $ GHA.runStep "brew install coreutils",
                  integratedTestStep
                ]
        )
  where
    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          GHA.env "VULKAN_SDK" "/Users",
          setupBuilderEnv,
          GHA.env "PERIDOT_CLI_ARCHIVER_PATH" $ GHA.mkExpression "format('{0}/tools/target/debug/peridot-archiver', github.workspace)"
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p mac 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

addPPAStep :: [String] -> GHA.Step
addPPAStep ppaList =
  GHA.namedAs "Add External PPA" $
    GHA.runStep $
      "sudo apt-add-repository -y " <> unwords ppaList

aptInstallStep :: [String] -> GHA.Step
aptInstallStep packages =
  GHA.namedAs "install apt packages" $
    GHA.runStep $
      "sub apt-get update && sudo apt-get install -y " <> unwords packages

checkCradleLinux :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleLinux precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Linux)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ addPPAStep ["ppa:pipewire-debian/pipewire-upstream"],
                  GHA.namedAs "Install extra packages" $ aptInstallStep ["libwayland-dev", "libpipewire-0.3-dev", "libspa-0.2-dev"],
                  checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  GHA.identifiedAs llvmCacheStepId llvmCacheStep,
                  llvmInstallStep,
                  cliBuildStep,
                  integratedTestStep
                ]
        )
  where
    llvmCacheStepId = "llvm-cache"
    llvmInstallStep =
      GHA.namedAs "Install LLVM" $
        GHA.actionStep "KyleMayes/install-llvm-action@v1" $
          M.fromList
            [ ("version", toJSON ("11" :: String)),
              ("cached", toJSON $ GHA.mkRefStepOutputExpression llvmCacheStepId "cache-hit")
            ]
    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          setupBuilderEnv
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleAndroid precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Android)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  applyModifiers
                    [ GHA.namedAs "Setup Rust for Android",
                      RustToolchainAction.useStable,
                      RustToolchainAction.forTarget "aarch64-linux-android"
                    ]
                    RustToolchainAction.step,
                  GHA.namedAs "Setup Java" $ SetupJavaAction.step "adopt" $ Just "17",
                  GHA.namedAs "install cargo-ndk" $ GHA.runStep "cargo install cargo-ndk",
                  cliBuildStep,
                  integratedTestStep
                ]
        )
  where
    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          setupBuilderEnv,
          GHA.env "NDK_PLATFORM_TARGET" "28"
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

reportSuccessJob :: (Member SlackNotifyContext r) => Eff r GHA.Job
reportSuccessJob = do
  reportSteps <- slackNotifySteps ReportSuccess
  pure
    $ applyModifiers
      [ GHA.namedAs "Report as Success",
        GHA.permit GHA.IDTokenPermission GHA.PermWrite
      ]
    $ GHA.job ([checkoutHeadStep, checkoutStep] <> reportSteps)

preconditionOutputHasChanges, preconditionOutputHasWorkflowChanges :: String
preconditionOutputHasChanges = GHA.mkExpression $ GHA.mkNeedsOutputPath "preconditions" "has_code_changes" <> " == 1"
preconditionOutputHasWorkflowChanges = GHA.mkExpression $ GHA.mkNeedsOutputPath "preconditions" "has_workflow_changes" <> " == 1"

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

preconditions :: GHA.Job
preconditions =
  applyModifiers
    [ GHA.namedAs "Preconditions",
      preconditionBeginTimestampOutputDef,
      GHA.jobOutput "has_code_changes" $ GHA.mkRefStepOutputExpression "fileck" "has_code_changes",
      GHA.jobOutput "has_workflow_changes" $ GHA.mkRefStepOutputExpression "fileck" "has_workflow_changes"
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
            <> queryString
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
    apiRequest = "curl -s -H \"Authorization: Bearer " <> secretGitHubTokenExpr <> "\" -X POST -d \"$POSTDATA\" https://api.github.com/graphql"

integrityTest :: GHA.Workflow
integrityTest = run $ runReader slackNotifyProvider $ do
  let preconditions' = M.singleton "preconditions" preconditions
  checkFormats' <- M.singleton "check-formats" <$> checkFormats preconditionOutputHasChanges
  checkBaseLayer' <- M.singleton "check-baselayer" <$> checkBaseLayer preconditionOutputHasChanges
  checkTools' <- M.singleton "check-tools" <$> checkTools preconditionOutputHasChanges
  checkModules' <- M.singleton "check-modules" <$> checkModules preconditionOutputHasChanges
  checkExamples' <- M.singleton "check-examples" <$> checkExamples preconditionOutputHasChanges
  checkCradleWindows' <- M.singleton "check-cradle-windows" <$> checkCradleWindows preconditionOutputHasChanges
  checkCradleMacos' <- M.singleton "check-cradle-macos" <$> checkCradleMacos preconditionOutputHasChanges
  checkCradleLinux' <- M.singleton "check-cradle-linux" <$> checkCradleLinux preconditionOutputHasChanges
  checkCradleAndroid' <- M.singleton "check-cradle-android" <$> checkCradleAndroid preconditionOutputHasChanges
  reportSuccessJob' <- M.singleton "report-success" <$> reportSuccessJob

  let prTrigger = GHA.filterType "opened" $ GHA.filterType "synchronize" GHA.workflowPullRequestTrigger
      checkJobs =
        [ checkFormats',
          checkBaseLayer'
            ~=> [checkTools', checkModules' ~=> checkExamples']
            ~=> [checkCradleWindows', checkCradleMacos', checkCradleLinux', checkCradleAndroid']
        ]
  pure
    $ GHA.buildWorkflow
      [ GHA.namedAs "Integrity Check",
        GHA.workflowConcurrency $ GHA.ConcurrentCancelledGroup $ GHA.mkExpression "github.ref",
        GHA.workflowJobs $ preconditions' ~=> checkJobs ~=> reportSuccessJob'
      ]
    $ GHA.onPullRequest prTrigger

weeklyIntegrityTest :: GHA.Workflow
weeklyIntegrityTest = run $ runReader weeklySlackNotifyProvider $ do
  let preconditions' =
        M.singleton "preconditions" $
          applyModifiers [GHA.namedAs "Preconditions", preconditionBeginTimestampOutputDef] $
            GHA.job [preconditionRecordBeginTimeStamp]
  checkFormats' <- M.singleton "check-formats" <$> checkFormats "true"
  checkBaseLayer' <- M.singleton "check-baselayer" <$> checkBaseLayer "true"
  checkTools' <- M.singleton "check-tools" <$> checkTools "true"
  checkModules' <- M.singleton "check-modules" <$> checkModules "true"
  checkExamples' <- M.singleton "check-examples" <$> checkExamples "true"
  checkCradleWindows' <- M.singleton "check-cradle-windows" <$> checkCradleWindows "true"
  checkCradleMacos' <- M.singleton "check-cradle-macos" <$> checkCradleMacos "true"
  checkCradleLinux' <- M.singleton "check-cradle-linux" <$> checkCradleLinux "true"
  checkCradleAndroid' <- M.singleton "check-cradle-android" <$> checkCradleAndroid "true"
  reportSuccessJob' <- M.singleton "report-success" <$> reportSuccessJob

  let checkJobs =
        [ checkFormats',
          checkBaseLayer'
            ~=> [checkTools', checkModules' ~=> checkExamples']
            ~=> [checkCradleWindows', checkCradleMacos', checkCradleLinux', checkCradleAndroid']
        ]
  pure
    $ GHA.buildWorkflow
      [ GHA.namedAs "Integrity Check (Weekly)",
        GHA.workflowJobs $ preconditions' ~=> checkJobs ~=> reportSuccessJob'
      ]
    $ GHA.scheduled "0 12 * * wed"

main :: IO ()
main = do
  LBS8.writeFile "./integrity-test.yml" $ encode integrityTest
  LBS8.writeFile "./weekly-integrity-test.yml" $ encode weeklyIntegrityTest
