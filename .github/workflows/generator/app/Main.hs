{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CustomAction.CheckBuildSubdirectory qualified as CheckBuildSubdirAction
import CustomAction.CodeFormChecker qualified as CodeFormCheckerAction
import CustomAction.PostCINotifications qualified as PostCINotificationsAction
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Yaml (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Workflow.GitHub.Actions as GHA
import Workflow.GitHub.Actions.Predefined.AWS.ConfigureCredentials qualified as AWSConfigureCredentials
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction

pullRequestHeadHashExpr, pullRequestBaseHashExpr, pullRequestNumberExpr, pullRequestTitleExpr :: String
pullRequestHeadHashExpr = GHA.mkExpression "github.event.pull_request.head.sha"
pullRequestBaseHashExpr = GHA.mkExpression "github.event.pull_request.base.sha"
pullRequestNumberExpr = GHA.mkExpression "github.event.number"
pullRequestTitleExpr = GHA.mkExpression "github.event.pull_request.title"

repositoryOwnerLoginExpr, repositoryNameExpr :: String
repositoryOwnerLoginExpr = GHA.mkExpression "github.event.repository.owner.login"
repositoryNameExpr = GHA.mkExpression "github.event.repository.name"

secretGitHubTokenExpr :: String
secretGitHubTokenExpr = GHA.mkExpression "secrets.GITHUB_TOKEN"

depends :: [String] -> GHA.Job -> GHA.Job
depends deps x = x {GHA.needs = Just (fromMaybe [] (GHA.needs x) <> deps)}

applyModifiers :: (Foldable f) => f (a -> a) -> a -> a
applyModifiers = flip $ foldl (flip ($))

runOnFailure :: (Conditional c) => c -> c
runOnFailure = withCondition "failure()"

configureSlackNotification :: GHA.Step
configureSlackNotification =
  namedAs "Configure for Slack Notification" $
    AWSConfigureCredentials.step $
      AWSConfigureCredentials.defaultParams
        { AWSConfigureCredentials.awsRegion = Just "ap-northeast-1",
          AWSConfigureCredentials.roleToAssume = Just "arn:aws:iam::208140986057:role/GHALambdaInvoker"
        }

preconditionRecordBeginTimeStamp :: GHA.Step
preconditionRecordBeginTimeStamp =
  applyModifiers [GHA.identifiedAs "begintime", GHA.namedAs "Getting begintime"] $
    GHA.runStep "echo \"begintime=$(date +%s)\" >> $GITHUB_OUTPUT"

preconditionBeginTimestampOutputDef :: GHA.Job -> GHA.Job
preconditionBeginTimestampOutputDef = GHA.jobOutput "begintime" $ GHA.mkRefStepOutputExpression "begintime" "begintime"

checkoutStep, checkoutHeadStep :: GHA.Step
checkoutStep = Checkout.step Checkout.defaultParams
checkoutHeadStep = namedAs "Checking out (HEAD commit)" $ Checkout.step params
  where
    params = Checkout.defaultParams {Checkout.ref = Just pullRequestHeadHashExpr}

cacheStep :: [String] -> String -> GHA.Step
cacheStep paths key = GHA.step {GHA.uses = Just "actions/cache@v2", GHA.with = M.fromList [("path", toJSON $ intercalate "\n" paths), ("key", toJSON key)]}

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

slackNotifySteps :: SlackNotificationProvider -> SlackNotification -> [GHA.Step]
slackNotifySteps provider ReportSuccess = [configureSlackNotification, buildSuccessReportStep provider]
slackNotifySteps provider (ReportFailure jobName) = [configureSlackNotification, buildFailureReportStep provider jobName]

checkFormats :: SlackNotificationProvider -> String -> GHA.Job
checkFormats provider precondition =
  applyModifiers [GHA.namedAs "Code Formats", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  GHA.namedAs "Running Check - Line Width" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptCodeFormCheck,
                  GHA.namedAs "Running Check - Debugging Weaks" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptVulnerabilitiesEliminator,
                  GHA.namedAs "Running Check - Trailing Newline for Source Code Files" $ CodeFormCheckerAction.step CodeFormCheckerAction.ScriptTrailingNewlineChecker
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-formats"))
      )

checkBaseLayer :: SlackNotificationProvider -> String -> GHA.Job
checkBaseLayer provider precondition =
  applyModifiers [GHA.namedAs "Base Layer", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  GHA.namedAs "Building as Checking" $ GHA.actionStep "./.github/actions/checkbuild-baselayer" M.empty
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-baselayer"))
      )

checkTools :: SlackNotificationProvider -> String -> GHA.Job
checkTools provider precondition =
  applyModifiers [GHA.namedAs "Tools", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./tools"
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-tools"))
      )

checkModules :: SlackNotificationProvider -> String -> GHA.Job
checkModules provider precondition =
  applyModifiers [GHA.namedAs "Modules", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./modules"
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-modules"))
      )

checkExamples :: SlackNotificationProvider -> String -> GHA.Job
checkExamples provider precondition =
  applyModifiers [GHA.namedAs "Examples", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  CheckBuildSubdirAction.step "./examples"
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-examples"))
      )

cliBuildStep, archiverBuildStep :: GHA.Step
cliBuildStep = namedAs "Build CLI" $ GHA.workAt "./tools/cli" $ GHA.runStep "cargo build"
archiverBuildStep = namedAs "Build archiver" $ GHA.workAt "./tools/archiver" $ GHA.runStep "cargo build"

setupBuilderEnv :: GHA.Step -> GHA.Step
setupBuilderEnv =
  applyModifiers
    [ GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)",
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"
    ]

checkCradleWindows :: SlackNotificationProvider -> String -> GHA.Job
checkCradleWindows provider precondition =
  applyModifiers [GHA.namedAs "Cradle(Windows)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  cliBuildStep,
                  GHA.namedAs "cargo check" $ integratedTestStep integratedTestNormalScript,
                  GHA.namedAs "cargo check for transparent-back" $ integratedTestStep integratedTestTransparentScript
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-cradle-windows"))
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

checkCradleMacos :: SlackNotificationProvider -> String -> GHA.Job
checkCradleMacos provider precondition =
  applyModifiers [GHA.namedAs "Cradle(macOS)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  cliBuildStep,
                  archiverBuildStep,
                  GHA.namedAs "Install requirements" $ GHA.runStep "brew install coreutils",
                  integratedTestStep
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-cradle-macos"))
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

checkCradleLinux :: SlackNotificationProvider -> String -> GHA.Job
checkCradleLinux provider precondition =
  applyModifiers [GHA.namedAs "Cradle(Linux)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
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
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-cradle-linux"))
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

checkCradleAndroid :: SlackNotificationProvider -> String -> GHA.Job
checkCradleAndroid provider precondition =
  applyModifiers [GHA.namedAs "Cradle(Android)", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job
      ( ( withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  RustToolchainAction.step $
                    RustToolchainAction.defaultParams
                      { RustToolchainAction.toolchain = Just "stable",
                        RustToolchainAction.target = Just "aarch64-linux-android"
                      },
                  GHA.namedAs "Setup Java" $
                    GHA.actionStep "actions/setup-java@v3" $
                      M.fromList
                        [ ("distribution", toJSON ("adopt" :: String)),
                          ("java-version", toJSON ("17" :: String))
                        ],
                  GHA.namedAs "install cargo-ndk" $ GHA.runStep "cargo install cargo-ndk",
                  cliBuildStep,
                  integratedTestStep
                ]
        )
          <> (runOnFailure <$> slackNotifySteps provider (ReportFailure "check-cradle-android"))
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

reportSuccessJob :: SlackNotificationProvider -> GHA.Job
reportSuccessJob provider =
  applyModifiers [GHA.namedAs "Report as Success", GHA.permit GHA.IDTokenPermission GHA.PermWrite] $
    GHA.job $
      [checkoutHeadStep, checkoutStep] <> slackNotifySteps provider ReportSuccess

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
          PostCINotificationsAction.mode =
            PostCINotificationsAction.DiffMode
              { PostCINotificationsAction.diffHeadSHA = pullRequestHeadHashExpr,
                PostCINotificationsAction.diffBaseSHA = pullRequestBaseHashExpr,
                PostCINotificationsAction.diffPRNumber = pullRequestNumberExpr,
                PostCINotificationsAction.diffPRTitle = pullRequestTitleExpr
              }
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

requireJobsBefore :: Map String Job -> Map String Job -> Map String Job
requireJobsBefore preJobs afterJobs = (depends requiredJobNames <$> afterJobs) <> preJobs
  where
    requiredJobNames = M.keys preJobs

(~=>) :: Map String Job -> Map String Job -> Map String Job
(~=>) = requireJobsBefore

concurrent :: [Map String Job] -> Map String Job
concurrent = mconcat

infixl 5 ~=>

integrityTest :: GHA.Workflow
integrityTest =
  GHA.buildWorkflow
    [ GHA.namedAs "Integrity Check",
      GHA.workflowConcurrency $ GHA.ConcurrentCancelledGroup $ GHA.mkExpression "github.ref",
      GHA.workflowJobs $ preconditions' ~=> checkJobs ~=> reportSuccessJob'
    ]
    $ GHA.OnEventsDetailed (Just prTrigger) Nothing Nothing
  where
    prTrigger = GHA.filterType "opened" $ GHA.filterType "synchronize" GHA.workflowPullRequestTrigger
    preconditions' = M.singleton "preconditions" preconditions
    checkFormats' = M.singleton "check-formats" $ checkFormats slackNotifyProvider preconditionOutputHasChanges
    checkBaseLayer' = M.singleton "check-baselayer" $ checkBaseLayer slackNotifyProvider preconditionOutputHasChanges
    checkTools' = M.singleton "check-tools" $ checkTools slackNotifyProvider preconditionOutputHasChanges
    checkModules' = M.singleton "check-modules" $ checkModules slackNotifyProvider preconditionOutputHasChanges
    checkExamples' = M.singleton "check-examples" $ checkExamples slackNotifyProvider preconditionOutputHasChanges
    checkCradleWindows' = M.singleton "check-cradle-windows" $ checkCradleWindows slackNotifyProvider preconditionOutputHasChanges
    checkCradleMacos' = M.singleton "check-cradle-macos" $ checkCradleMacos slackNotifyProvider preconditionOutputHasChanges
    checkCradleLinux' = M.singleton "check-cradle-linux" $ checkCradleLinux slackNotifyProvider preconditionOutputHasChanges
    checkCradleAndroid' = M.singleton "check-cradle-android" $ checkCradleAndroid slackNotifyProvider preconditionOutputHasChanges
    reportSuccessJob' = M.singleton "report-success" $ reportSuccessJob slackNotifyProvider
    checkJobs =
      concurrent
        [ checkFormats',
          checkBaseLayer'
            ~=> concurrent [checkTools', checkModules' ~=> checkExamples']
            ~=> concurrent [checkCradleWindows', checkCradleMacos', checkCradleLinux', checkCradleAndroid']
        ]

main :: IO ()
main = do
  LBS8.writeFile "./integrity-test.yml" $ encode integrityTest
