module IntegrityTest.Shared
  ( pullRequestNumberExpr,
    preconditionRecordBeginTimeStamp,
    preconditionBeginTimestampOutputDef,
    checkFormats,
    checkBaseLayer,
    checkTools,
    checkModules,
    checkExamples,
    checkCradleWindows,
    checkCradleMacos,
    checkCradleLinux,
    checkCradleAndroid,
    reportSuccessJob,
  )
where

import Control.Eff (Eff, Member)
import CustomAction.CheckBuildSubdirectory qualified as CheckBuildSubdirAction
import CustomAction.CodeFormChecker qualified as CodeFormCheckerAction
import Data.Aeson (ToJSON (toJSON))
import Data.Map qualified as M
import SlackNotification (SlackNotification (ReportSuccess), SlackNotifyContext, reportJobFailure, slackNotifySteps)
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Cache qualified as CacheAction
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction
import Workflow.GitHub.Actions.Predefined.SetupJava qualified as SetupJavaAction

pullRequestHeadHashExpr, pullRequestNumberExpr :: String
pullRequestHeadHashExpr = GHA.mkExpression "github.event.pull_request.head.sha"
pullRequestNumberExpr = GHA.mkExpression "github.event.number"

preconditionRecordBeginTimeStamp :: GHA.Step
preconditionRecordBeginTimeStamp =
  applyModifiers [GHA.identifiedAs "begintime", GHA.namedAs "Getting begintime"] $
    GHA.runStep "echo \"begintime=$(date +%s)\" >> $GITHUB_OUTPUT"

preconditionBeginTimestampOutputDef :: GHA.Job -> GHA.Job
preconditionBeginTimestampOutputDef = GHA.jobOutput "begintime" $ GHA.mkRefStepOutputExpression "begintime" "begintime"

checkoutStep, checkoutHeadStep :: GHA.Step
checkoutStep = GHA.namedAs "Checking out" $ Checkout.step Nothing
checkoutHeadStep = GHA.namedAs "Checking out (HEAD commit)" $ Checkout.step $ Just pullRequestHeadHashExpr

rustCacheStep, llvmCacheStep :: GHA.Step
rustCacheStep =
  GHA.namedAs "Initialize Cache" $
    CacheAction.step ["~/.cargo/registry", "~/.cargo/git", "target"] $
      os <> "-cargo-" <> hash
  where
    os = GHA.mkExpression "runner.os"
    hash = GHA.mkExpression "hashFiles('**/Cargo.lock')"
llvmCacheStep =
  GHA.namedAs "Initialize LLVM Cache" $
    CacheAction.step ["./llvm"] $
      GHA.mkExpression "runner.os" <> "-llvm-11"

checkFormats :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkFormats precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Code Formats"] $
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
    applyModifiers [GHA.namedAs "Base Layer"] $
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
    applyModifiers [GHA.namedAs "Tools"] $
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
    applyModifiers [GHA.namedAs "Modules"] $
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
    applyModifiers [GHA.namedAs "Examples"] $
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

withBuilderEnv :: GHA.StepModifier
withBuilderEnv =
  applyModifiers
    [ GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)",
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"
    ]

checkCradleWindows :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleWindows precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Windows)", GHA.jobRunsOn ["windows-latest"]] $
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
    integratedTestStep = applyModifiers [GHA.env "VK_SDK_PATH" "", withBuilderEnv] . GHA.runStep

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
    applyModifiers [GHA.namedAs "Cradle(macOS)", GHA.jobRunsOn ["macos-latest"]] $
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
          withBuilderEnv,
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
      "sudo apt-get update && sudo apt-get install -y " <> unwords packages

checkCradleLinux :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleLinux precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Linux)"] $
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
          withBuilderEnv
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: (Member SlackNotifyContext r) => String -> Eff r GHA.Job
checkCradleAndroid precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Cradle(Android)"] $
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
          withBuilderEnv,
          GHA.env "NDK_PLATFORM_TARGET" "28"
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

reportSuccessJob :: (Member SlackNotifyContext r) => Eff r GHA.Job
reportSuccessJob = do
  reportSteps <- slackNotifySteps ReportSuccess
  pure $
    applyModifiers [GHA.namedAs "Report as Success", GHA.grantWritable GHA.IDTokenPermission] $
      -- NotificationでHeadの情報見るっぽくて必要そう
      GHA.job ([checkoutStep, checkoutHeadStep] <> reportSteps)