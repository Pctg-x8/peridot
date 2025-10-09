{-# LANGUAGE MultilineStrings #-}

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

import Data.Function ((&))
import Data.Functor ((<&>))
import SlackNotification (SlackReportContext(..), reportJobFailure)
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Cache qualified as CacheAction
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.InstallLLVM qualified as InstallLLVMAction
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction
import Workflow.GitHub.Actions.Predefined.SetupJava qualified as SetupJavaAction

pullRequestHeadHashExpr, pullRequestNumberExpr :: String
pullRequestHeadHashExpr = GHA.mkExpression "github.event.pull_request.head.sha"
pullRequestNumberExpr = GHA.mkExpression "github.event.number"

preconditionRecordBeginTimeStamp :: GHA.Step
preconditionRecordBeginTimeStamp =
  GHA.identifiedAs "begintime" $
    GHA.namedAs "Getting begintime" $
      GHA.runStep "echo \"begintime=$(date +%s)\" >> $GITHUB_OUTPUT"

preconditionBeginTimestampOutputDef :: GHA.Job -> GHA.Job
preconditionBeginTimestampOutputDef = GHA.jobForwardingStepOutput "begintime" "begintime"

checkoutStep, checkoutHeadStep :: GHA.Step
checkoutStep = GHA.namedAs "Checking out" $ Checkout.step Nothing & Checkout.submodules Checkout.SubmodulesRecursive
checkoutHeadStep = GHA.namedAs "Checking out (HEAD commit)" $ Checkout.step (Just pullRequestHeadHashExpr) & Checkout.submodules Checkout.SubmodulesRecursive

-- あとでlatest自動取得とかしたいけど面倒だから一旦これでいいや
setupCargoOutputTranslatorStep :: GHA.Step
setupCargoOutputTranslatorStep = GHA.namedAs "Setup cargo-json-gha-translator" $
  GHA.runStep "mkdir -p $HOME/.local/bin && curl -o $HOME/.local/bin/cargo-json-gha-translator -L https://github.com/Pctg-x8/cargo-json-gha-translator/releases/download/v0.1.3/cargo-json-gha-translator && chmod +x $HOME/.local/bin/cargo-json-gha-translator"

rustCacheStep, llvmCacheStep :: GHA.Step
rustCacheStep =
  GHA.namedAs "Initialize Cache" $
    CacheAction.step ["~/.cargo/registry", "~/.cargo/git", "target"] $
      GHA.runnerOs <> "-cargo-" <> hash
  where
    hash = GHA.mkExpression "hashFiles('**/Cargo.lock')"
llvmCacheStep =
  GHA.namedAs "Initialize LLVM Cache" $ CacheAction.step ["./llvm"] $ GHA.runnerOs <> "-llvm-11"

checkFormats :: SlackReportContext m => Functor m => String -> m GHA.Job
checkFormats precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Code Formats"] $
      GHA.job
        ( GHA.withCondition precondition
            <$> [ checkoutHeadStep,
                  checkoutStep,
                  rustCacheStep,
                  setupCargoOutputTranslatorStep,
                  GHA.namedAs "Running Rustfmt" $ GHA.runStep "cargo fmt -- --check",
                  GHA.namedAs "Running Clippy" $ GHA.runStep "set -o pipefail; cargo clippy --all-features --all-targets --message-format=json | $HOME/.local/bin/cargo-json-gha-translator",
                  GHA.namedAs "Running Check - Trailing Newline for Source Code Files" $
                    GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/trailing_newline_checker.sh"
                ]
        )

checkBaseLayer :: SlackReportContext m => Functor m => String -> m GHA.Job
checkBaseLayer precondition = reportJobFailure $ GHA.namedAs "Base Layer" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ checkoutHeadStep,
              checkoutStep,
              rustCacheStep,
              setupCargoOutputTranslatorStep,
              GHA.namedAs "check" $
                GHA.runStep "set -o pipefail; cargo check --package peridot --verbose --features=bedrock/VK_EXT_debug_report --message-format=json | $HOME/.local/bin/cargo-json-gha-translator",
              GHA.namedAs "check(mt)" $
                GHA.runStep "set -o pipefail; cargo check --package peridot --verbose --features=bedrock/VK_EXT_debug_report,mt --message-format=json | $HOME/.local/bin/cargo-json-gha-translator"
            ]

checkTools :: SlackReportContext m => Functor m => String -> m GHA.Job
checkTools precondition = reportJobFailure $ GHA.namedAs "Tools" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition <$>
        [ checkoutHeadStep
        , checkoutStep
        , rustCacheStep
        , setupCargoOutputTranslatorStep
        , GHA.namedAs "check" $
            GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
              & GHA.workAt "tools"
        ]

checkModules :: SlackReportContext m => Functor m => String -> m GHA.Job
checkModules precondition = reportJobFailure $ GHA.namedAs "Modules" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition <$>
        [ checkoutHeadStep
        , checkoutStep
        , rustCacheStep
        , setupCargoOutputTranslatorStep
        , GHA.namedAs "check" $
            GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
              & GHA.workAt "modules"
        ]

checkExamples :: SlackReportContext m => Functor m => String -> m GHA.Job
checkExamples precondition = reportJobFailure $ GHA.namedAs "Examples" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition <$>
        [ checkoutHeadStep
        , checkoutStep
        , rustCacheStep
        , setupCargoOutputTranslatorStep
        , GHA.namedAs "check" $
            GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
              & GHA.workAt "examples"
        ]

cliBuildStep, archiverBuildStep :: GHA.Step
cliBuildStep = GHA.namedAs "Build CLI" $ GHA.workAt "./tools/cli" $ GHA.runStep "cargo build"
archiverBuildStep = GHA.namedAs "Build archiver" $ GHA.workAt "./tools/archiver" $ GHA.runStep "cargo build"

withBuilderEnv :: (GHA.HasEnvironmentVariables e) => e -> e
withBuilderEnv = setCradleBase . setBuiltinAssetsPath . setLibrarySearchPaths
  where
    setCradleBase = GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)"
    setBuiltinAssetsPath =
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"
    setLibrarySearchPaths = GHA.env "LD_LIBRARY_PATH" $ GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/RelWithDebInfo/lib:{1}', github.workspace, env.LD_LIBRARY_PATH)"

checkCradleWindows :: SlackReportContext m => Functor m => String -> m GHA.Job
checkCradleWindows precondition =
  reportJobFailure $ GHA.namedAs "Cradle(Windows)" $ GHA.jobRunsOn ["windows-latest"] $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ checkoutHeadStep,
              checkoutStep,
              rustCacheStep,
              cliBuildStep,
              GHA.namedAs "cargo check" $ integratedTestStep integratedTestNormalScript,
              GHA.namedAs "cargo check for transparent-back" $ integratedTestStep integratedTestTransparentScript
            ]

    integratedTestStep = GHA.env "VK_SDK_PATH" "" . withBuilderEnv . GHA.runStep

    integratedTestNormalScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot test examples/image-plane -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"
    integratedTestTransparentScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot test examples/image-plane -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"

checkCradleMacos :: SlackReportContext m => Functor m => String -> m GHA.Job
checkCradleMacos precondition =
  reportJobFailure $ GHA.namedAs "Cradle(macOS)" $ GHA.jobRunsOn ["macos-latest"] $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ checkoutHeadStep,
              checkoutStep,
              rustCacheStep,
              cliBuildStep,
              archiverBuildStep,
              GHA.namedAs "Install requirements" $ GHA.runStep "brew install coreutils",
              integratedTestStep
            ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          GHA.env "VULKAN_SDK" "/Users",
          withBuilderEnv,
          GHA.env "PERIDOT_CLI_ARCHIVER_PATH" $
            GHA.mkExpression "format('{0}/tools/target/debug/peridot-archiver', github.workspace)"
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p mac 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

addPPAStep :: [String] -> GHA.Step
addPPAStep ppaList = GHA.namedAs "Add External PPA" $ GHA.runStep $ "sudo apt-add-repository -y " <> unwords ppaList

aptInstallStep :: [String] -> GHA.Step
aptInstallStep packages =
  GHA.namedAs "install apt packages" $
    GHA.runStep $
      "sudo apt-get update && sudo apt-get install -y " <> unwords packages

checkCradleLinux :: SlackReportContext m => Functor m => String -> m GHA.Job
checkCradleLinux precondition = reportJobFailure $ GHA.namedAs "Cradle(Linux)" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ addPPAStep ["ppa:pipewire-debian/pipewire-upstream"],
              GHA.namedAs "Install extra packages" $
                aptInstallStep ["libwayland-dev", "libpipewire-0.3-dev", "libspa-0.2-dev"],
              checkoutHeadStep,
              checkoutStep,
              rustCacheStep,
              GHA.identifiedAs llvmCacheStepId llvmCacheStep,
              llvmInstallStep,
              cliBuildStep,
              integratedTestStep
            ]

    llvmCacheStepId = "llvm-cache"
    llvmInstallStep =
      GHA.namedAs "Install LLVM" $
        InstallLLVMAction.step "11"
          & InstallLLVMAction.isCached (CacheAction.refCacheHit llvmCacheStepId)
    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          withBuilderEnv
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: SlackReportContext m => Functor m => String -> m GHA.Job
checkCradleAndroid precondition = reportJobFailure $ GHA.namedAs "Cradle(Android)" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ checkoutHeadStep,
              checkoutStep,
              rustCacheStep,
              GHA.namedAs "Setup Rust for Android" $
                RustToolchainAction.step
                  & RustToolchainAction.useStable
                  & RustToolchainAction.forTarget "aarch64-linux-android",
              GHA.namedAs "Setup Java" $ SetupJavaAction.step "adopt" & SetupJavaAction.javaVersion "17",
              GHA.namedAs "install cargo-ndk" $ GHA.runStep "cargo install cargo-ndk",
              cliBuildStep,
              integratedTestStep
            ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          withBuilderEnv,
          GHA.env "NDK_PLATFORM_TARGET" "28"
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

reportSuccessJob :: SlackReportContext m => Functor m => m GHA.Job
reportSuccessJob =
  reportSuccessSteps <&> \reportSteps ->
    -- NotificationでHeadの情報見るっぽくて必要そう
    let steps = [checkoutStep, checkoutHeadStep] <> reportSteps
     in GHA.namedAs "Report as Success" $ GHA.grantWritable GHA.IDTokenPermission $ GHA.job steps
