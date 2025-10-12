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
    Step (..),
    flattenSteps,
    preBuildCDeps,
    checkoutStep,
    ccacheUbuntuVariants,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import SlackNotification (SlackReportContext (..), reportJobFailure)
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Cache qualified as CacheAction
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction
import Workflow.GitHub.Actions.Predefined.SetupJava qualified as SetupJavaAction

pullRequestHeadHashExpr, pullRequestNumberExpr :: String
pullRequestHeadHashExpr = GHA.mkExpression "github.event.pull_request.head.sha"
pullRequestNumberExpr = GHA.mkExpression "github.event.number"

data Step = Step GHA.Step | StepGroup [Step]

flattenSteps :: [Step] -> [GHA.Step]
flattenSteps (Step s : xs) = s : flattenSteps xs
flattenSteps (StepGroup ss : xs) = flattenSteps ss <> flattenSteps xs
flattenSteps [] = []

preconditionRecordBeginTimeStamp :: GHA.Step
preconditionRecordBeginTimeStamp =
  GHA.identifiedAs "begintime" $
    GHA.namedAs "Getting begintime" $
      GHA.runStep "echo \"begintime=$(date +%s)\" >> $GITHUB_OUTPUT"

preconditionBeginTimestampOutputDef :: GHA.Job -> GHA.Job
preconditionBeginTimestampOutputDef = GHA.jobForwardingStepOutput "begintime" "begintime"

checkoutStep, checkoutHeadStep :: GHA.Step
checkoutStep = GHA.namedAs "Checking out" $ Checkout.step Nothing & Checkout.submodules Checkout.SubmodulesRecursive
checkoutHeadStep =
  GHA.namedAs "Checking out (HEAD commit)" $
    Checkout.step (Just pullRequestHeadHashExpr) & Checkout.submodules Checkout.SubmodulesRecursive

-- あとでlatest自動取得とかしたいけど面倒だから一旦これでいいや
setupCargoOutputTranslatorStep :: GHA.Step
setupCargoOutputTranslatorStep =
  GHA.namedAs "Setup cargo-json-gha-translator" $
    GHA.runStep
      """
      mkdir -p $HOME/.local/bin && curl -o $HOME/.local/bin/cargo-json-gha-translator -L \\
        https://github.com/Pctg-x8/cargo-json-gha-translator/releases/download/v0.1.3/cargo-json-gha-translator
      chmod +x $HOME/.local/bin/cargo-json-gha-translator
      """

rustCacheStep :: GHA.Step
rustCacheStep =
  GHA.namedAs "Initialize Cache" $
    CacheAction.step ["~/.cargo/registry", "~/.cargo/git", "target", "tools/target"] key
      & CacheAction.restoreKeys [keyPrefix]
  where
    keyPrefix = GHA.runnerOs <> "-cargo-"
    key = keyPrefix <> GHA.mkExpression "hashFiles('**/*.rs', '**/Cargo.toml')"

cmake :: [String] -> String
cmake args = unwords ("cmake" : args)

data CCachePlatformVariants = CCachePlatformVariants
  { ccInstallStep :: Step,
    ccCacheDirectoryPath :: String,
    ccCommandPrelude :: Maybe String
  }

ccacheUbuntuVariants, ccacheWindowsVariants, ccacheMacVariants :: CCachePlatformVariants
ccacheUbuntuVariants =
  CCachePlatformVariants
    { ccInstallStep = Step $ GHA.namedAs "Install ccache" $ GHA.runStep "sudo apt-get update && sudo apt-get install ccache",
      ccCacheDirectoryPath = "~/.cache/ccache",
      ccCommandPrelude = Nothing
    }
ccacheWindowsVariants =
  CCachePlatformVariants
    { ccInstallStep = Step $ GHA.namedAs "Install ccache" $ GHA.runStep "choco install ccache",
      ccCacheDirectoryPath = "~\\AppData\\Roaming\\ccache",
      ccCommandPrelude =
        Just
          """
          Install-Module Pscx -Scope CurrentUser -Force -Function Import-VisualStudioVars
          Import-VisualStudioVars -VisualStudioVersion 2022 -Architecture x64
          """
    }
ccacheMacVariants =
  CCachePlatformVariants
    { ccInstallStep = Step $ GHA.namedAs "Install ccache" $ GHA.runStep "brew install ccache",
      ccCacheDirectoryPath = "~/Library/Caches/ccache",
      ccCommandPrelude = Nothing
    }

preBuildCDeps :: CCachePlatformVariants -> Step
preBuildCDeps variants =
  StepGroup
    [ Step $
        GHA.namedAs "Cache ccache artifacts" $
          CacheAction.step [ccCacheDirectoryPath variants] (ccCachePrefix <> ccTargetHash)
            & CacheAction.restoreKeys [ccCachePrefix],
      ccInstallStep variants,
      Step $
        GHA.namedAs "Pre-build c deps(slang)" $
          GHA.runStep
            ( maybe "" (<> "\n") (ccCommandPrelude variants)
                <> cmake
                  [ "--preset",
                    "default",
                    "-G",
                    "Ninja",
                    "-DCMAKE_CXX_COMPILER_LAUNCHER=ccache",
                    "-DCMAKE_C_COMPILER_LAUNCHER=ccache",
                    "-DSLANG_ENABLE_SLANG_RHI=FALSE",
                    "-DSLANG_ENABLE_GFX=FALSE",
                    "-DSLANG_ENABLE_SLANGD=FALSE",
                    "-DSLANG_ENABLE_SLANGC=FALSE",
                    "-DSLANG_ENABLE_SLANGI=FALSE",
                    "-DSLANG_ENABLE_SLANGRT=FALSE",
                    "-DSLANG_ENABLE_TESTS=FALSE",
                    "-DSLANG_ENABLE_EXAMPLES=FALSE"
                  ]
                <> " && cmake --build --preset releaseWithDebugInfo"
            )
            & GHA.workAt "thirdparty/slang/source-repo",
      Step $
        GHA.namedAs "Pre-build c deps(ktx)" $
          GHA.runStep
            ( maybe "" (<> " && ") (ccCommandPrelude variants)
                <> cmake
                  [ ".",
                    "-B",
                    "build",
                    "-G",
                    "Ninja",
                    "-DCMAKE_CXX_COMPILER_LAUNCHER=ccache",
                    "-DCMAKE_C_COMPILER_LAUNCHER=ccache",
                    "-DKTX_FEATURE_TESTS=OFF",
                    "-DKTX_FEATURE_VK_UPLOAD=OFF",
                    "-DKTX_FEATURE_GL_UPLOAD=OFF",
                    "-DKTX_FEATURE_TOOLS=OFF"
                  ]
                <> " && cmake --build build"
            )
            & GHA.workAt "thirdparty/ktx/source-repo"
    ]
  where
    ccCachePrefix = GHA.runnerOs <> "-ccache-"
    ccTargetHash = GHA.mkExpression ("hashFiles(" <> intercalate ", " (map (\x -> "'" <> x <> "'") hashFileTargets) <> ")")
    hashFileTargets =
      [ "thirdparty/slang/source-repo/**/*.c",
        "thirdparty/slang/source-repo/**/*.h",
        "thirdparty/slang/source-repo/**/*.cpp",
        "thirdparty/slang/source-repo/**/*.hpp",
        "thirdparty/slang/source-repo/**/*.inl",
        "thirdparty/ktx/source-repo/**/*.c",
        "thirdparty/ktx/source-repo/**/*.h",
        "thirdparty/ktx/source-repo/**/*.cpp",
        "thirdparty/ktx/source-repo/**/*.hpp",
        "thirdparty/ktx/source-repo/**/*.inl"
      ]

cdepsEnvVars :: (GHA.HasEnvironmentVariables e) => e -> e
cdepsEnvVars =
  GHA.env "PERIDOT_BUILD_TP_SLANG_SKIP_CMAKE" "1"
    . GHA.env "PERIDOT_BUILD_TP_KTX_SKIP_CMAKE" "1"
    -- NinjaでビルドするとRelWithDebInfoじゃなくてReleaseに生成されるらしい
    . GHA.env "PERIDOT_BUILD_TP_SLANG_LIB_PATH" (GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Release/lib', github.workspace)")

checkFormats :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkFormats precondition =
  reportJobFailure $
    applyModifiers [GHA.namedAs "Code Formats", cdepsEnvVars] $
      GHA.job
        ( GHA.withCondition precondition
            <$> flattenSteps
              [ Step checkoutStep,
                Step rustCacheStep,
                preBuildCDeps ccacheUbuntuVariants,
                Step setupCargoOutputTranslatorStep,
                Step $ GHA.namedAs "Running Rustfmt" $ GHA.runStep "cargo fmt -- --check",
                Step $
                  GHA.namedAs "Running Clippy" $
                    GHA.runStep
                      """
                      set -o pipefail
                      cargo clippy --all-features --all-targets --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
                      """,
                Step $
                  GHA.namedAs "Running Check - Trailing Newline for Source Code Files" $
                    GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/trailing_newline_checker.sh"
              ]
        )

checkBaseLayer :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkBaseLayer precondition = reportJobFailure $ GHA.namedAs "Base Layer" $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> [ checkoutStep,
              rustCacheStep,
              setupCargoOutputTranslatorStep,
              GHA.namedAs "check" $
                GHA.runStep
                  """
                  set -o pipefail
                  cargo check --package peridot --verbose --features=bedrock/VK_EXT_debug_report --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
                  """,
              GHA.namedAs "check(mt)" $
                GHA.runStep
                  """
                  set -o pipefail
                  cargo check --package peridot --verbose --features=bedrock/VK_EXT_debug_report,mt --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
                  """
            ]

checkTools :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkTools precondition = reportJobFailure $ GHA.namedAs "Tools" $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheUbuntuVariants,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "check" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "tools"
          ]

checkModules :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkModules precondition = reportJobFailure $ GHA.namedAs "Modules" $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheUbuntuVariants,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "check" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "modules"
          ]

checkExamples :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkExamples precondition = reportJobFailure $ GHA.namedAs "Examples" $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheUbuntuVariants,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "check" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "examples"
          ]

cliBuildStep, archiverBuildStep :: GHA.Step
cliBuildStep = GHA.namedAs "Build CLI" $ GHA.workAt "./tools/cli" $ GHA.runStep "cargo build"
archiverBuildStep = GHA.namedAs "Build archiver" $ GHA.workAt "./tools/archiver" $ GHA.runStep "cargo build"

withBuilderEnv :: (GHA.HasEnvironmentVariables e) => e -> e
withBuilderEnv = setCradleBase . setBuiltinAssetsPath
  where
    setCradleBase = GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)"
    setBuiltinAssetsPath =
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"

setLibrarySearchPathsUnix :: (GHA.HasEnvironmentVariables e) => e -> e
setLibrarySearchPathsUnix = GHA.env "LD_LIBRARY_PATH" $ GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Release/lib:{0}/thirdparty/ktx/source-repo/build:{1}', github.workspace, env.LD_LIBRARY_PATH)"

checkCradleWindows :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleWindows precondition =
  reportJobFailure $ GHA.namedAs "Cradle(Windows)" $ GHA.jobRunsOn ["windows-latest"] $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheWindowsVariants,
            Step cliBuildStep,
            Step $
              GHA.namedAs "Copy thirdparty DLLs" $
                GHA.runStep
                  """
                  Copy-Item -Path thirdparty/ktx/source-repo/build/Debug/ktx.dll -Destination tools/target/debug/ktx.dll
                  Copy-Item -Path thirdparty/slang/source-repo/build/RelWithDebInfo/bin/slang.dll -Destination tools/target/debug/slang.dll
                  Copy-Item -Path thirdparty/slang/source-repo/build/RelWithDebInfo/bin/slang-glslang.dll -Destination tools/target/debug/slang-glslang.dll
                  """,
            Step $ GHA.namedAs "cargo check" $ integratedTestStep integratedTestNormalScript,
            Step $ GHA.namedAs "cargo check for transparent-back" $ integratedTestStep integratedTestTransparentScript
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

checkCradleMacos :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleMacos precondition =
  reportJobFailure $ GHA.namedAs "Cradle(macOS)" $ GHA.jobRunsOn ["macos-latest"] $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheMacVariants,
            Step cliBuildStep,
            Step archiverBuildStep,
            Step $ GHA.namedAs "Install requirements" $ GHA.runStep "brew install coreutils",
            Step integratedTestStep
          ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          GHA.env "VULKAN_SDK" "/Users",
          withBuilderEnv,
          GHA.env "PERIDOT_CLI_ARCHIVER_PATH" $
            GHA.mkExpression "format('{0}/tools/target/debug/peridot-archiver', github.workspace)",
          setLibrarySearchPathsUnix
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p mac 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

addPPAStep :: [String] -> GHA.Step
addPPAStep ppaList = GHA.namedAs "Add External PPA" $ GHA.runStep $ "sudo apt-add-repository -y " <> unwords ppaList

aptInstallStep :: [String] -> GHA.Step
aptInstallStep packages =
  GHA.namedAs "install apt packages" $
    GHA.runStep $
      "sudo apt-get update && sudo apt-get install -y " <> unwords packages

checkCradleLinux :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleLinux precondition = reportJobFailure $ GHA.namedAs "Cradle(Linux)" $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step $ addPPAStep ["ppa:pipewire-debian/pipewire-upstream"],
            Step $
              GHA.namedAs "Install extra packages" $
                aptInstallStep ["libwayland-dev", "libpipewire-0.3-dev", "libspa-0.2-dev"],
            Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheUbuntuVariants,
            Step cliBuildStep,
            Step integratedTestStep
          ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          withBuilderEnv,
          setLibrarySearchPathsUnix
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleAndroid precondition = reportJobFailure $ GHA.namedAs "Cradle(Android)" $ cdepsEnvVars $ GHA.job steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            preBuildCDeps ccacheUbuntuVariants,
            Step $
              GHA.namedAs "Setup Rust for Android" $
                RustToolchainAction.step
                  & RustToolchainAction.useStable
                  & RustToolchainAction.forTarget "aarch64-linux-android",
            Step $ GHA.namedAs "Setup Java" $ SetupJavaAction.step "adopt" & SetupJavaAction.javaVersion "17",
            Step $ GHA.namedAs "install cargo-ndk" $ GHA.runStep "cargo install cargo-ndk",
            Step cliBuildStep,
            Step integratedTestStep
          ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "cargo check",
          GHA.stepUseShell "bash",
          withBuilderEnv,
          GHA.env "NDK_PLATFORM_TARGET" "28",
          setLibrarySearchPathsUnix
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

reportSuccessJob :: (SlackReportContext m) => (Functor m) => m GHA.Job
reportSuccessJob =
  reportSuccessSteps <&> \reportSteps ->
    -- NotificationでHeadの情報見るっぽくて必要そう
    let steps = [checkoutStep, checkoutHeadStep] <> reportSteps
     in GHA.namedAs "Report as Success" $ GHA.grantWritable GHA.IDTokenPermission $ GHA.job steps
