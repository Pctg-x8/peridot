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
    disableAPTManualUpdateStep,
    RunnerVariant (..),
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import SlackNotification (SlackReportContext (..), reportJobFailure, reportSuccessSteps)
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Cache qualified as CacheAction
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.Rust.Toolchain qualified as RustToolchainAction
import Workflow.GitHub.Actions.Predefined.SetupJava qualified as SetupJavaAction

pullRequestNumberExpr :: String
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

checkoutStep :: GHA.Step
checkoutStep = GHA.namedAs "Checking out" $ Checkout.step Nothing & Checkout.submodules Checkout.SubmodulesRecursive

-- あとでlatest自動取得とかしたいけど面倒だから一旦これでいいや
setupCargoOutputTranslatorStep :: GHA.Step
setupCargoOutputTranslatorStep =
  GHA.namedAs "Setup cargo-json-gha-translator" $
    GHA.runStep
      """
      mkdir -p $HOME/.local/bin
      curl -o $HOME/.local/bin/cargo-json-gha-translator -L https://github.com/Pctg-x8/cargo-json-gha-translator/releases/download/v0.1.3/cargo-json-gha-translator
      chmod +x $HOME/.local/bin/cargo-json-gha-translator
      """

-- https://github.com/actions/runner-images/issues/10977
disableAPTManualUpdateStep :: GHA.Step
disableAPTManualUpdateStep =
  GHA.namedAs "Disable man-db triggers for apt-get" $
    GHA.runStep
      """
      echo \"set man-db/auto-update false\" | sudo debconf-communicate
      sudo dpkg-reconfigure man-db
      """

rustCacheStep :: GHA.Step
rustCacheStep =
  GHA.namedAs "Initialize Cache" $
    CacheAction.step ["~/.cargo/registry", "~/.cargo/git", "target", "tools/target", "examples/**/target", "cradle/**/target"] key
      & CacheAction.restoreKeys [keyPrefix]
  where
    keyPrefix = GHA.runnerOs <> "-cargo-"
    key = keyPrefix <> GHA.mkExpression "hashFiles('**/*.rs', '**/Cargo.toml')"

cmake :: [String] -> String
cmake args = unwords ("cmake" : args)

data RunnerVariant = RunnerVariantWindows | RunnerVariantMac | RunnerVariantUbuntu

preBuildCDeps :: RunnerVariant -> Step
preBuildCDeps variant =
  StepGroup
    [ Step $
        GHA.namedAs "Cache ccache artifacts" $
          CacheAction.step [cacheDirectoryPath] (ccCachePrefix <> ccTargetHash)
            & CacheAction.restoreKeys [ccCachePrefix],
      installStep,
      Step $
        GHA.namedAs "Pre-build c deps(slang)" $
          GHA.runStep
            ( maybe "" (<> "\n") commandPrelude
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
                <> " && cmake --build --preset debug"
            )
            & GHA.workAt "thirdparty/slang/source-repo",
      Step $
        GHA.namedAs "Pre-build c deps(ktx)" $
          GHA.runStep
            ( maybe "" (<> "\n") commandPrelude
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
                <> maybe "" ("\n" <>) commandPostKtx
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

    cacheDirectoryPath = case variant of
      RunnerVariantUbuntu -> "~/.cache/ccache"
      RunnerVariantWindows -> "~\\AppData\\Local\\ccache"
      RunnerVariantMac -> "~/Library/Caches/ccache"
    installStep = case variant of
      RunnerVariantUbuntu -> Step $ GHA.namedAs "Install ccache" $ GHA.runStep "sudo apt-get update && sudo apt-get install ccache"
      RunnerVariantWindows -> Step $ GHA.namedAs "Install ccache" $ GHA.runStep "choco install ccache"
      RunnerVariantMac -> Step $ GHA.namedAs "Install ccache" $ GHA.runStep "brew install ccache"
    commandPrelude = case variant of
      RunnerVariantWindows ->
        Just
          -- Pscx 4.0.0じゃないとvs2022のサポートがないのでPrereleaseを有効にする（TODO: 4.0.0正式リリースが来たら消す）
          """
          Install-Module Pscx -Scope CurrentUser -Force -AllowClobber -AllowPrerelease
          Import-VisualStudioVars -VisualStudioVersion 2022 -Architecture x64
          """
      _ -> Nothing
    commandPostKtx = case variant of
      RunnerVariantWindows ->
        -- NinjaだとどうやらImport Libraryが作られないらしいので自前で作る（ktxのみ？）
        Just
          """
          echo \"EXPORTS\" | Set-Content build\\Debug\\ktx.def
          dumpbin /exports build\\Debug\\ktx.dll | Select-Object -Skip 19 | ForEach-Object { (-split $_)[3] } | ADd-Content build\\Debug\\ktx.def
          lib /def:build\\Debug\\ktx.def /out:build\\Debug\\ktx.lib /machine:x64
          """
      _ -> Nothing

skipCDeps :: (GHA.HasEnvironmentVariables e) => e -> e
skipCDeps = GHA.env "PERIDOT_BUILD_SKIP_CDEPS" "1"

cdepsEnvVars :: (GHA.HasEnvironmentVariables e) => RunnerVariant -> e -> e
cdepsEnvVars variant =
  GHA.env "PERIDOT_BUILD_TP_SLANG_SKIP_CMAKE" "1"
    . GHA.env "PERIDOT_BUILD_TP_KTX_SKIP_CMAKE" "1"
    . GHA.env "PERIDOT_BUILD_TP_SLANG_LIB_PATH" slangLibPath
  where
    -- CIではDebugでビルドしてるのでそれを指定（ただしWindows以外ではなぜかRelease以下に生成される）
    slangLibPath = case variant of
      RunnerVariantWindows -> GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Debug/lib', github.workspace)"
      _ -> GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Release/lib', github.workspace)"

addPPAStep :: [String] -> GHA.Step
addPPAStep ppaList = GHA.namedAs "Add External PPA" $ GHA.runStep $ "sudo apt-add-repository -y " <> unwords ppaList

aptInstallStep :: [String] -> GHA.Step
aptInstallStep packages =
  GHA.namedAs "install apt packages" $
    GHA.runStep $
      "sudo apt-get update && sudo apt-get install -y " <> unwords packages

stdBashStep :: String -> GHA.Step
stdBashStep command = GHA.runStep command & GHA.stepUseShell "bash --noprofile --norc -eo pipefail {0}"

stdJob :: (SlackReportContext m, Functor m) => String -> [GHA.Step] -> m GHA.Job
stdJob name steps = reportJobFailure $ GHA.namedAs name $ GHA.job steps

stdWindowsJob :: (SlackReportContext m, Functor m) => String -> [GHA.Step] -> m GHA.Job
stdWindowsJob name steps = reportJobFailure $ GHA.namedAs name $ GHA.jobRunsOn ["windows-latest"] $ GHA.job steps

stdMacJob :: (SlackReportContext m, Functor m) => String -> [GHA.Step] -> m GHA.Job
stdMacJob name steps = reportJobFailure $ GHA.namedAs name $ GHA.jobRunsOn ["macos-latest"] $ GHA.job steps

checkFormats :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkFormats precondition =
  stdJob
    "Code Formats"
    ( GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step setupCargoOutputTranslatorStep,
            Step $ GHA.namedAs "Run rustfmt" $ GHA.runStep "cargo fmt -- --check",
            Step $
              GHA.namedAs "Run clippy" $
                stdBashStep "cargo clippy --all-features --all-targets --message-format=json | $HOME/.local/bin/cargo-json-gha-translator" & skipCDeps,
            Step $
              GHA.namedAs "Run check - Trailing Newline for Source Code Files" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/trailing_newline_checker.sh"
          ]
    )

checkBaseLayer :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkBaseLayer precondition =
  stdJob
    "Base Layer"
    ( GHA.withCondition precondition
        <$> [ checkoutStep,
              rustCacheStep,
              setupCargoOutputTranslatorStep,
              GHA.namedAs "Run checks" $
                stdBashStep "cargo check --package peridot --features=bedrock/VK_EXT_debug_report --message-format=json | $HOME/.local/bin/cargo-json-gha-translator" & skipCDeps,
              GHA.namedAs "Run checks(mt)" $
                stdBashStep "cargo check --package peridot --features=bedrock/VK_EXT_debug_report,mt --message-format=json | $HOME/.local/bin/cargo-json-gha-translator" & skipCDeps
            ]
    )

checkTools :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkTools precondition =
  stdJob
    "Tools"
    ( GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "Run checks" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "tools"
                  & skipCDeps
          ]
    )

checkModules :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkModules precondition =
  stdJob
    "Modules"
    ( GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "Run checks" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "modules"
                  & skipCDeps
          ]
    )

checkExamples :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkExamples precondition =
  stdJob
    "Examples"
    ( GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step setupCargoOutputTranslatorStep,
            Step $
              GHA.namedAs "Run checks" $
                GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
                  & GHA.workAt "examples"
                  & skipCDeps
          ]
    )

cliBuildStep :: GHA.Step
cliBuildStep = GHA.namedAs "Build CLI" $ GHA.workAt "./tools/cli" $ GHA.runStep "cargo build --no-default-features"

withBuilderEnv :: (GHA.HasEnvironmentVariables e) => e -> e
withBuilderEnv = setCradleBase . setBuiltinAssetsPath
  where
    setCradleBase = GHA.env "PERIDOT_CLI_CRADLE_BASE" $ GHA.mkExpression "format('{0}/cradle', github.workspace)"
    setBuiltinAssetsPath =
      GHA.env "PERIDOT_CLI_BUILTIN_ASSETS_PATH" $ GHA.mkExpression "format('{0}/builtin-assets', github.workspace)"

setLibrarySearchPathsUnix :: (GHA.HasEnvironmentVariables e) => e -> e
setLibrarySearchPathsUnix = GHA.env "LD_LIBRARY_PATH" $ GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Release/lib:{0}/thirdparty/ktx/source-repo/build:{1}', github.workspace, env.LD_LIBRARY_PATH)"

checkCradleWindows :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleWindows precondition = stdWindowsJob "Cradle(Windows)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step cliBuildStep,
            Step $ GHA.namedAs "Run checks" $ integratedTestStep integratedTestNormalScript,
            Step $ GHA.namedAs "Run checks for transparent-back" $ integratedTestStep integratedTestTransparentScript
          ]

    integratedTestStep = GHA.env "VK_SDK_PATH" "" . withBuilderEnv . skipCDeps . GHA.runStep
    integratedTestNormalScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot check examples/image-plane -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"
    integratedTestTransparentScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot check examples/image-plane -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"

checkCradleMacos :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleMacos precondition = platformExtraEnvs <$> stdMacJob "Cradle(macOS)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
            Step $
              cliBuildStep
                & GHA.env "RUSTFLAGS" (GHA.mkExpression "format('-Clink-arg=-Wl,-rpath,{0}/thirdparty/slang/source-repo/build/Release/lib -Clink-arg=-Wl,-rpath,{0}/thirdparty/ktx/source-repo/build', github.workspace)"),
            Step $ GHA.namedAs "Install requirements" $ GHA.runStep "brew install coreutils",
            Step integratedTestStep
          ]

    platformExtraEnvs = GHA.env "PERIDOT_BUILD_CLI_SKIP_DEBUG_RPATH" "1"

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "Run checks",
          GHA.stepUseShell "bash",
          GHA.env "VULKAN_SDK" "/Users",
          withBuilderEnv,
          GHA.env "PERIDOT_CLI_ARCHIVER_PATH" $
            GHA.mkExpression "format('{0}/tools/target/debug/peridot-archiver', github.workspace)",
          skipCDeps
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p mac 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleLinux :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleLinux precondition = stdJob "Cradle(Linux)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step disableAPTManualUpdateStep,
            Step $ addPPAStep ["ppa:pipewire-debian/pipewire-upstream"],
            Step $
              GHA.namedAs "Install extra packages" $
                aptInstallStep ["libwayland-dev", "libpipewire-0.3-dev", "libspa-0.2-dev"],
            Step checkoutStep,
            Step rustCacheStep,
            Step cliBuildStep,
            Step integratedTestStep
          ]

    integratedTestStep =
      applyModifiers
        [ GHA.namedAs "Run checks",
          GHA.stepUseShell "bash",
          withBuilderEnv,
          setLibrarySearchPathsUnix,
          skipCDeps
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleAndroid precondition = cdepsEnvVars RunnerVariantUbuntu <$> stdJob "Cradle(Android)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step rustCacheStep,
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
        [ GHA.namedAs "Run checks",
          GHA.stepUseShell "bash",
          withBuilderEnv,
          GHA.env "NDK_PLATFORM_TARGET" "28",
          setLibrarySearchPathsUnix,
          skipCDeps
        ]
        $ GHA.runStep "./tools/target/debug/peridot check examples/image-plane -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

reportSuccessJob :: (SlackReportContext m) => (Functor m) => m GHA.Job
reportSuccessJob =
  reportSuccessSteps <&> \reportSteps ->
    GHA.namedAs "Report as Success" $ GHA.grantWritable GHA.IDTokenPermission $ GHA.job reportSteps
