{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultilineStrings #-}

module IntegrityTest.Shared
  ( pullRequestNumberExpr,
    preconditionRecordBeginTimeStamp,
    preconditionBeginTimestampOutputDef,
    checkFormats,
    preciseCheck,
    checkCradleWindows,
    checkCradleMacos,
    checkCradleLinux,
    checkCradleAndroid,
    checkCradleAll,
    checkEditorMP,
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
import Data.Map qualified as M
import Peridot qualified
import SlackNotification (SlackReportContext (..), reportJobFailure, reportSuccessSteps)
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.JobGroupComposer (JobMap)
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
checkoutStep = GHA.namedAs "Checking out (with submodules)" $ Checkout.step Nothing & Checkout.submodules Checkout.SubmodulesRecursive

lightCheckoutStep :: GHA.Step
lightCheckoutStep = GHA.namedAs "Checking out" $ Checkout.step Nothing

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

rustCacheStep :: String -> GHA.Step
rustCacheStep postfix =
  GHA.namedAs "Initialize Cache" $
    CacheAction.step ["~/.cargo", "target", "tools/target", "examples/**/target", "cradle/**/target", "editor/**/target"] key
      & CacheAction.restoreKeys [commonKeyPrefix, keyPrefix]
  where
    commonKeyPrefix = GHA.runnerOs <> "-cargo-"
    keyPrefix = commonKeyPrefix <> (if postfix == "" then "" else postfix <> "-")
    key = keyPrefix <> GHA.mkExpression "hashFiles('**/*.rs', '**/Cargo.toml')"

cmake :: [String] -> String
cmake args = unwords ("cmake" : args)

cargoTranslated :: String -> [String] -> String
cargoTranslated subcmd args = "cargo " <> subcmd <> " " <> unwords args <> " --message-format=json | $HOME/.local/bin/cargo-json-gha-translator"

data RunnerVariant = RunnerVariantWindows | RunnerVariantMac | RunnerVariantUbuntu

preBuildCDeps :: RunnerVariant -> Step
preBuildCDeps variant =
  StepGroup
    [ Step $
        GHA.namedAs "Setup cache for ccache artifacts" $
          CacheAction.step [cacheDirectoryPath] (ccCachePrefix <> ccTargetHash)
            & CacheAction.restoreKeys [ccCachePrefix],
      installStep,
      Step $ GHA.namedAs "Prepare cdeps build dir" $ GHA.runStep prepareBuildDirCmd,
      Step $
        GHA.parallelSteps
          [ GHA.namedAs "Pre-build c deps(slang)" $
              GHA.runStep
                ( maybe "" (<> "\n") commandPrelude
                    <> cmake
                      [ "--preset",
                        "default",
                        "-G",
                        "Ninja",
                        "-DCMAKE_CXX_COMPILER_LAUNCHER=ccache",
                        "-DCMAKE_C_COMPILER_LAUNCHER=ccache",
                        "-DSLANG_ENABLE_DXIL=FALSE",
                        "-DSLANG_ENABLE_GFX=FALSE",
                        "-DSLANG_ENABLE_SLANGD=FALSE",
                        "-DSLANG_ENABLE_SLANGC=FALSE",
                        "-DSLANG_ENABLE_SLANGI=FALSE",
                        "-DSLANG_ENABLE_SLANGRT=FALSE",
                        "-DSLANG_ENABLE_SLANG_GLSLANG=FALSE",
                        "-DSLANG_ENABLE_SLANG_PROXY=FALSE",
                        "-DSLANG_ENABLE_TESTS=FALSE",
                        "-DSLANG_ENABLE_EXAMPLES=FALSE",
                        "-DSLANG_ENABLE_REPLAYER=FALSE",
                        "-DSLANG_ENABLE_RECORD_REPLAY=FALSE",
                        "-DSLANG_ENABLE_SLANG_RHI=FALSE",
                        "-DSLANG_LIB_TYPE=STATIC"
                      ]
                    <> " && cmake --build --preset releaseWithDebugInfo"
                )
                & GHA.workAt "thirdparty/slang/source-repo",
            GHA.namedAs "Pre-build c deps(ktx)" $
              GHA.runStep
                ( maybe "" (<> "\n") commandPrelude
                    <> cmake
                      [ "../../source-repo",
                        "-B",
                        ".",
                        "-G",
                        "Ninja",
                        "-DCMAKE_CXX_COMPILER_LAUNCHER=ccache",
                        "-DCMAKE_C_COMPILER_LAUNCHER=ccache",
                        "-DKTX_FEATURE_TESTS=OFF",
                        "-DKTX_FEATURE_VK_UPLOAD=OFF",
                        "-DKTX_FEATURE_GL_UPLOAD=OFF",
                        "-DKTX_FEATURE_TOOLS=OFF"
                      ]
                    <> " && cmake --build ."
                    <> maybe "" ("\n" <>) commandPostKtx
                )
                & GHA.workAt ktxBuildDir
          ]
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
    installStep = Step $ GHA.namedAs "Install ccache" $ case variant of
      RunnerVariantUbuntu -> GHA.runStep "sudo apt-get update && sudo apt-get install ccache"
      RunnerVariantWindows -> GHA.runStep "choco install ccache"
      RunnerVariantMac -> GHA.runStep "brew install ccache"
    commandPrelude = case variant of
      RunnerVariantWindows ->
        Just
          """
          & 'C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\Tools\\Launch-VsDevShell.ps1'
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
    ktxBuildDir = case variant of
      RunnerVariantWindows -> "thirdparty/ktx/cdeps-build/x86_64-pc-windows-msvc"
      RunnerVariantMac -> "thirdparty/ktx/cdeps-build/aarch64-apple-darwin"
      RunnerVariantUbuntu -> "thirdparty/ktx/cdeps-build/x86_64-unknown-linux-gnu"
    prepareBuildDirCmd = case variant of
      RunnerVariantWindows -> "New-Item " <> ktxBuildDir
      RunnerVariantMac -> "mkdir -p " <> ktxBuildDir
      RunnerVariantUbuntu -> "mkdir -p " <> ktxBuildDir

skipCDeps :: (GHA.HasEnvironmentVariables e) => e -> e
skipCDeps = Peridot.setBuildEnvSwitch "SKIP_CDEPS"

cdepsEnvVars :: (GHA.HasEnvironmentVariables e) => RunnerVariant -> e -> e
cdepsEnvVars variant =
  Peridot.setBuildEnvSwitch "TP_SLANG_SKIP_CMAKE"
    . Peridot.setBuildEnvSwitch "TP_KTX_SKIP_CMAKE"
    . Peridot.setBuildEnv "TP_SLANG_LIB_PATH" slangLibPath
    . Peridot.setBuildEnvSwitch "TP_SPIRV_TOOLS_SKIP_CMAKE"
  where
    -- CIではDebugでビルドしてるのでそれを指定（ただしWindows以外ではなぜかRelease以下に生成される）
    slangLibPath = case variant of
      RunnerVariantWindows -> GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Debug/lib', github.workspace)"
      _ -> GHA.mkExpression "format('{0}/thirdparty/slang/source-repo/build/Release/lib', github.workspace)"

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

setDummyVulkanSDKPathForUnlinking :: (GHA.HasEnvironmentVariables e) => e -> e
setDummyVulkanSDKPathForUnlinking = GHA.env "VULKAN_SDK" ""

checkFormats :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkFormats precondition =
  stdJob "Code Formats" $
    GHA.withCondition precondition
      <$> flattenSteps
        [ Step lightCheckoutStep,
          Step $ rustCacheStep "formats",
          Step setupCargoOutputTranslatorStep,
          Step $ GHA.namedAs "Run rustfmt" $ GHA.runStep "cargo fmt --all --check",
          Step $
            GHA.namedAs "Run clippy" $
              skipCDeps $
                stdBashStep (cargoTranslated "clippy" ["--all-features", "--all-targets", "--workspace"]),
          Step $
            GHA.namedAs "Run check - Trailing Newline for Source Code Files" $
              GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/trailing_newline_checker.sh"
        ]

preciseCheck :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
preciseCheck precondition = stdJob "Precise Check" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step lightCheckoutStep,
            Step $ rustCacheStep "",
            Step setupCargoOutputTranslatorStep,
            Step $ GHA.namedAs "Run checks (base)" $ skipCDeps $ stdBashStep (cargoTranslated "clippy" ["--package", "peridot", "--features=bedrock/VK_EXT_debug_report"]),
            Step $ GHA.namedAs "Run checks (base, mt)" $ skipCDeps $ stdBashStep (cargoTranslated "clippy" ["--package", "peridot", "--features=bedrock/VK_EXT_debug_report,mt"]),
            Step $ GHA.namedAs "Run cahcks (modules)" $ skipCDeps $ GHA.workAt "modules" $ GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh",
            Step $ GHA.namedAs "Run checks (tools)" $ skipCDeps $ GHA.workAt "tools" $ stdBashStep (cargoTranslated "clippy" ["--workspace", "--all-features", "--all-targets"]),
            Step $ GHA.namedAs "Run checks (examples)" $ skipCDeps $ GHA.workAt "examples" $ GHA.runStep "exec $GITHUB_WORKSPACE/.github/scripts/checkbuild-subdir.sh"
          ]

cliBuildStep :: GHA.Step
cliBuildStep = GHA.namedAs "Build CLI" $ GHA.workAt "./tools/cli" $ GHA.runStep "cargo build --no-default-features"

withBuilderEnv :: (GHA.HasEnvironmentVariables e) => e -> e
withBuilderEnv = setCradleBase . setBuiltinAssetsPath
  where
    setCradleBase = Peridot.setCradleBasePath $ GHA.mkExpression "format('{0}/cradle', github.workspace)"
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
            Step $ rustCacheStep "",
            preBuildCDeps RunnerVariantWindows,
            Step $ skipCDeps cliBuildStep,
            Step $ GHA.namedAs "Run checks" $ integratedTestStep integratedTestNormalScript,
            Step $ GHA.namedAs "Run checks for transparent-back" $ integratedTestStep integratedTestTransparentScript
          ]

    integratedTestStep = setDummyVulkanSDKPathForUnlinking . withBuilderEnv . skipCDeps . GHA.runStep
    integratedTestNormalScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot check cradle/test-fixture -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"
    integratedTestTransparentScript =
      "\
      \$ErrorActionPreference = \"Continue\"\n\
      \pwsh -c 'tools/target/debug/peridot check cradle/test-fixture -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog"

checkCradleMacos :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleMacos precondition = platformExtraEnvs <$> stdMacJob "Cradle(macOS)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step $ rustCacheStep "",
            preBuildCDeps RunnerVariantMac,
            Step $
              skipCDeps cliBuildStep
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
        $ GHA.runStep "./tools/target/debug/peridot check cradle/test-fixture -p mac 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleLinux :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleLinux precondition = stdJob "Cradle(Linux)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step disableAPTManualUpdateStep,
            Step $
              GHA.namedAs "Install extra packages" $
                aptInstallStep ["libwayland-dev", "libpipewire-0.3-dev", "libspa-0.2-dev"],
            Step checkoutStep,
            Step $ rustCacheStep "",
            preBuildCDeps RunnerVariantUbuntu,
            -- worksround for ci broken
            Step $ Peridot.setBuildEnvSwitch "TP_KTX_WA_DISABLE_SSE" $ skipCDeps cliBuildStep,
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
        $ GHA.runStep "./tools/target/debug/peridot check cradle/test-fixture -p linux 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAndroid :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkCradleAndroid precondition = cdepsEnvVars RunnerVariantUbuntu <$> stdJob "Cradle(Android)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step checkoutStep,
            Step $ rustCacheStep "",
            Step $
              GHA.namedAs "Setup Rust for Android" $
                RustToolchainAction.step
                  & RustToolchainAction.useStable
                  & RustToolchainAction.forTarget "aarch64-linux-android",
            Step $ GHA.namedAs "Setup Java" $ SetupJavaAction.step "adopt" & SetupJavaAction.javaVersion "17",
            Step $ GHA.namedAs "install cargo-ndk" $ GHA.runStep "cargo install cargo-ndk",
            Step $ skipCDeps cliBuildStep,
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
        $ GHA.runStep "./tools/target/debug/peridot check cradle/test-fixture -p android 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"

checkCradleAll :: (SlackReportContext m) => (Applicative m) => String -> m JobMap
checkCradleAll precondition = do
  win <- checkCradleWindows precondition
  mac <- checkCradleMacos precondition
  linux <- checkCradleLinux precondition
  android <- checkCradleAndroid precondition

  return $ M.fromList [("check-cradle-win", win), ("check-cradle-mac", mac), ("check-cradle-linux", linux), ("check-cradle-android", android)]

checkEditor :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkEditor precondition = stdJob "Editor (Linux)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step lightCheckoutStep,
            Step $ rustCacheStep "editor",
            Step setupCargoOutputTranslatorStep,
            Step $ GHA.namedAs "Run rustfmt" $ GHA.workAt "editor/linux" $ GHA.runStep "cargo fmt -- --check",
            Step $ applyModifiers [GHA.namedAs "Run clippy", skipCDeps, GHA.workAt "editor/linux"] $ stdBashStep (cargoTranslated "clippy" [])
          ]

checkEditorWin :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkEditorWin precondition = stdWindowsJob "Editor (Windows)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step lightCheckoutStep,
            Step $ rustCacheStep "editor",
            Step $ applyModifiers [GHA.namedAs "Run clippy", GHA.workAt "editor/win", setDummyVulkanSDKPathForUnlinking, skipCDeps] $ GHA.runStep "cargo clippy"
          ]

checkEditorMac :: (SlackReportContext m) => (Functor m) => String -> m GHA.Job
checkEditorMac precondition = stdMacJob "Editor (macOS)" steps
  where
    steps =
      GHA.withCondition precondition
        <$> flattenSteps
          [ Step lightCheckoutStep,
            Step $ rustCacheStep "editor",
            Step $ applyModifiers [GHA.namedAs "Run clippy", GHA.workAt "editor/mac", skipCDeps] $ GHA.runStep "cargo clippy"
          ]

checkEditorMP :: (SlackReportContext m) => (Applicative m) => String -> m JobMap
checkEditorMP precondition = do
  linux <- checkEditor precondition
  win <- checkEditorWin precondition
  mac <- checkEditorMac precondition

  return $ M.fromList [("check-editor-linux", linux), ("check-editor-win", win), ("check-editor-mac", mac)]

reportSuccessJob :: (SlackReportContext m) => (Functor m) => m GHA.Job
reportSuccessJob =
  reportSuccessSteps <&> \reportSteps ->
    GHA.namedAs "Report as Success" $ GHA.grantWritable GHA.IDTokenPermission $ GHA.job reportSteps
