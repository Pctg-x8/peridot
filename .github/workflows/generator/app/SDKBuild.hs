{-# LANGUAGE NoOverloadedStrings #-}

module SDKBuild (workflow) where

import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.UploadArtifact qualified as UploadArtifact

powershellOnly :: (GHA.ConditionalElement e) => e -> e
powershellOnly = GHA.withCondition $ GHA.runnerOsExpr <> " == 'windows-latest'"

macOnly :: (GHA.ConditionalElement e) => e -> e
macOnly = GHA.withCondition $ GHA.runnerOsExpr <> " == 'macos-latest'"

bashOnly :: (GHA.ConditionalElement e) => e -> e
bashOnly = GHA.withCondition $ GHA.runnerOsExpr <> " != 'windows-latest'"

brewInstallStep :: [String] -> GHA.Step
brewInstallStep packages = macOnly $ GHA.runStep $ "brew install " <> unwords packages

bashScriptStep :: String -> GHA.Step
bashScriptStep script = bashOnly $ GHA.runStep script

poshScriptStep :: String -> String -> GHA.Step
poshScriptStep file args = powershellOnly $ GHA.runStep $ "powershell.exe -File " <> file <> " " <> args

artifactName, artifactDir :: String
artifactName = "PeridotSDK-" <> GHA.runnerOs
artifactDir = "peridot-sdk"

buildJob :: GHA.Job
buildJob =
  applyModifiers
    [ GHA.addStrategyMatrixEntry "os" ["windows-latest", "macos-latest", "ubuntu-latest"],
      GHA.jobRunsOn [GHA.mkRefMatrixValueExpression "os"]
    ]
    $ GHA.job
    $ mconcat [pure checkout, pure preconfigure, pure buildTools, buildPackage, pure upload]
  where
    checkout = GHA.namedAs "Checking out" $ Checkout.step Nothing
    preconfigure = GHA.namedAs "Upgrade utils (Only for macOS)" $ brewInstallStep ["bash", "findutils"]
    buildTools = GHA.namedAs "Build tools" $ GHA.runStep "make -C tools build-release"
    buildPackage =
      [ GHA.namedAs "Make package (For PowerShell Env)" $
          poshScriptStep "./make-dev-package.ps1" $
            "-OutDirectory " <> artifactDir <> " -PeridotBranch $($Env:GITHUB_REF -replace \"^refs/heads/\")",
        GHA.namedAs "Make package (For Bash Env)" $
          bashScriptStep $
            "./make-dev-package.sh -o " <> artifactDir <> " -b ${GITHUB_REF#\"refs/heads/\"}"
      ]
    upload = GHA.namedAs "Uploading Artifacts" $ UploadArtifact.step artifactName artifactDir

workflow :: GHA.Workflow
workflow =
  GHA.buildWorkflow
    [ GHA.namedAs "Dev-Package Build",
      GHA.workflowJob "dev-package-build-to-artifact" buildJob
    ]
    GHA.onWorkflowDispatch
