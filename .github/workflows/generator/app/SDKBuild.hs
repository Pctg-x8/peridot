{-# LANGUAGE NoOverloadedStrings #-}

module SDKBuild (workflow) where

import Data.Aeson (ToJSON (toJSON))
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.UploadArtifact qualified as UploadArtifact

powershellOnly :: (GHA.ConditionalElement e) => e -> e
powershellOnly = GHA.withCondition "matrix.os == 'windows-latest'"

macOnly :: (GHA.ConditionalElement e) => e -> e
macOnly = GHA.withCondition "matrix.os == 'macos-latest'"

bashOnly :: (GHA.ConditionalElement e) => e -> e
bashOnly = GHA.withCondition "matrix.os != 'windows-latest'"

bashScriptStep :: String -> GHA.Step
bashScriptStep script = bashOnly $ GHA.runStep script

poshScriptStep :: String -> String -> GHA.Step
poshScriptStep file args = powershellOnly $ GHA.runStep $ "powershell.exe -File " <> file <> " " <> args

buildJob :: GHA.Job
buildJob =
  applyModifiers
    [ GHA.jobAddStrategyMatrix "os" $ toJSON ["windows-latest", "macos-latest", "ubuntu-latest"],
      GHA.jobRunsOn [GHA.mkRefMatrixValueExpression "os"]
    ]
    $ GHA.job
    $ [checkout] <> buildTools <> buildPackage <> [upload]
  where
    artifactName = "PeridotSDK-" <> GHA.mkRefMatrixValueExpression "os"
    artifactDir = "peridot-sdk"
    checkout = GHA.namedAs "Checking out" $ Checkout.step Nothing
    buildTools =
      [ GHA.namedAs "Build tools (For PowerShell Env)" $
          poshScriptStep "./tools/build-all.ps1" "2>&1 | %{ \"$_\" }",
        macOnly $ GHA.namedAs "Upgrade utils (Only for macOS)" $ GHA.runStep "brew install bash findutils",
        GHA.namedAs "Build tools (For Bash Env)" $ bashScriptStep "./tools/build-all.sh"
      ]
    buildPackage =
      [ GHA.namedAs "Make package (For PowerShell Env)" $
          poshScriptStep "./make-dev-package.ps1" $
            "-OutDirectory " <> artifactDir <> " -PeridotBranch $($Env:GITHUB_REF -replace \"^refs/heads/\")",
        GHA.namedAs "Make package (For Bash Env)" $
          bashScriptStep $
            "./make-dev-package.sh -o " <> artifactDir <> " -b ${GITHUB_REF#\"refs/heads/\"}"
      ]
    upload = UploadArtifact.step artifactName artifactDir

workflow :: GHA.Workflow
workflow =
  GHA.buildWorkflow
    [ GHA.namedAs "Dev-Package Build",
      GHA.workflowJob "dev-package-build-to-artifact" buildJob
    ]
    GHA.onWorkflowDispatch
