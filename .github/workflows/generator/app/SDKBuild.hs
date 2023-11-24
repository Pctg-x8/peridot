{-# LANGUAGE NoOverloadedStrings #-}

module SDKBuild (workflow) where

import Data.Aeson (ToJSON (toJSON))
import Utils (applyModifiers)
import Workflow.GitHub.Actions qualified as GHA
import Workflow.GitHub.Actions.Predefined.Checkout qualified as Checkout
import Workflow.GitHub.Actions.Predefined.UploadArtifact qualified as UploadArtifact

powershellOnlyStep :: GHA.StepModifier
powershellOnlyStep = GHA.withCondition "matrix.os == 'windows-latest'"

macOnlyStep :: GHA.StepModifier
macOnlyStep = GHA.withCondition "matrix.os == 'macos-latest'"

bashOnlyStep :: GHA.StepModifier
bashOnlyStep = GHA.withCondition "matrix.os != 'windows-latest'"

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
      [ powershellOnlyStep $
          GHA.namedAs "Build tools (For PowerShell Env)" $
            GHA.runStep "powershell.exe -File ./tools/build-all.ps1 2>&1 | %{ \"$_\" }",
        macOnlyStep $ GHA.namedAs "Upgrade utils (Only for macOS)" $ GHA.runStep "brew install bash findutils",
        bashOnlyStep $ GHA.namedAs "Build tools (For Bash Env)" $ GHA.runStep "./tools/build-all.sh"
      ]
    buildPackage =
      [ powershellOnlyStep $
          GHA.namedAs "Make package (For PowerShell Env)" $
            GHA.runStep $
              "powershell.exe -File ./make-dev-package.ps1 -OutDirectory " <> artifactDir <> " -PeridotBranch $($Env:GITHUB_REF -replace \"^refs/heads/\")",
        bashOnlyStep $
          GHA.namedAs "Make package (For Bash Env)" $
            GHA.runStep $
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
