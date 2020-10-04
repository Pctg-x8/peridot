let GithubActions = ./schemas/Actions.dhall
let List/map = https://prelude.dhall-lang.org/List/map

let PowershellOnlyStep = \(params: GithubActions.Step.Type) -> params // {
    , `if` = Some "matrix.os == 'windows-latest'"
    }
let BashOnlyStep = \(params: GithubActions.Step.Type) -> params // {
    , `if` = Some "matrix.os != 'windows-latest'"
    }

in GithubActions.Workflow::{
    , name = Some "Dev-Package Build"
    , on = GithubActions.On.Single GithubActions.UnparameterizedTrigger.workflow_dispatch
    , jobs = toMap {
        , dev-pacage-build-to-artifact = GithubActions.Job::{
            , strategy = Some GithubActions.Strategy::{
                , matrix = Some (toMap {
                    os = List/map GithubActions.RunnerPlatform Text GithubActions.runnerPlatformAsText [
                        , GithubActions.RunnerPlatform.windows-latest
                        , GithubActions.RunnerPlatform.macos-latest
                        , GithubActions.RunnerPlatform.ubuntu-latest
                        ]
                    })
                }
            , runs-on = GithubActions.RunnerPlatform.Custom (GithubActions.mkExpression "matrix.os")
            , steps = [
                , GithubActions.Step::{
                    , name = "Checkout"
                    , uses = Some "actions/checkout@v2"
                    }
                , PowershellOnlyStep GithubActions.Step::{
                    , name = "Build tools (For PowerShell Env)"
                    , run = Some "powershell.exe -File ./tools/build-all.ps1 2>&1 | %{ \"$_\" }"
                    }
                , GithubActions.Step::{
                    , name = "Upgrade utils (Only for MacOS)"
                    , `if` = Some "matrix.os == 'macos-latest'"
                    , run = Some "brew install bash findutils"
                    }
                , BashOnlyStep GithubActions.Step::{
                    , name = "Build tools (For Bash Env)"
                    , run = Some "./tools/build-all.sh"
                    }
                , PowershellOnlyStep GithubActions.Step::{
                    , name = "Make package (For PowerShell Env)"
                    , run = Some "powershell.exe -File ./make-dev-package.ps1 -OutDirectory peridot-sdk -PeridotBranch $($Env:GITHUB_REF -replace \"^refs/heads/\")"
                    }
                , BashOnlyStep GithubActions.Step::{
                    , name = "Make package (For Bash Env)"
                    , run = Some "./make-dev-package.sh -o peridot-sdk -b \${GITHUB_REF#\"refs/heads/\"}"
                    }
                , GithubActions.Step::{
                    , name = "Upload package to Artifacts"
                    , uses = Some "actions/upload-artifact@v1"
                    , `with` = Some (toMap {
                        , name = "PeridotSDK-" ++ GithubActions.mkExpression "matrix.os"
                        , path = "peridot-sdk"
                        })
                    }
                ]
            }
        }
    }
