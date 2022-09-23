let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let actions/checkout =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps/actions/checkout.dhall

let aws-actions/configure-aws-credentials =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps/aws-actions/configure-aws-credentials.dhall

let CodeformCheckerAction = ../../actions/codeform-checker/schema.dhall

let CheckBuildSubdirAction = ../../actions/checkbuild-subdir/schema.dhall

let List/concat = https://prelude.dhall-lang.org/List/concat

let List/map = https://prelude.dhall-lang.org/List/map

let List/end_map = λ(t : Type) → λ(f : t → t) → λ(a : List t) → List/map t t f a

let eRepositoryOwnerLogin =
      GithubActions.mkExpression "github.event.repository.owner.login"

let eRepositoryName = GithubActions.mkExpression "github.event.repository.name"

let ePullRequestNumber = GithubActions.mkExpression "github.event.number"

let ePullRequestTitle =
      GithubActions.mkExpression "github.event.pull_request.title"

let ePullRequestHeadHash =
      GithubActions.mkExpression "github.event.pull_request.head.sha"

let ePullRequestBaseHash =
      GithubActions.mkExpression "github.event.pull_request.base.sha"

let eSecretGithubToken = GithubActions.mkExpression "secrets.GITHUB_TOKEN"

let depends =
      λ(deps : List Text) →
      λ(job : GithubActions.Job.Type) →
        job ⫽ { needs = Some deps }

let withCondition =
      λ(cond : Text) →
      λ(job : GithubActions.Job.Type) →
        job ⫽ { `if` = Some cond }

let withConditionStep =
      λ(cond : Text) →
      λ(step : GithubActions.Step.Type) →
        step ⫽ { `if` = Some cond }

let runStepOnFailure = withConditionStep "failure()"

let configureSlackNotification =
        aws-actions/configure-aws-credentials.step
          aws-actions/configure-aws-credentials.Params::{
          , awsRegion = "ap-northeast-1"
          , roleToAssume = Some
              "arn:aws:iam::208140986057:role/GHALambdaInvoker"
          }
      ⫽ { name = "Configure for Slack Notification" }

let preconditionRecordBeginTimeStep =
      GithubActions.Step::{
      , name = "Getting begintime"
      , id = Some "begintime"
      , run = Some "echo \"::set-output name=begintime::\$(date +%s)\""
      }

let preconditionBeginTimestampOutputDef =
      toMap
        { begintime =
            GithubActions.mkRefStepOutputExpression "begintime" "begintime"
        }

let checkoutStep = actions/checkout.stepv3 actions/checkout.Params::{=}

let checkoutHeadStep =
        actions/checkout.stepv3
          actions/checkout.Params::{ ref = Some ePullRequestHeadHash }
      ⫽ { name = "Checking out (HEAD commit)" }

let cacheStep =
      GithubActions.Step::{
      , name = "Initialize Cache"
      , uses = Some "actions/cache@v2"
      , `with` = Some
          ( toMap
              { path =
                  GithubActions.WithParameterType.Text
                    ''
                    ~/.cargo/registry
                    ~/.cargo/git
                    target
                    ''
              , key =
                  let os = GithubActions.mkExpression "runner.os"

                  let hash =
                        GithubActions.mkExpression "hashFiles('**/Cargo.lock')"

                  in  GithubActions.WithParameterType.Text "${os}-cargo-${hash}"
              }
          )
      }

let SlackNotification = < Success | Failure : Text >

let SlackNotifyProvider =
      { Success : GithubActions.Step.Type
      , Failure : Text → GithubActions.Step.Type
      }

let slackNotify =
      λ(provider : SlackNotifyProvider) →
      λ(state : SlackNotification) →
        [ configureSlackNotification, merge provider state ]

let checkFormats =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Code Formats"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  ,   CodeformCheckerAction.step
                        { script = CodeformCheckerAction.Script.codeform_check }
                    ⫽ { name = "Running Check: Line Width" }
                  ,   CodeformCheckerAction.step
                        { script =
                            CodeformCheckerAction.Script.vulnerabilities_elliminator
                        }
                    ⫽ { name = "Running Check: Debugging Weaks" }
                  ,   CodeformCheckerAction.step
                        { script =
                            CodeformCheckerAction.Script.trailing_newline_checker
                        }
                    ⫽ { name =
                          "Running Check: Trailing Newline for Source Code Files"
                      }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-formats")
                  )
              ]
        }

let checkBaseLayer =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Base Layer"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/end_map
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , GithubActions.Step::{
                    , name = "Building as Checking"
                    , uses = Some "./.github/actions/checkbuild-baselayer"
                    }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-baselayer")
                  )
              ]
        }

let checkTools =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Tools"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , CheckBuildSubdirAction.step { path = "tools" }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-tools")
                  )
              ]
        }

let checkModules =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Modules"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , CheckBuildSubdirAction.step { path = "." }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-modules")
                  )
              ]
        }

let checkExamples =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Examples"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , CheckBuildSubdirAction.step { path = "examples" }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-examples")
                  )
              ]
        }

let checkCradleWindows =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Cradle(Windows)"
        , runs-on = GithubActions.RunnerPlatform.windows-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/end_map
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , GithubActions.Step::{
                    , name = "Build CLI"
                    , run = Some "cargo build"
                    , working-directory = Some "tools/cli"
                    }
                  , GithubActions.Step::{
                    , name = "cargo check"
                    , run = Some
                        ''
                        $ErrorActionPreference = "Continue"
                        pwsh -c 'tools/target/release/peridot.exe test examples/basic -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
                        ''
                    , env = Some
                        ( toMap
                            { VK_SDK_PATH = ""
                            , PERIDOT_CLI_CRADLE_BASE =
                                GithubActions.mkExpression
                                  "format('{0}/cradle', github.workspace)"
                            }
                        )
                    }
                  , GithubActions.Step::{
                    , name = "cargo check for transparent-back"
                    , run = Some
                        ''
                            $ErrorActionPreference = "Continue"
                            pwsh -c 'tools/target/release/peridot.exe test examples/basic -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
                        ''
                    , env = Some
                        ( toMap
                            { VK_SDK_PATH = ""
                            , PERIDOT_CLI_CRADLE_BASE =
                                GithubActions.mkExpression
                                  "format('{0}/cradle', github.workspace)"
                            }
                        )
                    }
                  ]
              , List/end_map
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-cradle-windows")
                  )
              ]
        }

let checkCradleMacos =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Cradle(macOS)"
        , runs-on = GithubActions.RunnerPlatform.macos-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/end_map
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , GithubActions.Step::{
                    , name = "Build CLI"
                    , run = Some "cargo build"
                    , working-directory = Some "tools/cli"
                    }
                  , GithubActions.Step::{
                    , name = "Build archiver"
                    , run = Some "cargo build"
                    , working-directory = Some "tools/archiver"
                    }
                  , GithubActions.Step::{
                    , name = "install requirements"
                    , run = Some "brew install coreutils"
                    }
                  , GithubActions.Step::{
                    , name = "cargo check"
                    , run = Some
                        "tools/target/release/peridot check examples/basic -p mac 2>&1 | tee \$GITHUB_WORKSPACE/.buildlog"
                    , shell = Some GithubActions.Shell.bash
                    , env = Some
                        ( toMap
                            { VULKAN_SDK = "/Users"
                            , PERIDOT_CLI_CRADLE_BASE =
                                GithubActions.mkExpression
                                  "format('{0}/cradle', github.workspace)"
                            , PERIDOT_CLI_BUILTIN_ASSETS_PATH =
                                GithubActions.mkExpression
                                  "format('{0}/builtin-assets', github.workspace)"
                            , PERIDOT_CLI_ARCHIVER_PATH =
                                GithubActions.mkExpression
                                  "format('{0}/tools/target/release/peridot-archiver', github.workspace)"
                            }
                        )
                    }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-cradle-macos")
                  )
              ]
        }

let checkCradleLinux =
      λ(notifyProvider : SlackNotifyProvider) →
      λ(precondition : Text) →
        GithubActions.Job::{
        , name = Some "Cradle(Linux)"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ List/end_map
                  GithubActions.Step.Type
                  (withConditionStep precondition)
                  [ checkoutHeadStep
                  , checkoutStep
                  , cacheStep
                  , GithubActions.Step::{
                    , name = "Build CLI"
                    , run = Some "cargo build"
                    , working-directory = Some "tools/cli"
                    }
                  , GithubActions.Step::{
                    , name = "cargo check"
                    , run = Some
                        "tools/target/release/peridot check examples/basic -p linux 2>&1 | tee \$GITHUB_WORKSPACE/.buildlog"
                    , shell = Some GithubActions.Shell.bash
                    , env = Some
                        ( toMap
                            { PERIDOT_CLI_CRADLE_BASE =
                                GithubActions.mkExpression
                                  "format('{0}/cradle', github.workspace)"
                            , PERIDOT_CLI_BUILTIN_ASSETS_PATH =
                                GithubActions.mkExpression
                                  "format('{0}/builtin-assets', github.workspace)"
                            }
                        )
                    }
                  ]
              , List/map
                  GithubActions.Step.Type
                  GithubActions.Step.Type
                  runStepOnFailure
                  ( slackNotify
                      notifyProvider
                      (SlackNotification.Failure "check-cradle-linux")
                  )
              ]
        }

let reportSuccessJob =
      λ(notifyProvider : SlackNotifyProvider) →
        GithubActions.Job::{
        , name = Some "Report as Success"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
        , permissions = Some (toMap { id-token = "write" })
        , steps =
            List/concat
              GithubActions.Step.Type
              [ [ checkoutHeadStep, checkoutStep ]
              , slackNotify notifyProvider SlackNotification.Success
              ]
        }

in  { depends
    , withCondition
    , withConditionStep
    , runStepOnFailure
    , preconditionRecordBeginTimeStep
    , preconditionBeginTimestampOutputDef
    , eRepositoryOwnerLogin
    , eRepositoryName
    , ePullRequestNumber
    , ePullRequestTitle
    , ePullRequestHeadHash
    , ePullRequestBaseHash
    , eSecretGithubToken
    , configureSlackNotification
    , SlackNotifyProvider
    , checkoutHeadStep
    , checkoutStep
    , checkFormats
    , checkBaseLayer
    , checkTools
    , checkModules
    , checkExamples
    , checkCradleWindows
    , checkCradleMacos
    , checkCradleLinux
    , reportSuccessJob
    , cacheStep
    }
