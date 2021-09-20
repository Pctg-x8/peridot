let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let ProvidedSteps =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps.dhall

let CodeformCheckerAction = ../../actions/codeform-checker/schema.dhall

let CheckBuildSubdirAction = ../../actions/checkbuild-subdir/schema.dhall

let SlackNotifierAction =
      https://raw.githubusercontent.com/Pctg-x8/ci-notifications-post-invoker/master/schema.dhall

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

let eSecretAWSAccessKey = GithubActions.mkExpression "secrets.AWS_ACCESS_KEY_ID"

let eSecretAWSAccessSecret =
      GithubActions.mkExpression "secrets.AWS_ACCESS_SECRET"

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
      GithubActions.Step::{
      , name = "Configure for Slack Notification"
      , id = Some "cfgNotification"
      , run = Some
          ''
          # re-export configs for further step
          echo AWS_ROLE_ARN=$AWS_ROLE_ARN >> $GITHUB_ENV
          echo AWS_WEB_IDENTITY_TOKEN_FILE=$AWS_WEB_IDENTITY_TOKEN_FILE >> $GITHUB_ENV
          echo AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION >> $GITHUB_ENV

          curl -H "Authorization: Bearer $ACTIONS_ID_TOKEN_REQUEST_TOKEN" "$ACTIONS_ID_TOKEN_REQUEST_URL" | jq -r ".value" > $AWS_WEB_IDENTITY_TOKEN_FILE
          ''
      , env = Some
          ( toMap
              { AWS_ROLE_ARN = "arn:aws:iam::208140986057:role/GHALambdaInvoker"
              , AWS_WEB_IDENTITY_TOKEN_FILE = "/tmp/awstoken"
              , AWS_DEFAULT_REGION = "ap-northeast-1"
              }
          )
      }

let preconditionRecordBeginTimeStep =
      GithubActions.Step::{
      , name = "Getting begintime"
      , id = Some "begintime"
      , run = Some "echo \"::set-output name=begintime::\$(date +%s)\""
      }

let preconditionBeginTimestampOutputDef =
      toMap
        { begintime =
            GithubActions.mkExpression "steps.begintime.outputs.begintime"
        }

let checkoutStep = ProvidedSteps.checkoutStep ProvidedSteps.CheckoutParams::{=}

let checkoutHeadStep =
        ProvidedSteps.checkoutStep
          ProvidedSteps.CheckoutParams::{ ref = Some ePullRequestHeadHash }
      ⫽ { name = "Checking out (HEAD commit)" }

let cacheStep =
      GithubActions.Step::{
      , name = "Initialize Cache"
      , uses = Some "actions/cache@v2"
      , `with` = Some
          ( toMap
              { path =
                  ''
                  ~/.cargo/registry
                  ~/.cargo/git
                  target
                  ''
              , key =
                  "${GithubActions.mkExpression
                       "runner.os"}-cargo-${GithubActions.mkExpression
                                              "hashFiles('**/Cargo.lock')"}"
              }
          )
      }

let slackNotifyIfFailureStep =
      λ(stepName : Text) →
        SlackNotifierAction.step
          { status = SlackNotifierAction.Status.Failure stepName
          , begintime =
              GithubActions.mkExpression "needs.preconditions.outputs.begintime"
          , report_name = "PR Integrity Check"
          , mode =
              SlackNotifierAction.Mode.Diff
                { head_sha = ePullRequestHeadHash
                , base_sha = ePullRequestBaseHash
                , pr_number = ePullRequestNumber
                , pr_title = ePullRequestTitle
                }
          }

let slackNotifySuccessStep =
      SlackNotifierAction.step
        { status = SlackNotifierAction.Status.Success
        , begintime =
            GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        , report_name = "PR Integrity Check"
        , mode =
            SlackNotifierAction.Mode.Diff
              { head_sha = ePullRequestHeadHash
              , base_sha = ePullRequestBaseHash
              , pr_number = ePullRequestNumber
              , pr_title = ePullRequestTitle
              }
        }

let weeklySlackNotifyAsFailureStep =
      λ(stepName : Text) →
        SlackNotifierAction.step
          { status = SlackNotifierAction.Status.Failure stepName
          , begintime =
              GithubActions.mkExpression "needs.preconditions.outputs.begintime"
          , report_name = "Weekly Check"
          , mode = SlackNotifierAction.Mode.Branch
          }

let weeklySlackNotifyAsSuccessStep =
      SlackNotifierAction.step
        { status = SlackNotifierAction.Status.Success
        , begintime =
            GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        , report_name = "Weekly Check"
        , mode = SlackNotifierAction.Mode.Branch
        }

let SlackNotification = < Success | Failure : Text >

let SlackNotifyProvider =
      { Success : GithubActions.Step.Type
      , Failure : Text → GithubActions.Step.Type
      }

let prSlackNotifyProvider =
      { Success = slackNotifySuccessStep ⫽ { name = "Notify as Success" }
      , Failure =
          λ(phase : Text) →
            slackNotifyIfFailureStep phase ⫽ { name = "Notify as Failure" }
      }

let weeklySlackNotifyProvider =
      { Success =
          weeklySlackNotifyAsSuccessStep ⫽ { name = "Notify as Success" }
      , Failure =
          λ(phase : Text) →
              weeklySlackNotifyAsFailureStep phase
            ⫽ { name = "Notify as Failure" }
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
                    , run = Some "cargo build --release"
                    , working-directory = Some "tools/cli"
                    }
                  , GithubActions.Step::{
                    , name = "cargo check"
                    , run = Some
                        ''
                        $ErrorActionPreference = "Continue"
                        pwsh -c 'target/release/peridot.exe test examples/basic -p windows -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
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
                            pwsh -c 'target/release/peridot.exe test examples/basic -p windows -F transparent -F bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
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
              , List/map
                  GithubActions.Step.Type
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
                    , run = Some "cargo build --release"
                    , working-directory = Some "tools/cli"
                    }
                  , GithubActions.Step::{
                    , name = "Build archiver"
                    , run = Some "cargo build --release"
                    , working-directory = Some "tools/archiver"
                    }
                  , GithubActions.Step::{
                    , name = "install requirements"
                    , run = Some "brew install coreutils"
                    }
                  , GithubActions.Step::{
                    , name = "cargo check"
                    , run = Some
                        "target/release/peridot check examples/basic -p mac 2>&1 | tee \$GITHUB_WORKSPACE/.buildlog"
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
                                  "format('{0}/target/release/peridot-archiver', github.workspace)"
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

let reportSuccessJob =
      λ(notifyProvider : SlackNotifyProvider) →
        GithubActions.Job::{
        , name = Some "Report as Success"
        , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
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
    , eSecretGithubToken
    , configureSlackNotification
    , slackNotifyIfFailureStep
    , slackNotifySuccessStep
    , prSlackNotifyProvider
    , weeklySlackNotifyProvider
    , checkoutHeadStep
    , checkFormats
    , checkBaseLayer
    , checkTools
    , checkModules
    , checkExamples
    , checkCradleWindows
    , checkCradleMacos
    , reportSuccessJob
    , cacheStep
    }
