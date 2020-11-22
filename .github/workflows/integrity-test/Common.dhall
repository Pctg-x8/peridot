let GithubActions = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall
let ProvidedSteps = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps.dhall

let CodeformCheckerAction = ../../actions/codeform-checker/schema.dhall
let CheckBuildSubdirAction = ../../actions/checkbuild-subdir/schema.dhall
let SlackNotifierAction = https://raw.githubusercontent.com/Pctg-x8/ci-notifications-post-invoker/master/schema.dhall
let List/concat = https://prelude.dhall-lang.org/List/concat
let List/map = https://prelude.dhall-lang.org/List/map
let List/end_map = \(t: Type) -> \(f: t -> t) -> \(a: List t) -> List/map t t f a

let eRepositoryOwnerLogin = GithubActions.mkExpression "github.event.repository.owner.login"
let eRepositoryName = GithubActions.mkExpression "github.event.repository.name"
let ePullRequestNumber = GithubActions.mkExpression "github.event.number"
let ePullRequestTitle = GithubActions.mkExpression "github.event.pull_request.title"
let ePullRequestHeadHash = GithubActions.mkExpression "github.event.pull_request.head.sha"
let ePullRequestBaseHash = GithubActions.mkExpression "github.event.pull_request.base.sha"
let eSecretGithubToken = GithubActions.mkExpression "secrets.GITHUB_TOKEN"
let eSecretAWSAccessKey = GithubActions.mkExpression "secrets.AWS_ACCESS_KEY_ID"
let eSecretAWSAccessSecret = GithubActions.mkExpression "secrets.AWS_ACCESS_SECRET"

let depends = \(deps: List Text) -> \(job: GithubActions.Job.Type) -> job // { needs = Some deps }
let withCondition = \(cond: Text) -> \(job: GithubActions.Job.Type) -> job // { `if` = Some cond }
let withConditionStep = \(cond: Text) -> \(step: GithubActions.Step.Type) -> step // { `if` = Some cond }
let runStepOnFailure = withConditionStep "failure()"

let awsAccessEnvParams : SlackNotifierAction.ExecEnv =
    { AWS_ACCESS_KEY_ID = eSecretAWSAccessKey
    , AWS_SECRET_ACCESS_KEY = eSecretAWSAccessSecret
    , AWS_DEFAULT_REGION = "ap-northeast-1"
    }

let preconditionRecordBeginTimeStep = GithubActions.Step::{
    , name = "Getting begintime"
    , id = Some "begintime"
    , run = Some "echo \"::set-output name=begintime::$(date +%s)\""
    }
let preconditionBeginTimestampOutputDef = toMap {
    , begintime = GithubActions.mkExpression "steps.begintime.outputs.begintime"
    }

let checkoutStep = ProvidedSteps.checkoutStep ProvidedSteps.CheckoutParams::{=}
let checkoutHeadStep = (ProvidedSteps.checkoutStep ProvidedSteps.CheckoutParams::{ ref = Some ePullRequestHeadHash }) // {
    , name = "Checking out (HEAD commit)"
    }

let cacheStep = GithubActions.Step::{
    , name = "Initialize Cache"
    , uses = Some "actions/cache@v2"
    , `with` = Some (toMap {
        , path =
            ''
            ~/.cargo/registry
            ~/.cargo/git
            target
            ''
        , key = "${GithubActions.mkExpression "runner.os"}-cargo-${GithubActions.mkExpression "hashFiles('**/Cargo.lock')"}"
        })
    }

let slackNotifyIfFailureStep = \(stepName: Text) -> SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Failure stepName
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , report_name = "PR Integrity Check"
    , mode = SlackNotifierAction.Mode.Diff
        { head_sha = ePullRequestHeadHash
        , base_sha = ePullRequestBaseHash
        , pr_number = ePullRequestNumber
        , pr_title = ePullRequestTitle
        }
    } awsAccessEnvParams
let slackNotifySuccessStep = SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Success
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , report_name = "PR Integrity Check"
    , mode = SlackNotifierAction.Mode.Diff
        { head_sha = ePullRequestHeadHash
        , base_sha = ePullRequestBaseHash
        , pr_number = ePullRequestNumber
        , pr_title = ePullRequestTitle
        }
    } awsAccessEnvParams

let weeklySlackNotifyAsFailureStep = \(stepName: Text) -> SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Failure stepName
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , report_name = "Weekly Check"
    , mode = SlackNotifierAction.Mode.Branch
    } awsAccessEnvParams
let weeklySlackNotifyAsSuccessStep = SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Success
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , report_name = "Weekly Check"
    , mode = SlackNotifierAction.Mode.Branch
    } awsAccessEnvParams

let SlackNotification = < Success | Failure : Text >
let SlackNotifyProvider = { Success : GithubActions.Step.Type, Failure : Text -> GithubActions.Step.Type }
let prSlackNotifyProvider =
    { Success = slackNotifySuccessStep // { name = "Notify as Success" }
    , Failure = \(phase: Text) -> slackNotifyIfFailureStep phase // { name = "Notify as Failure" }
    }
let weeklySlackNotifyProvider =
    { Success = weeklySlackNotifyAsSuccessStep // { name = "Notify as Success" }
    , Failure = \(phase: Text) -> weeklySlackNotifyAsFailureStep phase // { name = "Notify as Failure" }
    }
let slackNotify = \(provider: SlackNotifyProvider) -> \(state: SlackNotification) -> merge provider state

let checkFormats = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Code Formats"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , CodeformCheckerAction.step { script = CodeformCheckerAction.Script.codeform_check } // { name = "Running Check: Line Width" }
            , CodeformCheckerAction.step { script = CodeformCheckerAction.Script.vulnerabilities_elliminator } // { name = "Running Check: Debugging Weaks" }
            , CodeformCheckerAction.step { script = CodeformCheckerAction.Script.trailing_newline_checker } // { name = "Running Check: Trailing Newline for Source Code Files" }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-formats"))]
        ]
    }

let checkBaseLayer = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Base Layer"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/end_map GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , GithubActions.Step::{ name = "Building as Checking", uses = Some "./.github/actions/checkbuild-baselayer" }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-baselayer"))]
        ]
    }

let checkTools = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Tools"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , CheckBuildSubdirAction.step { path = "tools" }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-tools"))]
        ]
    }
let checkModules = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Modules"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , CheckBuildSubdirAction.step { path = "." }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure  "check-modules"))]
        ]
    }
let checkExamples = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Examples"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , CheckBuildSubdirAction.step { path = "examples" }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure  "check-examples"))]
        ]
    }
let checkCradleWindows = \(notifyProvider : SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Cradle(Windows)"
    , runs-on = GithubActions.RunnerPlatform.windows-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/end_map GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , GithubActions.Step::{
                , name = "cargo check"
                , run = Some
                    ''
                    $ErrorActionPreference = "Continue"
                    pwsh -c './build.ps1 windows examples/basic -RunTests -Features bedrock/DynamicLoaded' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
                    ''
                , env = Some (toMap { VK_SDK_PATH = "" })
                }
            , GithubActions.Step::{
                , name = "cargo check for transparent-back"
                , run = Some
                ''
                    pwsh -c './build.ps1 windows examples/basic -RunTests -Features "transparent,bedrock/DynamicLoaded"' *>&1 | Tee-Object $Env:GITHUB_WORKSPACE/.buildlog
                ''
                , env = Some (toMap { VK_SDK_PATH = "" })
                }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-cradle-windows"))]
        ]
    }
let checkCradleMacos = \(notifyProvider : SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Cradle(macOS)"
    , runs-on = GithubActions.RunnerPlatform.macos-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/end_map GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
            , cacheStep
            , GithubActions.Step::{
                , name = "install requirements"
                , run = Some "brew install coreutils"
            }
            , GithubActions.Step::{
                , name = "cargo check"
                , run = Some "./build.sh mac examples/basic --RunChecks 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"
                , env = Some (toMap { VK_SDK_PATH = "" })
                }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-cradle-macos"))]
        ]
    }

let reportSuccessJob = \(notifyProvider: SlackNotifyProvider) -> GithubActions.Job::{
    , name = Some "Report as Success"
    , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
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
