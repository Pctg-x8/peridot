let GithubActions = ../schemas/Actions.dhall
let ProvidedSteps = ../schemas/ProvidedSteps.dhall

let CodeformCheckerAction = ../../actions/codeform-checker/schema.dhall
let CheckBuildSubdirAction = ../../actions/checkbuild-subdir/schema.dhall
let SlackNotifierAction = ../../actions/integrity-check-slack-notifier/schema.dhall
let List/concat = https://prelude.dhall-lang.org/List/concat
let List/map = https://prelude.dhall-lang.org/List/map

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

let checkoutStep = ProvidedSteps.checkoutStep ProvidedSteps.CheckoutStepParams::{=}
let checkoutHeadStep = (ProvidedSteps.checkoutStep ProvidedSteps.CheckoutStepParams::{ ref = Some ePullRequestHeadHash }) // {
    , name = "Checking out (HEAD commit)"
    }

let slackNotifyIfFailureStep = \(stepName: Text) -> SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Failure stepName
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , head_sha = ePullRequestHeadHash
    , base_sha = ePullRequestBaseHash
    , pr_number = ePullRequestNumber
    , pr_title = ePullRequestTitle
    } awsAccessEnvParams
let slackNotifySuccessStep = SlackNotifierAction.step {
    , status = SlackNotifierAction.Status.Success
    , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
    , head_sha = ePullRequestHeadHash
    , base_sha = ePullRequestBaseHash
    , pr_number = ePullRequestNumber
    , pr_title = ePullRequestTitle
    } awsAccessEnvParams

let weeklySlackNotifyAsFailureStep = \(stepName: Text) -> GithubActions.Step::{
    , name = "Notify as Failure"
    , uses = Some "./.github/actions/weekly-integrity-check-slack-notifier"
    , `if` = Some "failure()"
    , env = Some (toMap awsAccessEnvParams)
    , `with` = Some (toMap {
        , status = "failure"
        , failure_step = stepName
        , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        })
    }
let weeklySlackNotifyAsSuccessStep = GithubActions.Step::{
    , name = "Notify as Success"
    , uses = Some "./.github/actions/weekly-integrity-check-slack-notifier"
    , env = Some (toMap awsAccessEnvParams)
    , `with` = Some (toMap {
        , status = "success"
        , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        })
    }

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
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure "check-formats"))]
        ]
    }

let checkBaseLayer = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Base Layer"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (withConditionStep precondition) [
            , checkoutHeadStep
            , checkoutStep
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
            , CheckBuildSubdirAction.step { path = "." }
            ]
        , [runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure  "check-modules"))]
        ]
    }
let checkExamples = \(notifyProvider: SlackNotifyProvider) -> \(precondition: Text) -> GithubActions.Job::{
    , name = Some "Examples"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , withConditionStep precondition (CheckBuildSubdirAction.step { path = "examples" })
        , slackNotify notifyProvider SlackNotification.Success
        , runStepOnFailure (slackNotify notifyProvider (SlackNotification.Failure  "check-examples"))
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
    , checkFormats
    , checkBaseLayer
    , checkTools
    , checkModules
    , checkExamples
    }
