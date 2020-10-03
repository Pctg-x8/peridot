let GithubActions = ../schemas/Actions.dhall

let eRepositoryOwnerLogin = GithubActions.mkExpression "github.event.repository.owner.login"
let eRepositoryName = GithubActions.mkExpression "github.event.repository.name"
let ePullRequestNumber = GithubActions.mkExpression "github.event.number"
let ePullRequestTitle = GithubActions.mkExpression "github.event.pull_request.title"
let ePullRequestHeadHash = GithubActions.mkExpression "github.event.pull_request.head.sha"
let ePullRequestBaseHash = GithubActions.mkExpression "github.event.pull_request.base.sha"
let eGithubOriginHash = GithubActions.mkExpression "github.sha"
let eSecretGithubToken = GithubActions.mkExpression "secrets.GITHUB_TOKEN"
let eSecretAWSAccessKey = GithubActions.mkExpression "secrets.AWS_ACCESS_KEY_ID"
let eSecretAWSAccessSecret = GithubActions.mkExpression "secrets.AWS_ACCESS_SECRET"

let awsAccessEnvParams =
    { AWS_ACCESS_KEY_ID = eSecretAWSAccessKey
    , AWS_SECRET_ACCESS_KEY = eSecretAWSAccessSecret
    , AWS_DEFAULT_REGION = "ap-northeast-1"
    }

let preconditionRecordBeginTimeStep = GithubActions.Step::{
    , name = "Getting begintime"
    , id = Some "begintime"
    , run = Some "echo \"::set:output name=begintime::$(date +%s)\""
    }

let checkoutStep = GithubActions.Step::{
    , name = "Checking out"
    , uses = Some "actions/checkout@v2"
    }
let checkoutHeadStep = checkoutStep // {
    , name = "Checking out (HEAD commit)"
    , `with` = Some (toMap { ref = ePullRequestHeadHash })
    }
let runCodeformCheckerStep = \(name: Text) -> \(script: Text) -> GithubActions.Step::{
    , name = name
    , uses = Some "./.github/actions/codeform-checker"
    , `with` = Some (toMap { script = script })
    }
let runSubdirectoryCheckStep = \(path: Text) -> GithubActions.Step::{
    , name = "Building as Checking"
    , uses = Some "./.github/actions/checkbuild-subdir"
    , `with` = Some (toMap { path = path })
    }

let slackNotifyIfFailureStep = \(stepName: Text) -> GithubActions.Step::{
    , name = "Notify as Failure"
    , uses = Some "./.github/actions/integrity-check-slack-notifier"
    , `if` = Some "failure()"
    , env = Some (toMap awsAccessEnvParams)
    , `with` = Some (toMap {
        , status = "failure"
        , failure_step = stepName
        , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        , head_sha = ePullRequestHeadHash
        , base_sha = ePullRequestBaseHash
        , pr_number = ePullRequestNumber
        , pr_title = ePullRequestTitle
        })
    }
let slackNotifySuccessStep = GithubActions.Step::{
    , name = "Notify as Success"
    , uses = Some "./.github/actions/integrity-check-slack-notifier"
    , env = Some (toMap awsAccessEnvParams)
    , `with` = Some (toMap {
        , status = "success"
        , begintime = GithubActions.mkExpression "needs.preconditions.outputs.begintime"
        , head_sha = ePullRequestHeadHash
        , base_sha = ePullRequestBaseHash
        , pr_number = ePullRequestNumber
        , pr_title = ePullRequestTitle
        })
    }

let checkFormats = GithubActions.Job::{
    , name = "Code Formats"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , runCodeformCheckerStep "Running Check: Line Width" "codeform_check"
        , runCodeformCheckerStep "Running Check: Debugging Weaks" "vulnerabilities_elliminator"
        , slackNotifyIfFailureStep "check-formats"
        ]
    }

let checkBaseLayer = GithubActions.Job::{
    , name = "Base Layer"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , GithubActions.Step::{ name = "Building as Checking", uses = Some "./.github/actions/checkbuild-baselayer" }
        , slackNotifyIfFailureStep "check-baselayer"
        ]
    }

let checkTools = GithubActions.Job::{
    , name = "Tools"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , runSubdirectoryCheckStep "tools"
        , slackNotifyIfFailureStep "check-tools"
        ]
    }
let checkModules = GithubActions.Job::{
    , name = "Modules"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , runSubdirectoryCheckStep "."
        , slackNotifyIfFailureStep "check-modules"
        ]
    }
let checkExamples = GithubActions.Job::{
    , name = "Examples"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = [
        , checkoutHeadStep
        , checkoutStep
        , runSubdirectoryCheckStep "examples"
        , slackNotifySuccessStep
        , slackNotifyIfFailureStep "check-examples"
        ]
    }

let depends = \(deps: List Text) -> \(job: GithubActions.Job.Type) -> job // { needs = Some deps }
let withCondition = \(cond: Text) -> \(job: GithubActions.Job.Type) -> job // { `if` = Some cond }

in  { depends
    , withCondition
    , preconditionRecordBeginTimeStep
    , eRepositoryOwnerLogin
    , eRepositoryName
    , ePullRequestNumber
    , eSecretGithubToken
    , checkFormats
    , checkBaseLayer
    , checkTools
    , checkModules
    , checkExamples
    }
