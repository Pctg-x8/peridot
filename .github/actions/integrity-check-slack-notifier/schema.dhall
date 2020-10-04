let GithubActions = ../../workflows/schemas/Actions.dhall

let Status = < Success | Failure : Text >
let decomposeStatus =
    let handler =
        { Success = toMap { status = "success" }
        , Failure = \(stepName: Text) -> toMap { status = "failure", failure_step = stepName }
        }
    in \(status: Status) -> merge handler status
let Params =
    { status : Status
    , begintime : Text
    , head_sha : Text
    , base_sha : Text
    , pr_number : Text
    , pr_title : Text
    }
let ExecEnv =
    { AWS_ACCESS_KEY_ID : Text
    , AWS_SECRET_ACCESS_KEY : Text
    , AWS_DEFAULT_REGION : Text
    }
let step = \(params: Params) -> \(env: ExecEnv) -> GithubActions.Step::{
    , name = "Notify"
    , uses = Some "./.github/actions/integrity-check-slack-notifier"
    , env = Some (toMap env)
    , `with` = Some (decomposeStatus params.status # toMap {
        , begintime = params.begintime
        , head_sha = params.head_sha
        , base_sha = params.base_sha
        , pr_number = params.pr_number
        , pr_title = params.pr_title
        })
    }

in { Status, Params, ExecEnv, step }
