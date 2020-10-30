let GithubActions = ../../workflows/schemas/Actions.dhall

let Params = 
    { Type = { token : Text }
    , default = { token = GithubActions.mkExpression "secrets.GITHUB_TOKEN" }
    }
let step = \(params: Params.Type) -> GithubActions.Step::{
    , name = "Create PullRequest"
    , uses = Some "./.github/actions/create-dev-delivery-prs"
    , `with` = Some (toMap { token = params.token })
    }

in { Params, step }
