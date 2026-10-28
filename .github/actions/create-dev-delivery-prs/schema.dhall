let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let Params =
      { Type = { token : Text }
      , default.token = GithubActions.mkExpression "secrets.GITHUB_TOKEN"
      }

let step =
      λ(params : Params.Type) →
        GithubActions.Step::{
        , name = "Create PullRequest"
        , uses = Some "./.github/actions/create-dev-delivery-prs"
        , env = Some (toMap { GITHUB_TOKEN = params.token })
        }

in  { Params, step }
