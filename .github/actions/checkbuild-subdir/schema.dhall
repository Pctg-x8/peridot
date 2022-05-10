let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let Params = { path : Text }

let step =
      λ(params : Params) →
        GithubActions.Step::{
        , name = "Building as Checking"
        , uses = Some "./.github/actions/checkbuild-subdir"
        , env = Some (toMap { TARGET_PATH = params.path })
        }

in  { Params, step }
