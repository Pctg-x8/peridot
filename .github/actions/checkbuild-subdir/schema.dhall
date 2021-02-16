let GithubActions = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let Params = { path : Text }
let step = \(params: Params) -> GithubActions.Step::{
    , name = "Building as Checking"
    , uses = Some "./.github/actions/checkbuild-subdir"
    , `with` = Some (toMap { path = params.path })
    }

in { Params, step }
