let GithubActions = ../../workflows/schemas/Actions.dhall

let Script = < codeform_check | vulnerabilities_elliminator | trailing_newline_checker >
let scriptAsText =
    let handler =
        { codeform_check = "codeform_check"
        , vulnerabilities_elliminator = "vulnerabilities_elliminator"
        , trailing_newline_checker = "trailing_newline_checker"
        }
    in \(s: Script) -> merge handler s

let Params = { script : Script }
let step = \(params: Params) -> GithubActions.Step::{
    , name = "RunScript"
    , uses = Some "./.github/actions/codeform-checker"
    , `with` = Some (toMap { script = scriptAsText params.script })
    }

in { Script, Params, step }
