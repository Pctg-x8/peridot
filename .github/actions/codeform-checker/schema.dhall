let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let Script =
      < codeform_check
      | vulnerabilities_elliminator
      | trailing_newline_checker
      >

let scriptAsText =
      let handler =
            { codeform_check = "codeform_check"
            , vulnerabilities_elliminator = "vulnerabilities_elliminator"
            , trailing_newline_checker = "trailing_newline_checker"
            }

      in  λ(s : Script) → merge handler s

let Params = { script : Script }

let step =
      λ(params : Params) →
        GithubActions.Step::{
        , name = "RunScript"
        , uses = Some "./.github/actions/codeform-checker"
        , env = Some (toMap { RUN_SCRIPT = scriptAsText params.script })
        }

in  { Script, Params, step }
