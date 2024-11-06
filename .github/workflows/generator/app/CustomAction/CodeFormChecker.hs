module CustomAction.CodeFormChecker (Script (..), step) where

import Workflow.GitHub.Actions qualified as GHA

data Script = ScriptCodeFormCheck | ScriptVulnerabilitiesEliminator | ScriptTrailingNewlineChecker

scriptText :: Script -> String
scriptText ScriptCodeFormCheck = "codeform_check"
scriptText ScriptVulnerabilitiesEliminator = "vulnerabilities_elliminator"
scriptText ScriptTrailingNewlineChecker = "trailing_newline_checker"

step :: Script -> GHA.Step
step script = GHA.env "RUN_SCRIPT" (scriptText script) $ GHA.actionStep "./.github/actions/codeform-checker" mempty
