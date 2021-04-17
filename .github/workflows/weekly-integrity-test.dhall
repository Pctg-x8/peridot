let GithubActions = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall
let CommonDefs = ./integrity-test/Common.dhall

let preconditions = GithubActions.Job::{
    , name = Some "Preconditions"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , outputs = Some CommonDefs.preconditionBeginTimestampOutputDef
    , steps = [
        , CommonDefs.preconditionRecordBeginTimeStep
        ]
    }

let successPrerequisites =
    [ "preconditions"
    , "check-examples"
    , "check-formats"
    , "check-cradle-windows"
    , "check-cradle-macos"
    ]
in GithubActions.Workflow::{
    , name = Some "Integrity Check (Weekly)"
    , on = GithubActions.On.Detailed GithubActions.OnDetails::{
        , schedule = Some [GithubActions.Schedule::{ cron = "0 12 * * wed" }]
        }
    , jobs = toMap {
        , preconditions = preconditions
        , check-formats = CommonDefs.depends ["preconditions"] (CommonDefs.checkFormats CommonDefs.weeklySlackNotifyProvider "true")
        , check-baselayer = CommonDefs.depends ["preconditions"] (CommonDefs.checkBaseLayer CommonDefs.weeklySlackNotifyProvider "true")
        , check-tools = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.checkTools CommonDefs.weeklySlackNotifyProvider "true")
        , check-modules = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.checkModules CommonDefs.weeklySlackNotifyProvider "true")
        , check-examples = CommonDefs.depends ["preconditions", "check-modules"] (CommonDefs.checkExamples CommonDefs.weeklySlackNotifyProvider "true")
        , check-cradle-windows = CommonDefs.depends ["preconditions", "check-modules"] (CommonDefs.checkCradleWindows CommonDefs.weeklySlackNotifyProvider "true")
        , check-cradle-macos = CommonDefs.depends ["preconditions", "check-modules"] (CommonDefs.checkCradleMacos CommonDefs.weeklySlackNotifyProvider "true")
        , report-success = CommonDefs.depends successPrerequisites (CommonDefs.reportSuccessJob CommonDefs.weeklySlackNotifyProvider)
        }
    }
