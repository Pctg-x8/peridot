let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let CommonDefs = ./integrity-test/Common.dhall

let SlackNotifierAction =
      https://raw.githubusercontent.com/Pctg-x8/ci-notifications-post-invoker/master/schema.dhall

let preconditions =
      GithubActions.Job::{
      , name = Some "Preconditions"
      , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
      , outputs = Some CommonDefs.preconditionBeginTimestampOutputDef
      , steps = [ CommonDefs.preconditionRecordBeginTimeStep ]
      }

let successPrerequisites =
      [ "preconditions"
      , "check-examples"
      , "check-formats"
      , "check-cradle-windows"
      , "check-cradle-macos"
      , "check-cradle-linux"
      ]

let slackNotifyProvider
    : CommonDefs.SlackNotifyProvider
    = let succ =
              SlackNotifierAction.step
                { status = SlackNotifierAction.Status.Success
                , begintime =
                    GithubActions.mkExpression
                      "needs.preconditions.outputs.begintime"
                , report_name = "Weekly Check"
                , mode = SlackNotifierAction.Mode.Branch
                }
            ⫽ { name = "Notify as Success" }

      let fail =
            λ(phase : Text) →
                SlackNotifierAction.step
                  { status = SlackNotifierAction.Status.Failure phase
                  , begintime =
                      GithubActions.mkExpression
                        "needs.preconditions.outputs.begintime"
                  , report_name = "Weekly Check"
                  , mode = SlackNotifierAction.Mode.Branch
                  }
              ⫽ { name = "Notify as Failure" }

      in  { Success = succ, Failure = fail }

in  GithubActions.Workflow::{
    , name = Some "Integrity Check (Weekly)"
    , on =
        GithubActions.On.Detailed
          GithubActions.OnDetails::{
          , schedule = Some
            [ GithubActions.Schedule::{ cron = "0 12 * * wed" } ]
          }
    , jobs = toMap
        { preconditions
        , check-formats =
            CommonDefs.depends
              [ "preconditions" ]
              (CommonDefs.checkFormats slackNotifyProvider "true")
        , check-baselayer =
            CommonDefs.depends
              [ "preconditions" ]
              (CommonDefs.checkBaseLayer slackNotifyProvider "true")
        , check-tools =
            CommonDefs.depends
              [ "preconditions", "check-baselayer" ]
              (CommonDefs.checkTools slackNotifyProvider "true")
        , check-modules =
            CommonDefs.depends
              [ "preconditions", "check-baselayer" ]
              (CommonDefs.checkModules slackNotifyProvider "true")
        , check-examples =
            CommonDefs.depends
              [ "preconditions", "check-modules" ]
              (CommonDefs.checkExamples slackNotifyProvider "true")
        , check-cradle-windows =
            CommonDefs.depends
              [ "preconditions", "check-modules" ]
              (CommonDefs.checkCradleWindows slackNotifyProvider "true")
        , check-cradle-macos =
            CommonDefs.depends
              [ "preconditions", "check-modules" ]
              (CommonDefs.checkCradleMacos slackNotifyProvider "true")
        , check-cradle-linux =
            CommonDefs.depends
              [ "preconditions", "check-modules" ]
              (CommonDefs.checkCradleLinux slackNotifyProvider "true")
        , report-success =
            CommonDefs.depends
              successPrerequisites
              (CommonDefs.reportSuccessJob slackNotifyProvider)
        }
    }
