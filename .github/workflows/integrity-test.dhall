let GithubActions = ./schemas/Actions.dhall
let CommonDefs = ./integrity-test/Common.dhall

in GithubActions.Workflow::{
    , name = "Integrity Check"
    , on = GithubActions.On::{
        , pull_request = Some GithubActions.OnPullRequest::{
            , types = [
                , GithubActions.PullRequestTriggerTypes.opened
                , GithubActions.PullRequestTriggerTypes.synchronize
                ]
            }
        }
    , jobs = toMap {
        , preconditions = CommonDefs.preconditions
        , check-formats = CommonDefs.depends ["preconditions"] CommonDefs.checkFormats
        , check-baselayer = CommonDefs.depends ["preconditions"] CommonDefs.checkBaseLayer
        , check-tools = CommonDefs.depends ["preconditions", "check-baselayer"] CommonDefs.checkTools
        , check-modules = CommonDefs.depends ["preconditions", "check-baselayer"] CommonDefs.checkModules
        , check-examples = CommonDefs.depends ["preconditions", "check-modules"] CommonDefs.checkExamples
        }
    }
