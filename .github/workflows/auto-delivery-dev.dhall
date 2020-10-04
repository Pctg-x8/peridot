let GithubActions = ./schemas/Actions.dhall

in GithubActions.Workflow::{
    , name = Some "SubProject-dev Auto Deliveries"
    , on = GithubActions.On.Detailed GithubActions.OnDetails::{
        , push = Some GithubActions.OnPush::{ branches = Some ["dev"] }
        }
    , jobs = toMap {
        , make-pr-for-deliver = GithubActions.Job::{
            , name = Some "Make Delivering PullRequest"
            , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
            , steps = [
                , GithubActions.Step::{
                    , name = "Checking out"
                    , uses = Some "actions/checkout@v2"
                    }
                , GithubActions.Step::{
                    , name = "Fetching all branches"
                    , run = Some "git fetch --no-tags -p --depth=1 origin +refs/heads/*:refs/remotes/origin/*"
                    }
                , GithubActions.Step::{
                    , name = "Create PullRequests"
                    , uses = Some "./.github/actions/create-dev-delivery-prs"
                    , `with` = Some (toMap { token = GithubActions.mkExpression "secrets.GTIHUB_TOKEN" })
                    }
                ]
            }
        }
    }
