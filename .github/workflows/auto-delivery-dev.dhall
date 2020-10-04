let GithubActions = ./schemas/Actions.dhall
let ProvidedSteps = ./schemas/ProvidedSteps.dhall

let DeliveryPullRequestCreateAction = ../actions/create-dev-delivery-prs/schema.dhall

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
                , ProvidedSteps.checkoutStep ProvidedSteps.CheckoutStepParams::{=}
                , GithubActions.Step::{
                    , name = "Fetching all branches"
                    , run = Some "git fetch --no-tags -p --depth=1 origin +refs/heads/*:refs/remotes/origin/*"
                    }
                , DeliveryPullRequestCreateAction.step DeliveryPullRequestCreateAction.Params::{=}
                ]
            }
        }
    }
