let GithubActions = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall
let actions/checkout = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps/actions/checkout.dhall

let DeploymentAction = ../actions/deployment-dev/schema.dhall

in GithubActions.Workflow::{
    , name = Some "Document Continuous Deployment (for dev)"
    , on = GithubActions.On.Detailed GithubActions.OnDetails::{
        , push = Some GithubActions.OnPush::{
            , branches = Some ["dev"]
            , paths = Some ["**.rs", "**.toml"]
            }
        }
    , jobs = toMap {
        , doc-gen-deploy = GithubActions.Job::{
            , name = Some "Doc Generate and Deploy"
            , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
            , steps = [
                , actions/checkout.stepv3 actions/checkout.Params::{ ref = Some "dev" }
                , GithubActions.Step::{
                    , name = "Build docs"
                    , uses = Some "./.github/actions/build-doc"
                    }
                , DeploymentAction.step { FirebaseToken = GithubActions.mkExpression "secrets.DOC_HOST_FIREBASE_TOKEN" }
                ]
            }
        }
    }
