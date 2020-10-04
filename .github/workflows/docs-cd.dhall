let GithubActions = ./schemas/Actions.dhall

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
            , name = "Doc Generate and Deploy"
            , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
            , steps = [
                , GithubActions.Step::{
                    , name = "Checking out"
                    , uses = Some "actions/checkout@v2"
                    , `with` = Some (toMap { ref = "dev" })
                    }
                , GithubActions.Step::{
                    , name = "Build docs"
                    , uses = Some "./.github/actions/build-doc"
                    }
                , GithubActions.Step::{
                    , name = "Deployment to Firebase Hosting (for dev)"
                    , uses = Some "./.github/actions/deployment-dev"
                    , env = Some (toMap { FIREBASE_TOKEN = GithubActions.mkExpression "secrets.DOC_HOST_FIREBASE_TOKEN" })
                    }
                ]
            }
        }
    }
