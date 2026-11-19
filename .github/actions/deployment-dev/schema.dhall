let GithubActions =
      https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall

let Params = { FirebaseToken : Text }

let step =
      λ(params : Params) →
        GithubActions.Step::{
        , name = "Deployment to Firebase Hosting (for Dev)"
        , uses = Some "./.github/actions/deployment-dev"
        , env = Some (toMap { FIREBASE_TOKEN = params.FirebaseToken })
        }

in  { Params, step }
