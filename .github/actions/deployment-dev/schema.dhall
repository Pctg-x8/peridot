let GithubActions = ../../workflows/schemas/Actions.dhall

let Params = { FirebaseToken : Text }
let step = \(params: Params) -> GithubActions.Step::{
    , name = "Deployment to Firebase Hosting (for Dev)"
    , uses = Some "./.github/actions/deployment-dev"
    , env = Some (toMap { FIREBASE_TOKEN = params.FirebaseToken })
    }

in { Params, step }
