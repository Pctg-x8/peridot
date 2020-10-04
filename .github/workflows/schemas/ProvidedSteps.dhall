let GithubActions = ./Actions.dhall
let Map = https://prelude.dhall-lang.org/Map/Type

let CheckoutStepParams =
    { Type = { ref : Optional Text }
    , default = { ref = None Text }
    }
let checkoutStep = \(params: CheckoutStepParams.Type) -> GithubActions.Step::{
    , name = "Checking out"
    , uses = Some "actions/checkout@v2"
    , `with` = Some (merge { Some = \(x: Text) -> [{ mapKey = "ref", mapValue = x }], None = [] : Map Text Text } params.ref)
    }

let UploadArtifactParams =
    { Type = { name : Text, path : Text }
    , default = {=}
    }
let uploadArtifactStep = \(params: UploadArtifactParams.Type) -> GithubActions.Step::{
    , name = "Uploading Artifacts"
    , uses = Some "actions/upload-artifact@v1"
    , `with` = Some (toMap params)
    }

in { CheckoutStepParams, checkoutStep, UploadArtifactParams, uploadArtifactStep }
