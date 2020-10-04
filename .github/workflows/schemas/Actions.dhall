let Map = https://prelude.dhall-lang.org/Map/Type

let mkExpression = \(x: Text) -> "\${{ ${x} }}"

let Step =
    { Type =
        { name : Text
        , id : Optional Text
        , `if` : Optional Text
        , run : Optional Text
        , uses : Optional Text
        , env : Optional (Map Text Text)
        , `with` : Optional (Map Text Text)
        }
    , default =
        { id = None Text
        , `if` = None Text
        , run = None Text
        , uses = None Text
        , env = None (Map Text Text)
        , `with` = None (Map Text Text)
        }
    }

let RunnerPlatform = < `ubuntu-latest` | Custom: Text >

let Job =
    { Type =
        { name : Text
        , `runs-on` : RunnerPlatform
        , needs : Optional (List Text)
        , `if`: Optional Text
        , outputs : Optional (Map Text Text)
        , steps : List Step.Type
        }
    , default =
        { outputs = None (Map Text Text)
        , needs = None (List Text)
        , `if` = None Text
        }
    }

let Triggers = ./Triggers.dhall

let Workflow =
    { Type =
        { name : Optional Text
        , on : Triggers.On
        , jobs : Map Text Job.Type
        }
    , default = { name = None Text }
    }

in { Workflow
   , Job
   , RunnerPlatform
   , Step
   , mkExpression
   } /\ Triggers
