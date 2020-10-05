let Map = https://prelude.dhall-lang.org/Map/Type
let id = https://prelude.dhall-lang.org/Function/identity

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

let RunnerPlatform = < `ubuntu-latest` | `windows-latest` | `macos-latest` | Custom: Text >
let runnerPlatformAsText =
    let handler =
        { ubuntu-latest = "ubuntu-latest"
        , windows-latest = "windows-latest"
        , macos-latest = "macos-latest"
        , Custom = id Text
        }
    in \(p: RunnerPlatform) -> merge handler p

let Strategy =
    { Type =
        { matrix : Optional (Map Text (List Text))
        , fail-fast : Optional Bool
        , max-parallel : Optional Integer
        }
    , default =
        { matrix = None (Map Text (List Text))
        , fail-fast = None Bool
        , max-parallel = None Integer
        }
    }

let Job =
    { Type =
        { name : Optional Text
        , `runs-on` : RunnerPlatform
        , strategy : Optional Strategy.Type
        , needs : Optional (List Text)
        , `if`: Optional Text
        , outputs : Optional (Map Text Text)
        , steps : List Step.Type
        }
    , default =
        { name = None Text
        , outputs = None (Map Text Text)
        , strategy = None Strategy.Type
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
   , runnerPlatformAsText
   , Strategy
   , Step
   , mkExpression
   } /\ Triggers
