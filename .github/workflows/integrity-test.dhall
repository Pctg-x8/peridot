let GithubActions = ./schemas/Actions.dhall
let CommonDefs = ./integrity-test/Common.dhall
let ProvidedSteps = ./schemas/ProvidedSteps.dhall

let needsOutputPath = \(jobId: Text) -> \(name: Text) -> "needs." ++ jobId ++ ".outputs." ++ name

let preconditionOutputHasChanges = GithubActions.mkExpression (needsOutputPath "preconditions" "has_code_changes" ++ " == 1")
let preconditionOutputHasWorkflowChanges = GithubActions.mkExpression (needsOutputPath "preconditions" "has_workflow_changes" ++ " == 1")
let preconditions = GithubActions.Job::{
    , name = Some "Preconditions"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , outputs = Some (CommonDefs.preconditionBeginTimestampOutputDef # toMap {
        , has_code_changes = GithubActions.mkExpression "steps.fileck.outputs.has_code_changes"
        , has_workflow_changes = GithubActions.mkExpression "steps.fileck.outputs.has_workflow_changes"
        })
    , steps = [
        , CommonDefs.preconditionRecordBeginTimeStep
        , GithubActions.Step::{
            , name = "Checking Changed Filenames"
            , id = Some "fileck"
            , run = Some
                ''
                HAS_CODE_CHANGES=0
                HAS_WORKFLOW_CHANGES=0
                QUERY_STRING='query($cursor: String) { repository(owner: \"${ CommonDefs.eRepositoryOwnerLogin }\", name: \"${ CommonDefs.eRepositoryName }\") { pullRequest(number: ${ CommonDefs.ePullRequestNumber }) { files(first: 50, after: $cursor) { nodes { path }, pageInfo { hasNextPage, endCursor } } } } }'
                QUERY_CURSOR='null'
                while :; do
                  POSTDATA="{ \"query\": \"$QUERY_STRING\", \"variables\": { \"cursor\": $QUERY_CURSOR }\" } }"
                  echo $POSTDATA
                  API_RESPONSE=$(curl -s -H "Authorization: Bearer ${ CommonDefs.eSecretGithubToken }" -X POST -d "$POSTDATA" https://api.github.com/graphql)
                  echo $API_RESPONSE
                  PATHS=$(echo $API_RESPONSE | jq ".data.repository.pullRequest.files.nodes[].path")
                  echo $PATHS
                  echo $PATHS | grep -qE '\.rs"|Cargo(\.template)?\.toml"' && :
                  if [[ $? == 0 ]]; then HAS_CODE_CHANGES=1; fi
                  echo $PATHS | grep -qE '\.dhall"' && :
                  if [[ $? == 0 ]]; then HAS_WORKFLOW_CHANGES=1; fi
                  if [[ $HAS_CODE_CHANGES == 1 && $HAS_WORKFLOW_CHANGES == 1 ]]; then break; fi
                  HAS_NEXT_PAGE=$(echo $API_RESPONSE | jq ".data.repository.pullRequest.files.pageInfo.hasNextPage")
                  if [[ "$HAS_NEXT_PAGE" == "true" ]]; then
                    QUERY_CURSOR=$(echo $API_RESPONSE | jq ".data.repository.pullRequest.files.pageInfo.endCursor")
                  else
                    break
                  fi
                done < <(cat)
                echo "HAS_CODE_CHANGES?$HAS_CODE_CHANGES"
                echo "HAS_WORKFLOW_CHANGES?$HAS_WORKFLOW_CHANGES"
                echo "::set-output name=has_code_changes::$HAS_CODE_CHANGES"
                echo "::set-output name=has_workflow_changes::$HAS_WORKFLOW_CHANGES"
                ''
            }
        ]
    }
let checkWorkflowSync = GithubActions.Job::{
    , name = Some "Check Workflow Files are Synchronized"
    , runs-on = GithubActions.RunnerPlatform.macos-latest
    , steps = [
        , ProvidedSteps.checkoutStep ProvidedSteps.CheckoutStepParams::{=}
        , GithubActions.Step::{
            , name = "setup packages"
            , run = Some "brew install dhall-yaml colordiff"
            }
        , GithubActions.Step::{
            , name = "test-sync"
            , run = Some "make -C ./.github/workflows test-sync"
            }
        , CommonDefs.runStepOnFailure (CommonDefs.slackNotifyIfFailureStep  "check-sync-workflow" // { name = "Notify as Failure" })
        ]
    }

in GithubActions.Workflow::{
    , name = Some "Integrity Check"
    , on = GithubActions.On.Detailed GithubActions.OnDetails::{
        , pull_request = Some GithubActions.OnPullRequest::{
            , types = Some [
                , GithubActions.PullRequestTriggerTypes.opened
                , GithubActions.PullRequestTriggerTypes.synchronize
                ]
            }
        }
    , jobs = toMap {
        , preconditions = preconditions
        , check-formats = CommonDefs.depends ["preconditions"] (CommonDefs.withCondition preconditionOutputHasChanges (CommonDefs.checkFormats CommonDefs.prSlackNotifyProvider))
        , check-baselayer = CommonDefs.depends ["preconditions"] (CommonDefs.withCondition preconditionOutputHasChanges (CommonDefs.checkBaseLayer CommonDefs.prSlackNotifyProvider))
        , check-tools = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.withCondition preconditionOutputHasChanges (CommonDefs.checkTools CommonDefs.prSlackNotifyProvider))
        , check-modules = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.withCondition preconditionOutputHasChanges (CommonDefs.checkModules CommonDefs.prSlackNotifyProvider))
        , check-examples = CommonDefs.depends ["preconditions", "check-modules"] (CommonDefs.withCondition preconditionOutputHasChanges (CommonDefs.checkExamples CommonDefs.prSlackNotifyProvider))
        , check-sync-workflow = CommonDefs.depends ["preconditions"] (CommonDefs.withCondition preconditionOutputHasWorkflowChanges checkWorkflowSync)
        }
    }
