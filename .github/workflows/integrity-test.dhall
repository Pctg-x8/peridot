let GithubActions = ./schemas/Actions.dhall
let CommonDefs = ./integrity-test/Common.dhall

let preconditionOutputHasChanges = GithubActions.mkExpression "needs.preconditions.output.has_code_changes == 1"
let preconditions = GithubActions.Job::{
    , name = "Preconditions"
    , `runs-on` = GithubActions.RunnerPlatform.ubuntu-latest
    , outputs = Some (CommonDefs.preconditionBeginTimestampOutputDef # toMap {
        , has_code_changes = GithubActions.mkExpression "steps.fileck.outputs.has_code_changes"
        })
    , steps = [
        , CommonDefs.preconditionRecordBeginTimeStep
        , GithubActions.Step::{
            , name = "Checking Changed Filenames"
            , id = Some "fileck"
            , run = Some
                ''
                HAS_CODE_CHANGES=0
                QUERY_STRING='query($cursor: String) { repository(owner: \"${ CommonDefs.eRepositoryOwnerLogin }\", name: \"${ CommonDefs.eRepositoryName }\") { pullRequest(number: ${ CommonDefs.ePullRequestNumber }) { files(first: 50, after: $cursor) { nodes { path }, pageInfo { hasNextPage, endCursor } } } } }'
                QUERY_CURSOR='null'
                while :; do
                  POSTDATA="{ \"query\": \"$QUERY_STRING\", \"variables\": { \"cursor\": $QUERY_CURSOR }\" } }"
                  echo $POSTDATA
                  API_RESPONSE=$(curl -s -H "Authorization: Bearer ${ CommonDefs.eSecretGithubToken }" -X POST -d "$POSTDATA" https://api.github.com/graphql)
                  echo $API_RESPONSE
                  echo $API_RESPONSE | jq ".data.repository.pullRequest.files.nodes[].path" | grep -qE '\.rs"$|Cargo(\.template)?\.toml"$' && :
                  if [[ $? == 0 ]]; then HAS_CODE_CHANGES=1; break; fi
                  HAS_NEXT_PAGE=$(echo $API_RESPONSE | jq ".data.repository.pullRequest.files.pageInfo.hasNextPage")
                  if [[ "$HAS_NEXT_PAGE" == "true" ]]; then
                    QUERY_CURSOR=$(echo $API_RESPONSE | jq ".data.repository.pullRequest.files.pageInfo.endCursor")
                  else
                    break
                  fi
                done < <(cat)
                echo "::set-output name=has_code_changes::$HAS_CODE_CHANGES"
                ''
            }
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
        }
    }
