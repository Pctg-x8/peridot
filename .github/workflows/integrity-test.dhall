let GithubActions = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/schema.dhall
let ProvidedSteps = https://raw.githubusercontent.com/Pctg-x8/gha-schemas/master/ProvidedSteps.dhall
let CommonDefs = ./integrity-test/Common.dhall
let List/map = https://prelude.dhall-lang.org/List/map
let List/concat = https://prelude.dhall-lang.org/List/concat

let needsOutputPath = \(jobId: Text) -> \(name: Text) -> "needs.${jobId}.outputs.${name}"

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

let installDhallScript =
    let releaseAssetSelector = "nodes { name, downloadUrl }, pageInfo { hasNextPage, endCursor }"
    let releaseSelector = "nodes { releaseAssets(first: 10, after: $cursor) { ${ releaseAssetSelector } } }"
    let repositorySelector = "releases(first: 1, orderBy: { direction: DESC, field: CREATED_AT }) { ${ releaseSelector } }"
    let query = "query($cursor: String) { repository(owner: \\\"dhall-lang\\\", name: \\\"dhall-haskell\\\") { ${ repositorySelector } } }"
    in ''
    QUERY_STRING='${query}'
    QUERY_CURSOR='null'
    TARGET_FILE=""
    while :; do
      POSTDATA="{ \"query\": \"$QUERY_STRING\", \"variables\": { \"cursor\": $QUERY_CURSOR } }"
      API_RESPONSE=$(curl -s -H "Authorization: Bearer ${ CommonDefs.eSecretGithubToken }" -X POST -d "$POSTDATA" https://api.github.com/graphql)
      TARGET_FILE=$(echo $API_RESPONSE | jq -r '.data.repository.releases.nodes[0].releaseAssets.nodes[] | select(.name | startswith("dhall-yaml") and contains("-linux")).downloadUrl')
      if [[ $TARGET_FILE != "" ]]; then break; fi
      HAS_NEXT_PAGE=$(echo $API_RESPONSE | jq ".data.repository.releases.nodes[0].releaseAssets.pageInfo.hasNextPage")
      if [[ "$HAS_NEXT_PAGE" == "true" ]]; then
        QUERY_CURSOR=$(echo $API_RESPONSE | jq ".data.repository.releases.nodes[0].releaseAssets.pageINfo.endCursor")
      else
        echo "Latest dhall release does not contains dhall-yaml for linux platform!"
        exit 1
      fi
    done < <(cat)
    echo "$TARGET_FILE"
    mkdir $HOME/dhall
    curl -L $TARGET_FILE | tar x --bzip2 -C $HOME/dhall
    echo "$HOME/dhall/bin" >> $GITHUB_PATH
    sudo apt-get update
    sudo apt-get install -y colordiff
    ''
let checkWorkflowSync = GithubActions.Job::{
    , name = Some "Check Workflow Files are Synchronized"
    , runs-on = GithubActions.RunnerPlatform.ubuntu-latest
    , steps = List/concat GithubActions.Step.Type [
        , List/map GithubActions.Step.Type GithubActions.Step.Type (CommonDefs.withConditionStep preconditionOutputHasWorkflowChanges) [
            , CommonDefs.checkoutHeadStep
            , ProvidedSteps.checkoutStep ProvidedSteps.CheckoutParams::{=}
            , GithubActions.Step::{ name = "Setup Dhall", run = Some installDhallScript }
            , GithubActions.Step::{ name = "test-sync", run = Some "make -C ./.github/workflows test-sync" }
            ]
        , [CommonDefs.runStepOnFailure (CommonDefs.slackNotifyIfFailureStep  "check-sync-workflow" // { name = "Notify as Failure" })]
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
        , check-formats = CommonDefs.depends ["preconditions"] (CommonDefs.checkFormats CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , check-baselayer = CommonDefs.depends ["preconditions"] (CommonDefs.checkBaseLayer CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , check-tools = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.checkTools CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , check-modules = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.checkModules CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , check-examples = CommonDefs.depends ["preconditions", "check-modules"] (CommonDefs.checkExamples CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , check-sync-workflow = CommonDefs.depends ["preconditions"] checkWorkflowSync
        , check-cradle-windows = CommonDefs.depends ["preconditions", "check-baselayer"] (CommonDefs.checkCradleWindows CommonDefs.prSlackNotifyProvider preconditionOutputHasChanges)
        , report-success = CommonDefs.depends ["preconditions", "check-examples", "check-formats", "check-sync-workflow"] (CommonDefs.reportSuccessJob CommonDefs.prSlackNotifyProvider)
        }
    }
