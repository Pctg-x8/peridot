#!/bin/bash -xe

BEGINTIME=$(cat .begintime)
ENDTIME=$(date +%s)
BUILD_TIME_SECS=$(expr $ENDTIME - $BEGINTIME)

# build commit info
read COMMITTER_NAME COMMIT_MESSAGE < <(git log --format=%cn%x09%B -n 1 $GITHUB_SHA)
COMMIT_INFO="{\"committer\": \"$COMMITTER_NAME\", \"message\": \"$COMMIT_MESSAGE\", \"sha\": \"$GITHUB_SHA\"}"

if [ $INPUT_STATUS == "failure" ]; then
    PAYLOAD_STATUS_HEADER="\"status\": \"$INPUT_STATUS\", \"failure_step\": \"$INPUT_FAILURE_STEP\""
else
    PAYLOAD_STATUS_HEADER="\"status\": \"$INPUT_STATUS\""
fi

# urls
BUILD_URL="https://github.com/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"

# send!
PAYLOAD="{$PAYLOAD_STATUS_HEADER, \"build_url\": \"$BUILD_URL\", \"number\": \"$GITHUB_RUN_NUMBER\", \"duration\": $BUILD_TIME_SECS, \"repository\": \"$GITHUB_REPOSITORY\", \"branch_name\": \"$GITHUB_REF\", \"commit\": $COMMIT_INFO, \"weekly\": true}"
aws lambda invoke --function-name PeridotIntegrityTestNotificationGHA --invocation-type Event --payload "$PAYLOAD" out.log

# propagate failure status
[ $INPUT_STATUS == "success" ]
