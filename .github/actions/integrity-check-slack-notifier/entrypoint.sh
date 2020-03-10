#!/bin/bash -xe

# todo: 算出方法を考える
BUILD_TIME_SECS=0

# build commit info
git log --format=%cn\t%B -n 1 $INPUT_HEAD_SHA
git log --format=%cn\t%B -n 1 $INPUT_HEAD_SHA | read COMMITTER_NAME COMMIT_MESSAGE
COMMIT_INFO="{\"committer\": \"$COMMITTER_NAME\", \"message\": \"$COMMIT_MESSAGE\", \"sha\": \"$INPUT_HEAD_SHA\"}"

if [ $INPUT_STATUS == "failure" ]; then
    PAYLOAD_STATUS_HEADER="\"status\": \"$INPUT_STATUS\", \"failure_step\": \"$INPUT_FAILURE_STEP\""
else
    PAYLOAD_STATUS_HEADER="\"status\": \"$INPUT_STATUS\""
fi

# urls
BUILD_URL="https://github.com/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"
COMPARE_URL="https://github.com/$GITHUB_REPOSITORY/compare/$INPUT_BASE_SHA..$INPUT_HEAD_SHA"

# send!
PAYLOAD="{$PAYLOAD_STATUS_HEADER, \"build_url\": \"$BUILD_URL\", \"number\": \"$GITHUB_RUN_NUMBER\", \"duration\": $BUILD_TIME_SECS, \"compare_url\": \"$COMPARE_URL\", \"commit_hash\": \"$INPUT_HEAD_SHA\", \"repository\": \"$GITHUB_REPOSITORY\", \"ref\": \"$GITHUB_HEAD_REF\", \"pr_number\": $INPUT_PR_NUMBER, \"commit\": $COMMIT_INFO}"
aws lambda invoke --function-name PeridotIntegrityTestNotificationGHA --invocation-type Event --payload "$PAYLOAD" out.log

# propagate failure status
[ $INPUT_STATUS == "failure" ] && exit 1
