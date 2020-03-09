#!/bin/bash -xe

export BUILD_TIME_SECS=0

export COMMIT_INFO="{\"committer\": \"$INPUT_COMMITTER_NAME\", \"message\": \"$INPUT_COMMIT_MESSAGE\"}"
export PAYLOAD="{\"status\": \"$INPUT_STATUS\", \"build_url\", \"https://github.com/Pctg-x8/peridot/actions/runs/$GITHUB_RUN_ID\", \"number\": \"$GITHUB_RUN_NUMBER\", \"duration\": $BUILD_TIME_SECS, \"compare_url\": \"$INPUT_COMPARE_URL\", \"commit_hash\": \"$GITHUB_SHA\", \"repository\": \"$GITHUB_REPOSITORY\", \"ref\": \"$GITHUB_HEAD_REF\", \"pr_number\": $INPUT_PR_NUMBER, \"commit\": $COMMIT_INFO}"
aws lambda invoke --function-name PeridotIntegrityTestNotificationGHA --invocation-type Event --payload $PAYLOAD out.log
