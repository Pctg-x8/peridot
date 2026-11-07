#!/bin/sh -e

API_URL_REPO_BASE="https://api.github.com/repos/$GITHUB_REPOSITORY"
PR_URL="$API_URL_REPO_BASE/pulls"
LABEL_NAME="Changelog Auto Delivery"

HEAD_NAME=$(echo $GITHUB_REF | sed "s/refs\/heads\///")
TARGETS=$(git branch -a | sed -n "s/\s*remotes\/origin\/\(dev-.\+\)/\1/p")
for DEST in $TARGETS; do
    CHANGELOG_LIST=$(git log --oneline --format="- *%h*: %s (by %cn)" remotes/origin/$DEST..$HEAD_NAME)
    if [ $(echo $CHANGELOG_LIST | wc -l) == "0" ]; then
        echo "No diffs between $HEAD_NAME and $DEST. skipping"
    else
        POST_DATA="{\"title\": \"🚚$HEAD_NAME → $DEST🚚\", \"base\": \"$DEST\", \"head\": \"$HEAD_NAME\", \"maintainer_can_modify\": false}"
        PR_NUMBER=$(curl -X POST -H "Content-Type: application/json" -H "Authorization: Bearer $GITHUB_TOKEN" -d "$POST_DATA" $PR_URL | jq .number)
        curl -X POST -H "Content-Type: application/json" -H "Authorization: Bearer $GITHUB_TOKEN" -d "{ \"labels\": [\"$LABEL_NAME\"] }" "$API_URL_REPO_BASE/issues/$PR_NUMBER/labels"
    fi
done
