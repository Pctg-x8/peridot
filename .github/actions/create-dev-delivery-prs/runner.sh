#!/bin/sh -e

PR_URL="https://api.github.com/repos/$GITHUB_REPOSITORY/pulls"

HEAD_NAME=$(echo $GITHUB_REF | sed "s/refs\/heads\///")
TARGETS=$(git branch -a | sed -n "s/\s*remotes\/origin\/\(dev-.\+\)/\1/p")
for DEST in $TARGETS; do
    CHANGELOG_LIST=$(git log --oneline --format="- *%h*: %s (by %cn)" remotes/origin/$DEST..$HEAD_NAME)
    POST_DATA="{\"title\": \"🚚[CHANGELOG-AUTO-DELIVERY]$HEAD_NAME → $DEST🚚\", \"body\": \"## ChangeLogs\\n\\n$CHANGELOG_LIST\", \"base\": \"$DEST\", \"head\": \"$HEAD_NAME\"}"
    curl -X POST -H "Content-Type: application/json" -H "Authorization: Bearer $INPUT_TOKEN" -d "$POST_DATA" $PR_URL
done