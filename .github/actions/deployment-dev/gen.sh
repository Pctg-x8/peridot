#!/bin/sh

cd $GITHUB_WORKSPACE

# Copy generated docs for deployment
rsync --delete -auv ./target/doc/ ./doc/public/dev
cd doc
firebase deploy --token $FIREBASE_TOKEN --project docs-peridot
