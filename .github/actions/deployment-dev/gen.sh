#!/bin/sh -xe

# Copy generated docs for deployment
rsync --delete -auv $GITHUB_WORKSPACE/target/doc/ /doc/public/dev
cd /doc
firebase deploy --project docs-peridot
