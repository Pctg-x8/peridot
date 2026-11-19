#!/bin/bash

set -xe

SCRIPT_ROOT=$(dirname $(realpath $BASH_SOURCE))
PROJECT_ROOT=$(dirname $SCRIPT_ROOT)

git config core.hooksPath $PROJECT_ROOT/.githooks

