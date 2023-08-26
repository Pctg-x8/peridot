#!/bin/bash

SCRIPT_ROOT=$(basename $(realpath $BASH_SOURCE))
PROJECT_ROOT=$(basename $SCRIPT_ROOT)

git config core.hooksPath $PROJECT_ROOT/.githooks

