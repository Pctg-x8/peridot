#!/bin/bash -e

SCRIPT_PATH=$(dirname $0 | xargs realpath)
TOOL_PATH=$SCRIPT_PATH/../../target/release

(cd $SCRIPT_PATH/assets; for path in shaders/*.csh; do $TOOL_PATH/peridot-shaderbuild $path; done)

