#!/bin/bash

set -o pipefail

cargo test --verbose --features ci-nolib,bedrock/DynamicLoaded --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
