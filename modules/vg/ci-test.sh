#!/bin/bash

set -o pipefail

cargo test --features ci-nolib,bedrock/DynamicLoaded --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
