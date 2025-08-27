#!/bin/bash

set -o pipefail

cargo test --verbose --features bedrock/DynamicLoaded --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
