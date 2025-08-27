#!/bin/bash

cargo test --verbose --features bedrock/DynamicLoaded --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
