#!/bin/bash

set -o pipefail

cargo check --all-features --message-format=json | $HOME/.local/bin/cargo-json-gha-translator
