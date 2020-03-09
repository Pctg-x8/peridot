FROM rust:latest

CMD "env CARGO_TARGET_DIR=$GITHUB_WORKSPACE/.buildcache/target CARGO_HOME=$GITHUB_WORKSPACE/.buildcache cargo check --verbose --features=bedrock/VK_EXT_debug_report"
