#!/bin/bash

set -e

cargo build --features bedrock/VK_EXT_debug_report
install_name_tool -change @rpath/vulkan.framework/Versions/A/vulkan @executable_path/vulkan.framework/Versions/A/vulkan ../../target/debug/peridot-cradle-mac
cp -r $VK_SDK_PATH/macOS/Frameworks/vulkan.framework ../../target/debug/
eval ../../target/debug/peridot-cradle-mac
