#!/bin/bash

set -e

org=`pwd`
cd $CONFIGURATION_BUILD_DIR/$EXECUTABLE_FOLDER_PATH
echo "Creating alias to $CONFIGURATION_BUILD_DIR/$EXECUTABLE_FOLDER_PATH/libvulkan.1.1.92.dylib"
ln -sf libvulkan.1.1.92.dylib libvulkan.dylib

echo "Replacing $CONFIGURATION_BUILD_DIR/$UNLOCALIZED_RESOURCES_FOLDER_PATH/vulkan/**/*.json"
for f in $CONFIGURATION_BUILD_DIR/$UNLOCALIZED_RESOURCES_FOLDER_PATH/vulkan/**/*.json; do
	echo "Processing $f..."
	sed -ie 's/..\/..\/..\/lib/..\/..\/..\/Frameworks/g' $f
done

