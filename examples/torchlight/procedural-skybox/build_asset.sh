#!/bin/bash -xe

SCRIPT_PATH=$(dirname $0 | xargs realpath)
TOOL_PATH="$SCRIPT_PATH/../../../target/release"

cd $SCRIPT_PATH/assets
for code in shaders/*.csh; do
    $TOOL_PATH/peridot-shaderbuild $code
done
echo "Compiling Precomputation Shaders......"
mkdir shaders/precompute || true
glslc shaders/transmittance_precompute.comp -o shaders/precompute/transmittance.spv
glslc shaders/single_scatter_precompute.comp -o shaders/precompute/single_scatter.spv
glslc shaders/multiple_scatter_precompute.comp -o shaders/precompute/multiple_scatter.spv
glslc shaders/gather_precompute.comp -o shaders/precompute/gather.spv
glslc shaders/accum2.comp -o shaders/precompute/accum2.spv
glslc shaders/accum3.comp -o shaders/precompute/accum3.spv
