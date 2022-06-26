cmake_minimum_required(VERSION 3.4.1)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu11 -Wall -Werror")
set(USERLIB_PATH "${CMAKE_SOURCE_DIR}/jniLibs/${ANDROID_ABI}/libpegamelib.so")
# Export the ANativeActivity_onCreate function
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -u ANativeActivity_onCreate")

add_library(ntv SHARED dummy.c)
target_link_libraries(ntv android log ${USERLIB_PATH})
