cmake_minimum_required(VERSION 3.4.1)

# Add native_app_glue from android ndk
add_library(native_app_glue STATIC ${ANDROID_NDK}/sources/android/native_app_glue/android_native_app_glue.c)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu11 -Wall -Werror")
set(USERLIB_PATH "${CMAKE_SOURCE_DIR}/../../../../../../target/${ANDROID_ABI}-linux-android/debug/libpegamelib.a")
# Export the ANativeActivity_onCreate function
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -u ANativeActivity_onCreate")

add_library(ntv SHARED dummy.c)
target_link_libraries(ntv android native_app_glue log ${USERLIB_PATH})
