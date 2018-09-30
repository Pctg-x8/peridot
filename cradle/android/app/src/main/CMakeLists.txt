cmake_minimum_required(VERSION 3.4.1)

# Add native_app_glue from android ndk
add_library(native_app_glue STATIC ${ANDROID_NDK}/sources/android/native_app_glue/android_native_app_glue.c)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu11 -Wall -Werror")
# Export the ANativeActivity_onCreate function
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -u ANativeActivity_onCreate")

add_library(infinitesweeper SHARED dummy.c)
target_link_libraries(infinitesweeper
    android native_app_glue log ${CMAKE_SOURCE_DIR}/../../../peridot-cradle-android/target/${ANDROID_ABI}-linux-android/debug/libinfinitesweeper.a)
