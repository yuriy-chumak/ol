LOCAL_PATH := $(call my-dir)
 
include $(CLEAR_VARS)

# https://developer.android.com/ndk/guides/android_mk.html 
LOCAL_MODULE    := ol
LOCAL_SRC_FILES := ../src/olvm.c ../src/boot.c
LOCAL_CFLAGS    += -std=c99 -O2 -s
LOCAL_LDFLAGS   := -Xlinker --export-dynamic

include $(BUILD_EXECUTABLE)
