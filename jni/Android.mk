LOCAL_PATH := $(call my-dir)
 
include $(CLEAR_VARS)
 
LOCAL_MODULE    := ol
LOCAL_SRC_FILES := ../src/olvm.c ../src/boot.c
LOCAL_CFLAGS    += -std=c99 -O2 -s -DNDEBUG

include $(BUILD_EXECUTABLE)
