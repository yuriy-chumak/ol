LOCAL_PATH := $(call my-dir)
#!/Users/uri/Android/android-ndk-r9d/ndk-build
# NDK_APPLICATION_MK=`pwd`/Application.mk

include $(CLEAR_VARS)
LOCAL_MODULE    := ol

LOCAL_SRC_FILES := src/olvm.c src/boot.c
LOCAL_CFLAGS    += -std=c99 -O2 -DNDEBUG -ldl

include $(BUILD_EXECUTABLE)
#
#	$(CC) $(CFLAGS) src/olvm.c src/boot.c -o $@ \
#	   -Xlinker --export-dynamic -ldl
#	@echo Ok.
#CFLAGS += -std=c99 -O2 -DNDEBUG -s
