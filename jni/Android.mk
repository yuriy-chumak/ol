LOCAL_PATH := $(call my-dir)
# https://developer.android.com/ndk/guides/android_mk.html

$(info $(TARGET_ARCH))
$(info $(TARGET_ARCH_ABI))

ifeq ($(TARGET_ARCH_ABI),arm64-v8a)
   LOCAL_OBJ_ARCH:=aarch64
   LOCAL_OBJ_FORMAT:=elf64-littleaarch64
endif
ifeq ($(TARGET_ARCH_ABI),x86_64)
   LOCAL_OBJ_ARCH:=i386:x86-64
   LOCAL_OBJ_FORMAT:=elf64-x86-64
endif
ifeq ($(TARGET_ARCH_ABI),x86)
   LOCAL_OBJ_ARCH:=i386
   LOCAL_OBJ_FORMAT:=elf32-i386
endif
ifeq ($(TARGET_ARCH_ABI),mips)
   LOCAL_OBJ_ARCH:=elf32-tradlittlemips
   LOCAL_OBJ_FORMAT:=mips:isa32
endif
ifeq ($(TARGET_ARCH_ABI),armeabi)
   LOCAL_OBJ_ARCH:=armv5te
   LOCAL_OBJ_FORMAT:=elf32-littlearm
endif

obj/local/$(TARGET_ARCH_ABI)/objs/ol/repl.o: repl
	@echo [$(TARGET_ARCH_ABI)] Compile        : repl.o
	@$(TOOLCHAIN_PREFIX)objcopy -I binary -B $(LOCAL_OBJ_ARCH) -O $(LOCAL_OBJ_FORMAT) repl obj/local/$(TARGET_ARCH_ABI)/objs/ol/repl.o


include $(CLEAR_VARS)

LOCAL_MODULE    := ol
LOCAL_SRC_FILES += ../src/olvm.c repl.o
LOCAL_CFLAGS    += -std=c99 -O2 -s
LOCAL_LDFLAGS   := -Xlinker --export-dynamic

$(info $(LOCAL_SRC_FILES))

# for solving the
# "error: only position independent executables (PIE) are supported."
# uncomment next lines:
#LOCAL_CFLAGS    += -fPIC
#LOCAL_LDFLAGS   += -Xlinker -pie

include $(BUILD_EXECUTABLE)
