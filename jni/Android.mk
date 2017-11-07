LOCAL_PATH := $(call my-dir)

# elf object formats for supported architecture
elf_format=$(or \
   $(if $(findstring $(1),armeabi),    -B armv5te              -O elf32-littlearm)\
   $(if $(findstring $(1),armeabi-v7a),-B arm                  -O elf32-littlearm)\
   $(if $(findstring $(1),x86),        -B i386                 -O elf32-i386)\
   $(if $(findstring $(1),x86_64),     -B i386:x86-64          -O elf64-x86-64)\
   $(if $(findstring $(1),arm64-v8a),  -B aarch64              -O elf64-littleaarch64)\
   $(if $(findstring $(1),mips),       -B mips:isa32 -O elf32-tradlittlemips)\
   $(if $(findstring $(1),mips64),     -B mips:isa64r6 -O elf64-tradlittlemips)\
)
filename=$(lastword $(subst /, ,$(1)))

.PHONY: $(TOOLCHAIN_PREFIX)objcopy
jni/../obj/local/$(TARGET_ARCH_ABI)/repl.o: $(TOOLCHAIN_PREFIX)objcopy jni/../obj/local/$(TARGET_ARCH_ABI)
	@$(word 1,$^) -I binary $(call elf_format,$(call filename,$(word 2,$^))) repl $@

# main ol module
include $(CLEAR_VARS)

LOCAL_MODULE    := ol
LOCAL_SRC_FILES += ../src/olvm.c ../obj/local/$(TARGET_ARCH_ABI)/repl.c
LOCAL_CFLAGS    += -std=c99 -O2 -s -DNAKED_VM
LOCAL_LDFLAGS   := -Xlinker --export-dynamic
LOCAL_LDFLAGS   += -Xlinker obj/local/$(TARGET_ARCH_ABI)/repl.o

jni/../obj/local/$(TARGET_ARCH_ABI)/repl.c: jni/../obj/local/$(TARGET_ARCH_ABI)/repl.o
	@echo // This empty file required for the stupid Android build system >$@

include $(BUILD_EXECUTABLE)
