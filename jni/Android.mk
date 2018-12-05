LOCAL_PATH := $(call my-dir)

# elf object formats for supported architectures
elf_format=$(or \
   $(if $(findstring $(1),armeabi),    -B armv5te              -O elf32-littlearm),\
   $(if $(findstring $(1),armeabi-v7a),-B arm                  -O elf32-littlearm),\
   $(if $(findstring $(1),x86),        -B i386                 -O elf32-i386),\
   $(if $(findstring $(1),x86_64),     -B i386:x86-64          -O elf64-x86-64),\
   $(if $(findstring $(1),arm64-v8a),  -B aarch64              -O elf64-littleaarch64),\
   $(if $(findstring $(1),mips),       -B mips:isa32 -O elf32-tradlittlemips),\
   $(if $(findstring $(1),mips64),     -B mips:isa64r6 -O elf64-tradlittlemips)\
)
filename=$(lastword $(subst /, ,$(1)))


# https://balau82.wordpress.com/2012/02/19/linking-a-binary-blob-with-gcc/
.PHONY: $(TOOLCHAIN_PREFIX)objcopy
jni/../obj/local/$(TARGET_ARCH_ABI)/repl.o: $(TOOLCHAIN_PREFIX)objcopy jni/../obj/local/$(TARGET_ARCH_ABI)
	@$(word 1,$^) -I binary $(call elf_format,$(call filename,$(word 2,$^))) repl $@

# main ol module
include $(CLEAR_VARS)

LOCAL_MODULE    := ol
LOCAL_SRC_FILES += ../src/olvm.c ../obj/local/$(TARGET_ARCH_ABI)/repl.c
LOCAL_CFLAGS    += -std=c99 -O2 -s -DNAKED_VM -Iinclude
LOCAL_LDFLAGS   := -Xlinker --export-dynamic
LOCAL_LDFLAGS   += -Xlinker obj/local/$(TARGET_ARCH_ABI)/repl.o

LOCAL_CFLAGS    += -DOLVM_LIBRARY_SO_NAME="\"libol.so\""

jni/../obj/local/$(TARGET_ARCH_ABI)/repl.c: jni/../obj/local/$(TARGET_ARCH_ABI)/repl.o
	@echo // This empty file required by the stupid Android build system >$@

include $(BUILD_SHARED_LIBRARY)

# NOTES:
#
# mips PIC:
# https://clang.llvm.org/doxygen/Mips_8cpp_source.html
# Historically, PIC code for MIPS was associated with -mabicalls, a.k.a
# SVR4 abicalls. Static code does not use SVR4 calling sequences. An ABI
# extension was developed by Richard Sandiford & Code Sourcery to support
# static code calling PIC code (CPIC). For O32 and N32 this means we have
# several combinations of PIC/static and abicalls. Pure static, static
# with the CPIC extension, and pure PIC code.
#
# mips64
# I have not found the way to set the repl.o [nan2008] private flag. So still
# got an error "linking -mnan=legacy module with previous -mnan=2008 modules"
