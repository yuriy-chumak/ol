NDK_TOOLCHAIN_VERSION := 4.9
APP_ABI := armeabi armeabi-v7a x86 arm64-v8a mips x86_64

# 'mips64' still produces "linking -mnan=legacy module with previous -mnan=2008 modules"
# so, mips64 excluded.

# all: armeabi armeabi-v7a x86 arm64-v8a mips x86_64 mips64
