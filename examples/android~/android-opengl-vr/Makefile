.PHONY: help
help:
	@echo "Targets:"
	@echo "  clean - clean the folder from temp files"
	@echo "  build - build a regular android OpenGL app"
	@echo "  install - install the apk"
	@echo "  install - uninstall installed package"
	@echo "  start - start installed package"

# ---------------------------------------------------
# link assets libraries
$(shell [ -d assets/libraries ]        || mkdir -p assets/libraries)
$(shell [ -d assets/libraries/OpenGL ] || ln -s ../../../../libraries/OpenGL assets/libraries/OpenGL)
$(shell [ -d assets/libraries/lib ]    || ln -s ../../../../libraries/lib    assets/libraries/lib)
$(shell [ -d assets/libraries/otus ]   || ln -s ../../../../libraries/otus   assets/libraries/otus)

# ===================================================
APK_BASENAME?=debug

ANDROID_SDK?=/opt/android/sdk
ANDROID_NDK?=/opt/android/ndk
BUILD_TOOLS?=$(ANDROID_SDK)/build-tools/29.0.2

ifeq ("$(wildcard $(ANDROID_SDK)/)","")
$(error ANDROID_SDK not set or invalid!)
endif

ifeq ("$(wildcard $(ANDROID_NDK)/)","")
$(error ANDROID_NDK not set or invalid!)
endif

.PHONY: all build clean start stop restart install uninstall reinstall logcat

build:
	./build

clean:
	rm -rf dex lib obj
	find src -name "R.java" -exec rm {} \;
	rm -f debug.apk debug.keystore

install:
	adb -d install -r debug.apk
	@# grant default permissions
	@# adb shell pm grant name.yuriychumak.ol android.permission.READ_EXTERNAL_STORAGE

uninstall:
	adb -d uninstall name.yuriychumak.ol

reinstall:
	@$(MAKE) uninstall
	@$(MAKE) install

start:
	adb shell am start -n name.yuriychumak.ol/name.yuriychumak.ol.NativeLoader
stop:
	adb shell am force-stop name.yuriychumak.ol

restart:
	@$(MAKE) stop
	@$(MAKE) start

#upload:
#	adb shell mkdir -p /sdcard/ol/media
#	cd ../../libraries; adb push * /sdcard/ol
#	cd media; adb shell mkdir sdcard/ol/media; adb push * /sdcard/ol/media

logcat:
	adb logcat -v color \
	    ol:D name.yuriychumak.ol.gui:V gl2es:E \
	    ol-egl:V ol-ovr:V \
	    threaded_app:V nativeloader:E \
	    AndroidRuntime:E DEBUG:V *:F

