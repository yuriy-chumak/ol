.PHONY: help
help:
	@echo "This is Makefile for the android projects"
	@echo "  build - build a regular android app"
	@echo "  install - install built package"
	@echo "  uninstall - uninstall built package"

# ---------------------------------------------------
# link assets libraries
#$(shell [ -d assets/libraries ] || mkdir assets/libraries)
#$(shell [ -d assets/libraries/OpenGL ] || ln -s ../../../../libraries/OpenGL assets/libraries/OpenGL)

# ===================================================
APK_BASENAME?=debug

ANDROID_SDK?=/opt/android/sdk
ANDROID_NDK?=/opt/android/ndk
BUILD_TOOLS?=$(ANDROID_SDK)/build-tools/29.0.2

ifeq ("$(wildcard $(ANDROID_SDK)/)", "")
$(error ANDROID_SDK not set or invalid!)
endif

ifeq ("$(wildcard $(ANDROID_NDK)/)", "")
$(error ANDROID_NDK not set or invalid!)
endif

.PHONY: all build clean start stop restart install uninstall reinstall logcat

all:
	# $(MAKE) build
	# $(MAKE) stop
	# $(MAKE) uninstall
	# $(MAKE) upload
	# $(MAKE) install
	# $(MAKE) start

build:
	./build

clean:
	rm -rf dex lib obj
	find src -name "R.java" -exec rm {} \;
	rm -f debug.apk debug.keystore

start:
	adb shell am start -n name.yuriy_chumak.ol.tui/name.yuriy_chumak.ol.tui.MainActivity
stop:
	adb shell am force-stop name.yuriy_chumak.ol.tui

restart:
	$(MAKE) stop
	$(MAKE) upload
	$(MAKE) start


install:
	adb -d install -r debug.apk
	# grant default permissions
	# adb shell pm grant name.yuriy_chumak.ol android.permission.READ_EXTERNAL_STORAGE

uninstall:
	adb -d uninstall name.yuriy_chumak.ol.tui

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

logcat:
	adb logcat -v color ol:V threaded_app:V *:F

#upload:
#	adb shell mkdir -p /sdcard/ol/media
#	cd ../../libraries; adb push * /sdcard/ol
#	cd media; adb shell mkdir sdcard/ol/media; adb push * /sdcard/ol/media
