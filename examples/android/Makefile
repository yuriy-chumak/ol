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

.PHONY: all

all:
	$(MAKE) build
	$(MAKE) stop
	$(MAKE) uninstall
	$(MAKE) upload
	$(MAKE) install
	$(MAKE) start

build:
	./make

clean:
	rm -rf dex lib obj
	find src -name "R.java" -exec rm {} \;
	rm -f debug.apk raylib.keystore

start:
	adb push main.lisp /sdcard/ol/main.lisp
	adb shell am start -n name.yuriy_chumak.ol/name.yuriy_chumak.ol.NativeLoader
stop:
	adb shell am force-stop name.yuriy_chumak.ol

restart:
	$(MAKE) stop
	$(MAKE) upload
	$(MAKE) start


install:
	adb -d install debug.apk
	# grant default permissions
	adb shell pm grant name.yuriy_chumak.ol android.permission.READ_EXTERNAL_STORAGE

uninstall:
	adb -d uninstall name.yuriy_chumak.ol

reinstall:
	$(MAKE) uninstall
	$(MAKE) install


upload:
	adb shell mkdir -p /sdcard/ol/media
	cd ../../libraries; adb push * /sdcard/ol
	cd media; adb shell mkdir sdcard/ol/media; adb push * /sdcard/ol/media
