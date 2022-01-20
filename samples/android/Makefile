APK_BASENAME?=ola

ANDROID_SDK_ROOT?=/opt/android/sdk
ANDROID_NDK_ROOT?=/opt/android/ndk

ifeq ("$(wildcard $(ANDROID_SDK_ROOT)/)","")
$(error Invalid ANDROID_SDK_ROOT variable!)
endif

ifeq ("$(wildcard $(ANDROID_NDK_ROOT)/)","")
$(error Invalid ANDROID_NDK_ROOT variable!)
endif

.PHONY: all jni clean install

all: jni assets/libraries ${APK_BASENAME}-debug.apk


ANDROID_BUILD_TOOLS=$(ANDROID_SDK_ROOT)/build-tools

_BUILD_TOOLS_VERSION=$(shell ls $(ANDROID_BUILD_TOOLS) | sort -n |tail -1)
_PLATORM=$(shell ls $(ANDROID_SDK_ROOT)/platforms | sort -nr |tail -1)
_ANDROID_CP=$(ANDROID_SDK_ROOT)/platforms/$(_PLATORM)/android.jar
_AAPT=$(ANDROID_BUILD_TOOLS)/$(_BUILD_TOOLS_VERSION)/aapt
_DX=$(ANDROID_BUILD_TOOLS)/$(_BUILD_TOOLS_VERSION)/dx
_ZIPALIGN=$(ANDROID_BUILD_TOOLS)/$(_BUILD_TOOLS_VERSION)/zipalign
_ADB=$(ANDROID_SDK_ROOT)/platform-tools/adb
_INTERMEDIATE=bin gen lib ${APK_BASENAME}.apk.unaligned classes.dex

_SRC=$(shell find . -name "*.java")
$(info _SRC: $(_SRC))

#export PATH=$(PATH):$(ANDROID_SDK_ROOT)/platform-tools

assets/libraries:
	ln -s ../../../libraries assets/libraries

jni:
	NDK_ROOT=$(ANDROID_NDK_ROOT) \
	make -C ../.. android
	cp -r ../../libs lib/

$(info Build with configuration:)
$(info build tools version: $(_BUILD_TOOLS_VERSION))
$(info platform: $(_PLATORM))
$(info )

clean:
	rm -rf $(_INTERMEDIATE)

bin:
	mkdir bin
gen:
	mkdir gen

${APK_BASENAME}-debug.apk: bin gen $(_SRC)
	$(_AAPT) package -f -m -J gen -M AndroidManifest.xml -S res -I $(_ANDROID_CP)
	javac -classpath $(_ANDROID_CP) \
		-sourcepath 'src:gen' \
		-d 'bin' -target 1.7 -source 1.7 \
		`find . -name "*.java"` \
		-bootclasspath /usr/lib/jvm/java-8-openjdk-amd64/jre/libjar
	$(_DX) --dex --output=classes.dex bin

	rm -f ${APK_BASENAME}.apk.unaligned
	rm -f ${APK_BASENAME}-debug.apk
	$(_AAPT) package -f -M AndroidManifest.xml -S res -I $(_ANDROID_CP) -F ${APK_BASENAME}.apk.unaligned

	$(_AAPT) add ${APK_BASENAME}.apk.unaligned classes.dex
	$(_AAPT) add ${APK_BASENAME}.apk.unaligned `find -L assets/ -type f \( ! -name "* *" \)`
	$(_AAPT) add ${APK_BASENAME}.apk.unaligned `find -L lib/ -type f`

	jarsigner -keystore ./debug.keystore -sigalg SHA1withRSA -digestalg SHA1 -storepass android ./${APK_BASENAME}.apk.unaligned androiddebugkey
	$(_ZIPALIGN) -f 4 ${APK_BASENAME}.apk.unaligned ${APK_BASENAME}-debug.apk

install: ${APK_BASENAME}-debug.apk
	adb install ${APK_BASENAME}-debug.apk
	adb shell am start -n name.yuriy_chumak.ol/name.yuriy_chumak.ol.MainActivity


#debug.keystore:
#	echo Debug Key generator.
#	echo Please make this answers:
#	echo
#	echo First and last name: Android Debug
#	echo Organizational unit: Android
#	echo Name of organization: Unknown
#	echo City or Locality: Unknown
#	echo State or Province: Unknown
#	echo Country code: US
#	keytool -genkey -v -keystore debug.keystore -alias androiddebugkey -storepass android -keypass android -keyalg RSA -validity 14000
