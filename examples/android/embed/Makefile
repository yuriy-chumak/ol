# ---------------------------------------
# global build setup, you can change it

# output package name
APK_NAME = debug.apk
# build tools version
BUILD_TOOLS_VERN = 29.0.2
# android platform version
PLATFORM_VERN = android-29

# ----------------------------------------------------
.PHONY: all help libs
help:
	@echo "Usage:"
	@echo "  make build - build a regular android app"
	@echo "  make install - install built package"
	@echo "  make uninstall - uninstall built package"
	@echo ""
	@echo "  make clean - delete intermediate files"

.PHONY: build install uninstall clean
# -------------------------------------------------
# setup Ol and Android folders and toolsets
OL_ROOT    ?= ../../..
OL_ANDROID ?= $(abspath $(OL_ROOT))/extensions/android

ANDROID_SDK?= /opt/android/sdk

ifndef ANDROID_SDK
$(error ANDROID_SDK not set)
endif
ifneq ($(shell test -d $(ANDROID_SDK); echo $$?), 0)
$(error Looks like ANDROID_SDK is invalid -> $(ANDROID_SDK))
endif

#PATH += $(ANDROID_SDK)/platform-tools
BUILD_TOOLS = $(ANDROID_SDK)/build-tools/$(BUILD_TOOLS_VERN)
ANDROID_JAR = $(ANDROID_SDK)/platforms/$(PLATFORM_VERN)/android.jar
KEYSTOREPWD = thepassword
# -------------------------------------------------
libs: # build ol native libraries
	@echo --- building native code -----
	$(MAKE) -C $(OL_ROOT) android

# -------------------------------------------------
# copy prebuilt native libraries (*.so)
lib/%.so: $(OL_ANDROID)/libs/%.so
	@mkdir -p $(dir $@)
	@cp $< $@

.PHONY: armeabi-v7a
armeabi-v7a: lib/armeabi-v7a/libol.so
armeabi-v7a: lib/armeabi-v7a/libolvm.so
.PHONY: arm64-v8a
arm64-v8a: lib/arm64-v8a/libol.so
arm64-v8a: lib/arm64-v8a/libolvm.so

.PHONY: x86
x86: lib/x86/libol.so
x86: lib/x86/libolvm.so
.PHONY: x86_64
x86_64: lib/x86_64/libol.so
x86_64: lib/x86_64/libolvm.so

.PHONY: native
native: libs x86 x86_64 arm64-v8a armeabi-v7a

# copy ol java interface source (*.java)
#src/lang/otuslisp/Ol.java: $(OL_ANDROID)/java/lang/otuslisp/Ol.java
#	@mkdir -p $(dir $@)
#	@cp $< $@

# ---------------------------------------------------
# apk signing debug key (use your own if you have)
debug.keystore:
	keytool -genkeypair -validity 1000 -dname "CN=Debug Key,O=Android,C=ES" -keystore $@ -storepass '$(KEYSTOREPWD)' -keypass '$(KEYSTOREPWD)' -alias projectKey -keyalg RSA

# ---------------------------------------------------
# build ol android libraries
build: native debug.keystore
	@echo --- building java project -----
	@echo - Create R.java from res:
	$(BUILD_TOOLS)/aapt package -f -m \
	   -S res -J src -M AndroidManifest.xml \
	   -I $(ANDROID_JAR)

	@echo - Compile java code:
	mkdir -p obj
	javac -verbose -source 1.8 -target 1.8 -d obj \
	   -bootclasspath jre/lib/rt.jar \
	   -classpath $(ANDROID_JAR):obj \
	   -sourcepath src `find . -name *.java` \
	   || exit

	@echo - Generate android bytecode:
	mkdir -p dex
	$(BUILD_TOOLS)/dx --verbose --dex --output=dex/classes.dex obj

	@echo - Create APK with code, resources, and assets
	$(BUILD_TOOLS)/aapt package -f \
	   -M AndroidManifest.xml -S res -A assets \
	   -I $(ANDROID_JAR) -F $(APK_NAME) dex

	@echo - Add shared libraries to apk
	$(BUILD_TOOLS)/aapt add $(APK_NAME) `find -L lib/ -name *.so`

	@echo - Sign apk
	jarsigner -keystore debug.keystore -storepass '$(KEYSTOREPWD)' -keypass '$(KEYSTOREPWD)' \
	          -signedjar $(APK_NAME) $(APK_NAME) projectKey

	@echo - Zipalign apk
	$(BUILD_TOOLS)/zipalign -f 4 $(APK_NAME) zipaligned.apk
	mv -f zipaligned.apk $(APK_NAME)

	@echo - Apk built.

install:
	adb -d install -r $(APK_NAME)

uninstall:
	adb -d uninstall name.yuriy_chumak.ol.embed

clean:
	rm -rf dex/ lib/ obj/
