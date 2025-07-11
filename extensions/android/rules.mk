ANDROID_SDK?= /opt/android/sdk

ifndef ANDROID_SDK
$(error ANDROID_SDK not set)
endif
ifneq ($(shell test -d $(ANDROID_SDK); echo $$?), 0)
$(error Looks like ANDROID_SDK is invalid -> $(ANDROID_SDK))
endif

BUILD_TOOLS = $(ANDROID_SDK)/build-tools/$(BUILD_TOOLS_VERN)
ANDROID_JAR = $(ANDROID_SDK)/platforms/$(PLATFORM_VERN)/android.jar

# -------------------------------------------------
.PHONY: libs
libs: # build ol native libraries
	@echo --- building native code -----
	cd $(OL_ANDROID)/jni; ./configure; cd -
	CFLAGS="-DCOMPATIBLE_QUADS=1" \
	$(MAKE) -C $(OL_ROOT) android

# -------------------------------------------------
# copy prebuilt native libraries (*.so)
.PHONY: native
native: libs

lib/%.so: $(OL_ANDROID)/libs/%.so
	@mkdir -p $(dir $@)
	@cp $< $@

NATIVE_PLATFORMS ?= armeabi-v7a arm64-v8a x86 x86_64
define add_to_natives
native: lib/$(1)/$(2)
endef

define SO
$(foreach np,$(NATIVE_PLATFORMS),$(eval $(call add_to_natives,$(np),$(1))))
endef

# copy ol java interface source (*.java)
#src/lang/otuslisp/Ol.java: $(OL_ANDROID)/java/lang/otuslisp/Ol.java
#	@mkdir -p $(dir $@)
#	@cp $< $@

# copy ol libraries and includes (*.scm, *.lisp)
.PHONY: assets

assets/%.scm: $(OL_ROOT)/libraries/%.scm
	@mkdir -p $(dir $@)
	@cp $< $@
assets/%.lisp: $(OL_ROOT)/libraries/%.lisp
	@mkdir -p $(dir $@)
	@cp $< $@

# ---------------------------------------------------
# apk signing debug key (use your own if you have)
debug.keystore:
	keytool -genkeypair -validity 1000 -dname "CN=Debug Key,O=Android,C=ES" -keystore $@ -storepass '$(KEYSTOREPWD)' -keypass '$(KEYSTOREPWD)' -alias projectKey -keyalg RSA

# ---------------------------------------------------
# build ol android libraries
build: native assets debug.keystore
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
