# --------------------------------------
# global build setup, you can change it:

# output package file name
APK_NAME = debug.apk
# used build tools version
BUILD_TOOLS_VERN = 29.0.2
# android platform version
PLATFORM_VERN = android-29

# native architectures supported
NATIVE_PLATFORMS = armeabi-v7a arm64-v8a x86 x86_64

# end of setup

# extract package name from AndroidManifest.xml
PACKAGE=$(shell cat AndroidManifest.xml |sed -n 's/.*package="\([^"]*\)".*/\1/p')

# -------------------------------------------------
.PHONY: help
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
OL_ROOT    ?= $(abspath ../../..)
OL_ANDROID ?= $(OL_ROOT)/extensions/android

include $(OL_ANDROID)/rules.mk

# change if you want
KEYSTOREPWD = thepassword

# -------------------------------------------------
# newton-dynamics library
libs: $(OL_ANDROID)/jni/newton-dynamics

$(OL_ANDROID)/jni/newton-dynamics:
	git clone https://github.com/yuriy-chumak/newton-dynamics-3.14.git --depth 1 $@

assets/lib/newton-dynamics.scm:
	curl -o $@ "https://raw.githubusercontent.com/yuriy-chumak/libol-newton-dynamics/refs/heads/master/lib/newton-dynamics.scm"

# -------------------------------------------------
# copy prebuilt native libraries (*.so)
LIBS=libolvm.so \
     libol.so \
     libgl2es.so libGLU.so \
     libnewton-dynamics.so \
     libmain.so
$(foreach so,$(LIBS),$(eval $(call SO,$(so))))

# copy java ol bridge sources (*.java)
# no files

# copy ol libraries and includes (*.scm, *.lisp)
assets: assets/lib/gl/config.scm
assets: assets/lib/gl/Android.lisp
assets: assets/lib/gl.scm
assets: assets/lib/gl/gl2es.lisp
assets: assets/lib/GLU.scm
assets: assets/OpenGL
assets: assets/otus/case-apply.scm
assets: assets/otus/ffi.scm
assets: assets/lib/newton-dynamics.scm

assets/OpenGL:
	cp -a $(OL_ROOT)/libraries/OpenGL assets/OpenGL

# -------------------------------------------------
install:
	adb -d install -r $(APK_NAME)

uninstall:
	adb -d uninstall $(PACKAGE)

clean:
	rm -rf dex/ lib/ obj/
	rm -rf assets/lib assets/OpenGL assets/otus assets/file

start:
	adb shell am start -n $(PACKAGE)/.Main
