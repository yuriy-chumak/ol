LOCAL_PATH := $(call my-dir)
# notes:
#
# mips64
# I have no found the way to set the repl.o [nan2008] private flag. So I still
# got an error "linking -mnan=legacy module with previous -mnan=2008 modules".
# 
# Therefore you can't just run "ndk-build" but you must run "make android" for
# the first time. Make will convert repl into repl.c and after this you can do
# anything: ndk-build or make android again for your choice.

# -- libOL ------------------------------------------------------------------------
include $(CLEAR_VARS)
LOCAL_MODULE   := shared-library
LOCAL_MODULE_FILENAME := libol

LOCAL_SRC_FILES := ../src/olvm.c
LOCAL_SRC_FILES += ../extensions/ffi.c
LOCAL_SRC_FILES += oljni.c ../tmp/repl.c
LOCAL_CFLAGS   += -std=c99 -std=gnu11 -O3 -g0 -Iinclude -DNAKED_VM -DEMBEDDED_VM -fsigned-char -Ijni/../src -DOLVM_FFI=1
LOCAL_LDFLAGS  := -Xlinker --export-dynamic

LOCAL_CFLAGS   += -DOLVM_LIBRARY_SO_NAME='"libol.so"'
LOCAL_LDLIBS   += -llog -landroid

#LOCAL_C_INCLUDES := jni/include
#LOCAL_CFLAGS   += -Ijni/include

include $(BUILD_SHARED_LIBRARY)

# -- OL ---------------------------------------------------------------------------
include $(CLEAR_VARS)
LOCAL_MODULE   := ol

LOCAL_SRC_FILES := ../src/olvm.c
LOCAL_SRC_FILES += ../tmp/repl.c

LOCAL_CFLAGS   += -std=c99 -std=gnu11 -O3 -g0 -Iinclude -fsigned-char
LOCAL_LDFLAGS  := -Xlinker --export-dynamic

LOCAL_LDLIBS   += -llog -landroid
include $(BUILD_EXECUTABLE)

# # ------------------------------------
# # test binary
# include $(CLEAR_VARS)
# LOCAL_MODULE   := embed

# LOCAL_SRC_FILES += embed.c repl.c
# LOCAL_CFLAGS   += -Iinclude -Iextensions -O0 -g3
# #
# #LOCAL_CFLAGS   += -std=c99 -O2 -s -Iinclude -DNAKED_VM

# LOCAL_LDLIBS   += -llog -landroid

# LOCAL_SHARED_LIBRARIES := ol
# include $(BUILD_EXECUTABLE)




# -- SOIL ------------------------------------------------------------------------
ifneq ("$(wildcard $(LOCAL_PATH)/SOIL/src)","")
include $(CLEAR_VARS)
LOCAL_MODULE   := SOIL

SOIL_SRC_FILES := $(wildcard $(LOCAL_PATH)/SOIL/src/*.c)
LOCAL_SRC_FILES := $(SOIL_SRC_FILES:$(LOCAL_PATH)/%=%)

LOCAL_CFLAGS   += -std=c99 -O2 -fdiagnostics-color=auto
LOCAL_LDFLAGS  += -Xlinker --export-dynamic

LOCAL_LDLIBS   += -llog -landroid -lEGL -lGLESv1_CM
#-lGLESv2 -lEGL
LOCAL_CFLAGS   += -ISOIL/src

#LOCAL_SHARED_LIBRARIES += gl4es
include $(BUILD_SHARED_LIBRARY)
endif


# # -- gl4es -----------------------------------------------------------------------
# ifneq ("$(wildcard $(LOCAL_PATH)/gl4es/src)","")
# include $(CLEAR_VARS)
# LOCAL_MODULE   := gl4es

# GL_SRC_FILES := $(wildcard $(LOCAL_PATH)/gl4es/src/gl/*.c)\
#                 $(wildcard $(LOCAL_PATH)/gl4es/src/gl/math/*.c)\
#                 $(wildcard $(LOCAL_PATH)/gl4es/src/gl/wrap/*.c)\
#                 $(wildcard $(LOCAL_PATH)/gl4es/src/glx/*.c)

# LOCAL_SRC_FILES  := $(GL_SRC_FILES:$(LOCAL_PATH)/%=%)
# LOCAL_C_INCLUDES := $(LOCAL_PATH)/gl4es/include

# LOCAL_CFLAGS   += -g -std=c99 -funwind-tables -O3 -fvisibility=hidden
# LOCAL_CFLAGS   += -DBCMHOST -DNOX11 -DNO_GBM -DNOEGL -DNO_INIT_CONSTRUCTOR -DANDROID -DGL4ES_COMPILE_FOR_USE_IN_SHARED_LIB -DDEBUG
# LOCAL_CFLAGS   += -include android_debug.h
# LOCAL_CFLAGS   += -DDEFAULT_ES=2

# LOCAL_LDLIBS   += -ldl -llog -landroid
# LOCAL_LDLIBS   += -lGLESv2
# #LOCAL_LDLIBS   += -lGLESv1_CM

# # prepatch
# #$(LOCAL_PATH)/gl4es/patched: $(LOCAL_PATH)/gl4es.patch
# #	echo 1 >$(LOCAL_PATH)/gl4es/patched

# include $(BUILD_SHARED_LIBRARY)
# endif


# # -- freetype2 -------------------------------------------------------------------
# ifneq ("$(wildcard $(LOCAL_PATH)/freetype2/src)","")
# include $(CLEAR_VARS)
# LOCAL_MODULE   := freetype2

# LOCAL_C_INCLUDES := $(LOCAL_PATH)/freetype2/include
# LOCAL_SRC_FILES := \
# 	freetype2/src/autofit/autofit.c \
# 	freetype2/src/base/basepic.c \
# 	freetype2/src/base/ftapi.c \
# 	freetype2/src/base/ftbase.c \
# 	freetype2/src/base/ftbbox.c \
# 	freetype2/src/base/ftbitmap.c \
# 	freetype2/src/base/ftdbgmem.c \
# 	freetype2/src/base/ftdebug.c \
# 	freetype2/src/base/ftglyph.c \
# 	freetype2/src/base/ftinit.c \
# 	freetype2/src/base/ftpic.c \
# 	freetype2/src/base/ftstroke.c \
# 	freetype2/src/base/ftsynth.c \
# 	freetype2/src/base/ftsystem.c \
# 	freetype2/src/cff/cff.c \
# 	freetype2/src/pshinter/pshinter.c \
# 	freetype2/src/psnames/psnames.c \
# 	freetype2/src/raster/raster.c \
# 	freetype2/src/sfnt/sfnt.c \
# 	freetype2/src/smooth/smooth.c \
# 	freetype2/src/truetype/truetype.c

# LOCAL_LDLIBS   += -llog -ldl -landroid
# LOCAL_CFLAGS   += -DANDROID_NDK -DFT2_BUILD_LIBRARY=1

# include $(BUILD_SHARED_LIBRARY)
# endif
