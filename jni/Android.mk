# https://developer.android.com/ndk/guides/android_mk
LOCAL_PATH := $(call my-dir)

# notes:
#
# mips64
# I have not found a way to set the repl.o [nan2008] private flag.  So I still
# got an error "linking -mnan=legacy module with previous -mnan=2008 modules".
# 
# Therefore you can't just run "ndk-build" but you must run "make android" for
# the first time. Make will convert repl into repl.c and after this you can do
# anything: ndk-build or make android again for your choice.

# common OL compiler flags
OL_CFLAGS   := -fsigned-char
OL_CFLAGS   += -I$(LOCAL_PATH)/../includes
OL_CFLAGS   += -std=c99 -std=gnu11 -O3 -g0

# -- libMAIN -----------------------------------------------------------------
# native app
include $(CLEAR_VARS)
LOCAL_MODULE   := main
LOCAL_SHARED_LIBRARIES := olvm

# configure
LOCAL_CFLAGS   += $(OL_CFLAGS)
# src
LOCAL_CFLAGS   += -I$(NDK_ROOT) -DREPL=repl
LOCAL_SRC_FILES += native-app.c ../tmp/repl.c
LOCAL_LDLIBS   += -llog -landroid

LOCAL_EXPORT_LDFLAGS := -u ANativeActivity_onCreate
include $(BUILD_SHARED_LIBRARY)

# -- libOLVM------------------------------------------------------------------
# ol shared library
include $(CLEAR_VARS)
LOCAL_MODULE   := olvm
#LOCAL_MODULE_FILENAME := libol

# configure
LOCAL_CFLAGS   += $(OL_CFLAGS)
LOCAL_CFLAGS   += -DOLVM_LIBRARY_SO_NAME='"libolvm.so"' \
                  -Wno-unsequenced -Wno-parentheses
LOCAL_CFLAGS   += -DOLVM_NOMAIN
# src
LOCAL_SRC_FILES := ../src/olvm.c #../tmp/repl.c

# extensions
LOCAL_CFLAGS   += -DHAS_DLOPEN=1
LOCAL_SRC_FILES += ../extensions/ffi.c

LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   += -llog -landroid

include $(BUILD_SHARED_LIBRARY)

# # -- OL ---------------------------------------------------------------------------
# include $(CLEAR_VARS)
# LOCAL_MODULE   := ol

# LOCAL_SRC_FILES := ../src/olvm.c
# LOCAL_SRC_FILES += ../tmp/repl.c
# LOCAL_SRC_FILES += ../extensions/ffi.c
# LOCAL_SRC_FILES += ../tests/ffi.c

# LOCAL_CFLAGS   += -std=c99 -std=gnu11 -O0 -g3 -Iincludes -fsigned-char
# LOCAL_CFLAGS   += -Ijni/../src -DOLVM_FFI=1 -Wno-unsequenced -Wno-parentheses
# LOCAL_LDFLAGS  := -Xlinker --export-dynamic

# LOCAL_LDLIBS   += -llog -landroid
# include $(BUILD_EXECUTABLE)

# -- gl4es -----------------------------------------------------------------------
ifneq ("$(wildcard $(LOCAL_PATH)/gl4es/src)","")
include $(CLEAR_VARS)
LOCAL_MODULE := gl4es

GL_SRC_FILES := $(wildcard $(LOCAL_PATH)/gl4es/src/gl/*.c)\
                $(wildcard $(LOCAL_PATH)/gl4es/src/gl/math/*.c)\
                $(wildcard $(LOCAL_PATH)/gl4es/src/gl/wrap/*.c)\
                $(wildcard $(LOCAL_PATH)/gl4es/src/glx/*.c)

LOCAL_SRC_FILES  := $(GL_SRC_FILES:$(LOCAL_PATH)/%=%)
LOCAL_C_INCLUDES := $(LOCAL_PATH)/gl4es/include

LOCAL_CFLAGS   += -g -std=gnu99 -funwind-tables -O3 -fvisibility=hidden
LOCAL_CFLAGS   += -DNOX11 -DNO_GBM -DDEFAULT_ES=2 -DNOEGL
LOCAL_CFLAGS   += -DNO_INIT_CONSTRUCTOR -DUSE_ANDROID_LOG
LOCAL_CFLAGS   += -include android_debug.h
#LOCAL_CFLAGS   += -DDEBUG

LOCAL_LDLIBS   += -ldl -llog -landroid
LOCAL_LDLIBS   += -lGLESv2

# prepatch example
#$(LOCAL_PATH)/gl4es/patched: $(LOCAL_PATH)/gl4es.patch
#	echo 1 >$(LOCAL_PATH)/gl4es/patched

include $(BUILD_SHARED_LIBRARY)
endif

# -- GLU -------------------------------------------------------------------------
ifneq ("$(wildcard $(LOCAL_PATH)/GLU/src)","")
include $(CLEAR_VARS)
LOCAL_MODULE := GLU

LOCAL_CFLAGS += -fsigned-char -DLIBRARYBUILD

LOCAL_CONLYFLAGS += -std=c99

LOCAL_C_INCLUDES := \
	$(LOCAL_PATH)/GLU/include \
	$(LOCAL_PATH)/GLU/src/include \
	$(LOCAL_PATH)/GLU/src/libnurbs/internals \
	$(LOCAL_PATH)/GLU/src/libnurbs/interface \
	$(LOCAL_PATH)/GLU/src/libnurbs/nurbtess

LOCAL_SRC_FILES := \
	GLU/src/libutil/error.c					\
	GLU/src/libutil/glue.c					\
	GLU/src/libutil/mipmap.c					\
	GLU/src/libutil/project.c					\
	GLU/src/libutil/quad.c					\
	GLU/src/libutil/registry.c					\
	GLU/src/libtess/dict.c					\
	GLU/src/libtess/geom.c					\
	GLU/src/libtess/memalloc.c					\
	GLU/src/libtess/mesh.c					\
	GLU/src/libtess/normal.c					\
	GLU/src/libtess/priorityq.c					\
	GLU/src/libtess/render.c					\
	GLU/src/libtess/sweep.c					\
	GLU/src/libtess/tess.c					\
	GLU/src/libtess/tessmono.c					\
	GLU/src/libnurbs/interface/bezierEval.cc			\
	GLU/src/libnurbs/interface/bezierPatch.cc			\
	GLU/src/libnurbs/interface/bezierPatchMesh.cc		\
	GLU/src/libnurbs/interface/glcurveval.cc			\
	GLU/src/libnurbs/interface/glinterface.cc			\
	GLU/src/libnurbs/interface/glrenderer.cc			\
	GLU/src/libnurbs/interface/glsurfeval.cc			\
	GLU/src/libnurbs/interface/incurveeval.cc			\
	GLU/src/libnurbs/interface/insurfeval.cc			\
	GLU/src/libnurbs/internals/arc.cc				\
	GLU/src/libnurbs/internals/arcsorter.cc			\
	GLU/src/libnurbs/internals/arctess.cc			\
	GLU/src/libnurbs/internals/backend.cc			\
	GLU/src/libnurbs/internals/basiccrveval.cc			\
	GLU/src/libnurbs/internals/basicsurfeval.cc			\
	GLU/src/libnurbs/internals/bin.cc				\
	GLU/src/libnurbs/internals/bufpool.cc			\
	GLU/src/libnurbs/internals/cachingeval.cc			\
	GLU/src/libnurbs/internals/ccw.cc				\
	GLU/src/libnurbs/internals/coveandtiler.cc			\
	GLU/src/libnurbs/internals/curve.cc				\
	GLU/src/libnurbs/internals/curvelist.cc			\
	GLU/src/libnurbs/internals/curvesub.cc			\
	GLU/src/libnurbs/internals/dataTransform.cc			\
	GLU/src/libnurbs/internals/displaylist.cc			\
	GLU/src/libnurbs/internals/flist.cc				\
	GLU/src/libnurbs/internals/flistsorter.cc			\
	GLU/src/libnurbs/internals/hull.cc				\
	GLU/src/libnurbs/internals/intersect.cc			\
	GLU/src/libnurbs/internals/knotvector.cc			\
	GLU/src/libnurbs/internals/mapdesc.cc			\
	GLU/src/libnurbs/internals/mapdescv.cc			\
	GLU/src/libnurbs/internals/maplist.cc			\
	GLU/src/libnurbs/internals/mesher.cc			\
	GLU/src/libnurbs/internals/monoTriangulationBackend.cc	\
	GLU/src/libnurbs/internals/monotonizer.cc			\
	GLU/src/libnurbs/internals/mycode.cc			\
	GLU/src/libnurbs/internals/nurbsinterfac.cc			\
	GLU/src/libnurbs/internals/nurbstess.cc			\
	GLU/src/libnurbs/internals/patch.cc				\
	GLU/src/libnurbs/internals/patchlist.cc			\
	GLU/src/libnurbs/internals/quilt.cc				\
	GLU/src/libnurbs/internals/reader.cc			\
	GLU/src/libnurbs/internals/renderhints.cc			\
	GLU/src/libnurbs/internals/slicer.cc			\
	GLU/src/libnurbs/internals/sorter.cc			\
	GLU/src/libnurbs/internals/splitarcs.cc			\
	GLU/src/libnurbs/internals/subdivider.cc			\
	GLU/src/libnurbs/internals/tobezier.cc			\
	GLU/src/libnurbs/internals/trimline.cc			\
	GLU/src/libnurbs/internals/trimregion.cc			\
	GLU/src/libnurbs/internals/trimvertpool.cc			\
	GLU/src/libnurbs/internals/uarray.cc			\
	GLU/src/libnurbs/internals/varray.cc			\
	GLU/src/libnurbs/nurbtess/directedLine.cc			\
	GLU/src/libnurbs/nurbtess/gridWrap.cc			\
	GLU/src/libnurbs/nurbtess/monoChain.cc			\
	GLU/src/libnurbs/nurbtess/monoPolyPart.cc			\
	GLU/src/libnurbs/nurbtess/monoTriangulation.cc		\
	GLU/src/libnurbs/nurbtess/partitionX.cc			\
	GLU/src/libnurbs/nurbtess/partitionY.cc			\
	GLU/src/libnurbs/nurbtess/polyDBG.cc			\
	GLU/src/libnurbs/nurbtess/polyUtil.cc			\
	GLU/src/libnurbs/nurbtess/primitiveStream.cc		\
	GLU/src/libnurbs/nurbtess/quicksort.cc			\
	GLU/src/libnurbs/nurbtess/rectBlock.cc			\
	GLU/src/libnurbs/nurbtess/sampleComp.cc			\
	GLU/src/libnurbs/nurbtess/sampleCompBot.cc			\
	GLU/src/libnurbs/nurbtess/sampleCompRight.cc		\
	GLU/src/libnurbs/nurbtess/sampleCompTop.cc			\
	GLU/src/libnurbs/nurbtess/sampleMonoPoly.cc			\
	GLU/src/libnurbs/nurbtess/sampledLine.cc			\
	GLU/src/libnurbs/nurbtess/searchTree.cc

LOCAL_SHARED_LIBRARIES := gl4es
#LOCAL_ALLOW_UNDEFINED_SYMBOLS := true
LOCAL_LDLIBS := -ldl -llog

include $(BUILD_SHARED_LIBRARY)
endif

# -- SOIL ------------------------------------------------------------------------
ifneq ("$(wildcard $(LOCAL_PATH)/SOIL/src)","")
include $(CLEAR_VARS)
LOCAL_MODULE   := SOIL

SOIL_SRC_FILES := $(wildcard $(LOCAL_PATH)/SOIL/src/*.c)
LOCAL_SRC_FILES := $(SOIL_SRC_FILES:$(LOCAL_PATH)/%=%)

LOCAL_CFLAGS   += -I$(LOCAL_PATH)/SOIL/include
LOCAL_CFLAGS   += -I$(LOCAL_PATH)/gl4es/include

LOCAL_SHARED_LIBRARIES += gl4es
include $(BUILD_SHARED_LIBRARY)
endif


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
