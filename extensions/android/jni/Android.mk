# https://developer.android.com/ndk/guides/android_mk
LOCAL_PATH := $(call my-dir)

OL_ROOT = ../../..

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
OL_CFLAGS   += -I$(LOCAL_PATH)/$(OL_ROOT)/includes
OL_CFLAGS   += -std=c99 -std=gnu11 -O3 -g0

# -- OL ----------------------------------------------------------------------
# OL executable (put to /data/local/tmp and run)
include $(CLEAR_VARS)
LOCAL_MODULE  := executable
LOCAL_MODULE_FILENAME := ol

LOCAL_CFLAGS   += $(OL_CFLAGS) -DHAVE_DLOPEN=1 -DHAVE_SOCKETS=1 -DREPL=repl

# src
LOCAL_SRC_FILES := $(OL_ROOT)/src/olvm.c
LOCAL_SRC_FILES += $(OL_ROOT)/src/repl.S
LOCAL_SRC_FILES += $(OL_ROOT)/extensions/ffi.c
#LOCAL_SRC_FILES+= $(OL_ROOT)/tests/ffi.c # only for tests/ffi.scm

# linker
LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   += -llog -landroid

include $(BUILD_EXECUTABLE)

# -- libOLVM------------------------------------------------------------------
# OLVM shared library
include $(CLEAR_VARS)
LOCAL_MODULE   := olvm
#LOCAL_MODULE_FILENAME := libolvm

# configure
LOCAL_CFLAGS   += $(OL_CFLAGS)
LOCAL_CFLAGS   += -DOLVM_LIBRARY_SO_NAME='"lib$(LOCAL_MODULE).so"' \
                  -Wno-unsequenced -Wno-parentheses
LOCAL_CFLAGS   += -DOLVM_NOMAIN
# src
LOCAL_SRC_FILES := $(OL_ROOT)/src/olvm.c

# linker
LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   += -llog -landroid

# extensions (ffi)
LOCAL_CFLAGS   += -DHAVE_DLOPEN=1
LOCAL_SRC_FILES += $(OL_ROOT)/extensions/ffi.c

include $(BUILD_SHARED_LIBRARY)

# -- java interface + repl ---------------------------------------------------
# OL library
include $(CLEAR_VARS)
LOCAL_MODULE   := libol
LOCAL_MODULE_FILENAME := libol
LOCAL_SHARED_LIBRARIES := olvm

# configure
LOCAL_CFLAGS   += $(OL_CFLAGS)
LOCAL_CFLAGS   += -DREPL=repl

# android jni + ol repl
LOCAL_SRC_FILES += jni.c
LOCAL_SRC_FILES += $(OL_ROOT)/src/repl.S

# linker
LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   += -llog -landroid

include $(BUILD_SHARED_LIBRARY)

# -- libMAIN -----------------------------------------------------------------
# native app
ifneq ($(wildcard $(LOCAL_PATH)/vrApi/stub.c),)
include $(CLEAR_VARS)
LOCAL_MODULE   := main
LOCAL_SHARED_LIBRARIES := olvm gl2es libol
# TODO: move "vrapi" under define

# configure
LOCAL_CFLAGS   += $(OL_CFLAGS)

# src
LOCAL_CFLAGS   += -I$(NDK_ROOT) -DREPL=repl -I$(LOCAL_PATH)/vrApi/include
LOCAL_SRC_FILES += main.c
LOCAL_SRC_FILES += ovr.c # Oculus Go / Meta Quest 2
LOCAL_SRC_FILES += egl.c # OpenGL ES

LOCAL_LDLIBS   += -llog -landroid

# opengl
ifeq ($(TARGET_ARCH_ABI),armeabi-v7a)
LOCAL_CFLAGS   += -DNO_GLES3=1 # no GLESv3 on the older platform
LOCAL_LDLIBS   += -lEGL -lGLESv2
else
ifeq ($(TARGET_ARCH_ABI),x86)
LOCAL_CFLAGS   += -DNO_GLES3=1 # no GLESv3 on the older platform
LOCAL_LDLIBS   += -lEGL -lGLESv2
else
LOCAL_LDLIBS   += -lEGL -lGLESv3
endif
endif

LOCAL_EXPORT_LDFLAGS := -u ANativeActivity_onCreate
include $(BUILD_SHARED_LIBRARY)
endif

# -- gl2es -----------------------------------------------------------------------
ifneq ($(wildcard $(LOCAL_PATH)/gl2es/src),)
include $(CLEAR_VARS)
LOCAL_MODULE := gl2es

LOCAL_CFLAGS   += -g -std=gnu99 -funwind-tables -O3 -fvisibility=hidden \
                  -DGL2ES_LIBRARY_SO_NAME=\"lib$(LOCAL_MODULE).so\"

# src
GL2ES_SRC_FILES := $(wildcard $(LOCAL_PATH)/gl2es/src/*.c)

LOCAL_SRC_FILES  := $(GL2ES_SRC_FILES:$(LOCAL_PATH)/%=%)
LOCAL_C_INCLUDES := $(LOCAL_PATH)/gl2es/include

# linker
LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   += -ldl -llog -landroid

LOCAL_LDLIBS   += -lEGL -lGLESv2

include $(BUILD_SHARED_LIBRARY)
endif

# # # -- gl4es -----------------------------------------------------------------------
# # ifneq ($(wildcard $(LOCAL_PATH)/gl4es/src),)
# # include $(CLEAR_VARS)
# # LOCAL_MODULE := gl4es

# # GL_SRC_FILES := $(wildcard $(LOCAL_PATH)/gl4es/src/gl/*.c)\
# #                 $(wildcard $(LOCAL_PATH)/gl4es/src/gl/math/*.c)\
# #                 $(wildcard $(LOCAL_PATH)/gl4es/src/gl/wrap/*.c)\
# #                 $(wildcard $(LOCAL_PATH)/gl4es/src/glx/*.c)

# # LOCAL_SRC_FILES  := $(GL_SRC_FILES:$(LOCAL_PATH)/%=%)
# # LOCAL_C_INCLUDES := $(LOCAL_PATH)/gl4es/include

# # LOCAL_CFLAGS   += -g -std=gnu99 -funwind-tables -O3 -fvisibility=hidden
# # LOCAL_CFLAGS   += -DNOX11 -DNO_GBM -DDEFAULT_ES=2 -DNOEGL
# # LOCAL_CFLAGS   += -DNO_INIT_CONSTRUCTOR -DUSE_ANDROID_LOG
# # #LOCAL_CFLAGS   += -DGL4ES_COMPILE_FOR_USE_IN_SHARED_LIB
# # LOCAL_CFLAGS   += -include android_debug.h
# # #LOCAL_CFLAGS   += -DDEBUG

# # LOCAL_LDLIBS   += -ldl -llog -landroid
# # LOCAL_LDLIBS   += -lGLESv2

# # # prepatch example
# # #$(LOCAL_PATH)/gl4es/patched: $(LOCAL_PATH)/gl4es.patch
# # #	echo 1 >$(LOCAL_PATH)/gl4es/patched

# # include $(BUILD_SHARED_LIBRARY)
# # endif

# -- GLU -------------------------------------------------------------------------
ifneq ($(wildcard $(LOCAL_PATH)/GLU/src),)
include $(CLEAR_VARS)
LOCAL_MODULE := GLU
LOCAL_SHARED_LIBRARIES := gl2es

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

# linker
#LOCAL_ALLOW_UNDEFINED_SYMBOLS := true
LOCAL_LDFLAGS  := -Xlinker --export-dynamic
LOCAL_LDLIBS   := -ldl
LOCAL_LDLIBS   += -llog -landroid

include $(BUILD_SHARED_LIBRARY)
endif

# -- SOIL ------------------------------------------------------------------------
ifneq ($(wildcard $(LOCAL_PATH)/SOIL/src),)
include $(CLEAR_VARS)
LOCAL_MODULE   := SOIL
LOCAL_SHARED_LIBRARIES += gl2es

SOIL_SRC_FILES := $(wildcard $(LOCAL_PATH)/SOIL/src/*.c)
LOCAL_SRC_FILES:= $(SOIL_SRC_FILES:$(LOCAL_PATH)/%=%)

LOCAL_CFLAGS   += -I$(LOCAL_PATH)/SOIL/include -D__ANDROID__
LOCAL_LDLIBS   := -lEGL
LOCAL_LDLIBS   += -llog -landroid

include $(BUILD_SHARED_LIBRARY)
endif

# # # -- freetype2 -------------------------------------------------------------------
# # ifneq ($(wildcard $(LOCAL_PATH)/freetype2/src),)
# # include $(CLEAR_VARS)
# # LOCAL_MODULE   := freetype2

# # LOCAL_C_INCLUDES := $(LOCAL_PATH)/freetype2/include
# # LOCAL_SRC_FILES := \
# # 	freetype2/src/autofit/autofit.c \
# # 	freetype2/src/base/basepic.c \
# # 	freetype2/src/base/ftapi.c \
# # 	freetype2/src/base/ftbase.c \
# # 	freetype2/src/base/ftbbox.c \
# # 	freetype2/src/base/ftbitmap.c \
# # 	freetype2/src/base/ftdbgmem.c \
# # 	freetype2/src/base/ftdebug.c \
# # 	freetype2/src/base/ftglyph.c \
# # 	freetype2/src/base/ftinit.c \
# # 	freetype2/src/base/ftpic.c \
# # 	freetype2/src/base/ftstroke.c \
# # 	freetype2/src/base/ftsynth.c \
# # 	freetype2/src/base/ftsystem.c \
# # 	freetype2/src/cff/cff.c \
# # 	freetype2/src/pshinter/pshinter.c \
# # 	freetype2/src/psnames/psnames.c \
# # 	freetype2/src/raster/raster.c \
# # 	freetype2/src/sfnt/sfnt.c \
# # 	freetype2/src/smooth/smooth.c \
# # 	freetype2/src/truetype/truetype.c

# # LOCAL_LDLIBS   += -llog -ldl -landroid
# # LOCAL_CFLAGS   += -DANDROID_NDK -DFT2_BUILD_LIBRARY=1

# # include $(BUILD_SHARED_LIBRARY)
# # endif

# -- libvrapi ------------------------------------------------------------------
ifneq ($(wildcard $(LOCAL_PATH)/vrApi/stub.c),)
include $(CLEAR_VARS)
LOCAL_MODULE   := vrapi
LOCAL_SHARED_LIBRARIES := gl2es

# vrApi libraries are only for armeabi-v7a and arm64-v8a
ifneq ($(wildcard $(LOCAL_PATH)/vrApi/libs/$(TARGET_ARCH_ABI)/libvrapi.so),)
LOCAL_SRC_FILES := vrApi/libs/$(TARGET_ARCH_ABI)/libvrapi.so
LOCAL_EXPORT_C_INCLUDES := $(LOCAL_PATH)/include

include $(PREBUILT_SHARED_LIBRARY)

# there is no vrApi library available, so just use an empty stub
else
LOCAL_SRC_FILES := vrApi/stub.c
LOCAL_LDFLAGS  := -Xlinker --export-dynamic

include $(BUILD_SHARED_LIBRARY)

endif
endif

# # -- newton-dynamics ---------------------------------------------------------
# ifneq ("$(wildcard $(LOCAL_PATH)/newton-dynamics/LICENSE)","")
# include $(CLEAR_VARS)
# LOCAL_MODULE := newton-dynamics
# LOCAL_SUBPATH = newton-dynamics/newton-dynamics/newton-3.14/sdk

# # skip unsupported platforms
# ifneq ("$(TARGET_ARCH_ABI)","arm64-v8a")
# SRC_FILES += newton-dynamics/stub.c
# else

# # only arm64-v8a supported (todo: add x86_64)
# LOCAL_CFLAGS += -fPIC \
#                 -D_POSIX_VER \
#                 -DDG_DISABLE_ASSERT \
# 				\
# 				-DHAVE_ARM_NEON_H -DHAVE_NEON=1 \
#                 -mfloat-abi=hard -include "arm_neon.h"

# # core
# DG_PATH = $(LOCAL_SUBPATH)/dgCore

# LOCAL_C_INCLUDES += $(LOCAL_PATH)/$(DG_PATH)
# LOCAL_SRC_FILES += \
# 	$(DG_PATH)/dg.cpp \
# 	$(DG_PATH)/dgAABBPolygonSoup.cpp \
# 	$(DG_PATH)/dgCRC.cpp \
# 	$(DG_PATH)/dgConvexHull3d.cpp \
# 	$(DG_PATH)/dgConvexHull4d.cpp \
# 	$(DG_PATH)/dgDebug.cpp \
# 	$(DG_PATH)/dgDelaunayTetrahedralization.cpp \
# 	$(DG_PATH)/dgGeneralMatrix.cpp \
# 	$(DG_PATH)/dgGeneralVector.cpp \
# 	$(DG_PATH)/dgGoogol.cpp \
# 	$(DG_PATH)/dgIntersections.cpp \
# 	$(DG_PATH)/dgMatrix.cpp \
# 	$(DG_PATH)/dgMemory.cpp \
# 	$(DG_PATH)/dgMutexThread.cpp \
# 	$(DG_PATH)/dgNode.cpp \
# 	$(DG_PATH)/dgObb.cpp \
# 	$(DG_PATH)/dgPolygonSoupBuilder.cpp \
# 	$(DG_PATH)/dgPolygonSoupDatabase.cpp \
# 	$(DG_PATH)/dgPolyhedra.cpp \
# 	$(DG_PATH)/dgPolyhedraMassProperties.cpp \
# 	$(DG_PATH)/dgProfiler.cpp \
# 	$(DG_PATH)/dgQuaternion.cpp \
# 	$(DG_PATH)/dgRandom.cpp \
# 	$(DG_PATH)/dgRef.cpp \
# 	$(DG_PATH)/dgRefCounter.cpp \
# 	$(DG_PATH)/dgSmallDeterminant.cpp \
# 	$(DG_PATH)/dgThread.cpp \
# 	$(DG_PATH)/dgThreadHive.cpp \
# 	$(DG_PATH)/dgTree.cpp \
# 	$(DG_PATH)/dgTypes.cpp

# # mesh geometry
# DG_MESH_PATH = $(LOCAL_SUBPATH)/dgMeshUtil

# LOCAL_C_INCLUDES += $(LOCAL_PATH)/$(DG_MESH_PATH)
# LOCAL_SRC_FILES += \
# 	$(DG_MESH_PATH)/dgMeshEffect1.cpp \
# 	$(DG_MESH_PATH)/dgMeshEffect2.cpp \
# 	$(DG_MESH_PATH)/dgMeshEffect3.cpp \
# 	$(DG_MESH_PATH)/dgMeshEffect4.cpp \
# 	$(DG_MESH_PATH)/dgMeshEffect5.cpp \
# 	$(DG_MESH_PATH)/dgMeshEffect6.cpp 

# # physics
# DG_PHYSICS_PATH = $(LOCAL_SUBPATH)/dgPhysics

# LOCAL_C_INCLUDES += $(LOCAL_PATH)/$(DG_PHYSICS_PATH)
# LOCAL_SRC_FILES += \
# 	$(DG_PHYSICS_PATH)/dgBallConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgBilateralConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgBody.cpp \
# 	$(DG_PHYSICS_PATH)/dgBodyMasterList.cpp \
# 	$(DG_PHYSICS_PATH)/dgBroadPhase.cpp \
# 	$(DG_PHYSICS_PATH)/dgBroadPhaseAggregate.cpp \
# 	$(DG_PHYSICS_PATH)/dgBroadPhaseMixed.cpp \
# 	$(DG_PHYSICS_PATH)/dgBroadPhaseSegregated.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollision.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionBVH.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionBox.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionCapsule.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionChamferCylinder.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionCompound.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionCompoundFractured.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionCone.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionConvex.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionConvexHull.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionConvexPolygon.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionCylinder.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionDeformableMesh.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionDeformableSolidMesh.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionHeightField.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionIncompressibleParticles.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionInstance.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionLumpedMassParticles.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionMassSpringDamperSystem.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionMesh.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionNull.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionScene.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionSphere.cpp \
# 	$(DG_PHYSICS_PATH)/dgCollisionUserMesh.cpp \
# 	$(DG_PHYSICS_PATH)/dgConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgContact.cpp \
# 	$(DG_PHYSICS_PATH)/dgContactSolver.cpp \
# 	$(DG_PHYSICS_PATH)/dgCorkscrewConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgDynamicBody.cpp \
# 	$(DG_PHYSICS_PATH)/dgHingeConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgKinematicBody.cpp \
# 	$(DG_PHYSICS_PATH)/dgNarrowPhaseCollision.cpp \
# 	$(DG_PHYSICS_PATH)/dgSkeletonContainer.cpp \
# 	$(DG_PHYSICS_PATH)/dgSlidingConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgUniversalConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgUpVectorConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgUserConstraint.cpp \
# 	$(DG_PHYSICS_PATH)/dgWorld.cpp \
# 	$(DG_PHYSICS_PATH)/dgWorldDynamicUpdate.cpp \
# 	$(DG_PHYSICS_PATH)/dgWorldDynamicsParallelSolver.cpp \
# 	$(DG_PHYSICS_PATH)/dgWorldDynamicsSimpleSolver.cpp \
# 	$(DG_PHYSICS_PATH)/dgWorldPlugins.cpp

# # engine files
# DG_NEWTON_PATH = $(LOCAL_SUBPATH)/dgNewton

# LOCAL_C_INCLUDES += $(LOCAL_PATH)/$(DG_NEWTON_PATH)
# LOCAL_SRC_FILES += \
# 	$(DG_NEWTON_PATH)/Newton.cpp \
# 	$(DG_NEWTON_PATH)/NewtonClass.cpp

# endif

# #LOCAL_ALLOW_UNDEFINED_SYMBOLS := true
# LOCAL_LDLIBS := -ldl -llog

# include $(BUILD_SHARED_LIBRARY)
# endif
