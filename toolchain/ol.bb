# Copyright (c) 2013-2015 LG Electronics, Inc.

SUMMARY = "ol"
DESCRIPTION = "ol"
LICENSE = "CLOSED"
AUTHOR = "Yuriy Chumak <yuriy.chumak@github.com>"

DEPENDS = ""

WEBOS_VERSION = "1.0.0-1_2c67a88cbaab9d6ad4fceb3084c2040ac4434264"
PR = "r0"

inherit webos_component
inherit webos_enhanced_submissions
inherit webos_cmake
inherit webos_daemon
inherit webos_system_bus

SRC_URI = "git://github.com/yuriy-chumak/OL.git"
#SRC_URI[md5sum] = "2d14a6f9f3fa8cb795d8b60831c369d1"

S = "${WORKDIR}/git"

do_configure() {
   echo "no configuring required yet"
}

do_compile () {
   export CFLAGS=-m32
   oe_runmake ${PARALLEL_MAKE} -C ${S} -f Makefile ol
}

do_install() {
   install ${S}/ol ${WORKDIR}/build
}
