################################################################################
#
# Otus Lisp
#
################################################################################

OL_VERSION = develop
OL_SITE = $(call github,yuriy-chumak,ol,$(OL_VERSION))
OL_LICENSE = GPLv3
OL_LICENSE_FILES = COPYING

define OL_BUILD_CMDS
	$(TARGET_MAKE_ENV) $(TARGET_CONFIGURE_OPTS) $(MAKE) -C $(@D)
endef

define OL_INSTALL_TARGET_CMDS
	$(TARGET_MAKE_ENV) $(MAKE) -C $(@D) install \
		DESTDIR="$(TARGET_DIR)"
endef

$(eval $(generic-package))
