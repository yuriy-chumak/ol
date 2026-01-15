# This project requires GNU make to build.

# Is this BSD Make?
.if defined(.PARSEDIR)

GMAKE = gmake

.DEFAULT:
	@$(GMAKE) -f GNUmakefile $(.TARGETS) $(MAKEFLAGS)

all:
	@$(GMAKE) -f GNUmakefile all $(MAKEFLAGS)

# GNU Make:
.else
include GNUmakefile
.endif
