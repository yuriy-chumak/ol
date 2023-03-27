## 'configure' part:

# stdint.h
stdint = $(shell echo "\
	\#include <stdint.h>\
	\n\
	int main() {\
	   return INT32_MAX;\
	}" |$(CC) -xc - -o /dev/null 2>/dev/null && echo 1)

#ifneq ($(call stdint),1)
#$(error Looks like you have no libc6-dev, please install.)
#endif

# xxd tool
ifneq ($(if $(shell echo "" |xxd),1,0),1)
$(error Looks like you don't have the xxd tool. This is usually part of the vim package. Please install.)
endif


# additional part
exists = $(shell echo "\
	char $3();\
	\
	int main() {\
	   return $3();\
	}" |$(CC) -xc - $4\
	          -include $2\
	          -o /dev/null 2>/dev/null && echo 1 || echo 0)

sizeof = $(shell SIZEOF=`mktemp /tmp/sizeof.XXXXXXXXX`; \
	trap "{ rm -f $$SIZEOF; }" EXIT; \
	echo "\
	void main() {\
	   printf(\"%d\", sizeof($1));\
	}" |$(CC) -xc - $3\
	          -include stdio.h\
	          -o $$SIZEOF 2>stderr && $$SIZEOF)

offsetof = $(shell OFFSETOF=`mktemp /tmp/offsetof.XXXXXXXXX`; \
	trap "{ rm -f $$OFFSETOF; }" EXIT; \
	echo "\
	void main() {\
	   printf(\"%d\", offsetof($1,$2));\
	}" |$(CC) -xc - $3\
	          -include X11/Xlib.h\
			  -include stdio.h\
	          -o $$OFFSETOF 2>/dev/null && $$OFFSETOF)

# default platform features
HAS_DLOPEN  ?= $(call exists,,stdlib.h, dlopen, -ldl)
HAS_SECCOMP ?= $(call exists,,linux/seccomp.h, prctl)
HAS_SOCKETS ?= $(call exists,,stdlib.h, socket)

HAS_MEMFD_CREATE ?= $(call exists,,sys/mman.h, memfd_create)

# computing native x11 variables
ifeq ($(UNAME),Linux)
x11_sizeof = $(shell echo "'|sizeof $1| (if x86? $(call sizeof,$1,,-m32 -include X11/Xlib.h) $(call sizeof,$1,,-m64 -include X11/Xlib.h))")
x11_offsetof = $(shell echo "'|$1.$2| (if x86? $(call offsetof,$1,$2,-m32 -include X11/Xlib.h) $(call offsetof,$1,$2,-m64 -include X11/Xlib.h))")
libraries/lib/x11/config.scm:
	@echo "(define-library (lib x11 config)\n\
	(export config)\n\
	(import (scheme core) (owl ff))\n\
	(begin\n\
	   (setq x86? (eq? (size (vm:cast 0 type-vptr)) 4))\n\
	   (define config {\n\
	      $(call x11_sizeof,XEvent)\n\
	      $(call x11_offsetof,XKeyEvent,keycode)\n\
	      $(call x11_offsetof,XButtonEvent,x)\n\
	      $(call x11_offsetof,XButtonEvent,y)\n\
	      $(call x11_offsetof,XButtonEvent,button)\n\
	      $(call x11_offsetof,XConfigureEvent,width)\n\
	      $(call x11_offsetof,XConfigureEvent,height)\n\
	      $(call x11_offsetof,XConfigureEvent,width)\n\
	   })\n\
	))" >$@
endif

