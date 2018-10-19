ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# this is only container for config targets
config: config/XVisualInfo config/XEvent

sizeof = \
   @printf "Determining size of $2 structure... ";\
   if echo "\
      \#include $1\n\
      int main() {\
         return (int)sizeof($2);\
      }" | $(CC) -xc - $3 -o /tmp/$2.$$$$; then\
      echo "Ok."; \
      chmod u+x /tmp/$2.$$$$;\
      /tmp/$2.$$$$; printf "(define sizeof:$2 %d)" $$? >$@;\
      rm -f /tmp/$2.$$$$;\
   else\
      echo "\033[0;31mError.\033[0m";\
      printf 0 > $@;\
   fi

config/XEvent:
	$(call sizeof, <X11/Xutil.h>,XEvent)
config/XVisualInfo:
	$(call sizeof, <X11/Xutil.h>,XVisualInfo)

# ...

endif
