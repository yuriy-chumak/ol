.PHONY: rosettacode

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# this tests are legal but can't be used in batch mode
excluded=\
  tests/rosettacode/opengl.scm \
  tests/rosettacode/chat_server.scm \
  tests/rosettacode/draw_a_pixel.scm \
  tests/rosettacode/draw_a_sphere.scm \
  tests/rosettacode/echo_server.scm

rosettacode: testing-binaries
rosettacode: $(wildcard tests/rosettacode/*.scm)
	@echo "*** rosettacode.org Ol samples"
	@for F in $(filter-out $(excluded),$(filter %.scm,$^)) ;do \
	   [ -f $$F.ok ] && $(MAKE) -s -B $$F.ok ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"
endif
