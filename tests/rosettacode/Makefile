.PHONY: rosettacode

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

rosettacode: testing-binaries
rosettacode: $(wildcard tests/rosettacode/*.scm)
	@echo "*** rosettacode.org Ol samples"
	@for F in $(filter-out tests/rosettacode/opengl.scm,$(filter %.scm,$^)) ;do \
	   $(MAKE) -s -B $$F.ok ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"

endif