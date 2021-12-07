.PHONY: check rosettacode

check: rosettacode

ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# tests without ^.ok file are legal but can't be used in batch mode

rosettacode: testing-binaries
rosettacode: $(wildcard tests/rosettacode/*.scm) $(wildcard tests/rosettacode/sorting_algorithms/*.scm)
	@echo "*** rosettacode.org Ol samples"
	@for F in $(filter %.scm,$^) ;do \
	   if [ -e $$F.ok ] ;then $(MAKE) -s -B $$F.ok ;fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "passed!"
endif
