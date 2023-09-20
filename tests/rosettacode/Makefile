ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

.PHONY: check rosettacode
.SILENT: rosettacode

check: rosettacode

# tests without ^.ok file are legal but can't be used in batch mode

rosettacode: testing-binaries
rosettacode: $(wildcard tests/rosettacode/*.scm) $(wildcard tests/rosettacode/sorting_algorithms/*.scm)
	@echo "rosettacode.org Ol samples"
	@echo "--------------------------"
	$(eval F1LEN=$(shell for F in $^; do echo $${#F}; done |sort -n| tail -1))
	$(call table-header, $(F1LEN))
	@for F in $(filter %.scm,$^) ;do \
	   if [ -e $$F.ok ] ;then \
	      printf "%-$(F1LEN)s " "$$F" ;\
	      $(MAKE) -s -B $$F.ok ;\
	   fi ;\
	done
	@if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	@echo "\033[1;32mpassed!\033[0m"

endif
