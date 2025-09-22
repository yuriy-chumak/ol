ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

TEST_FILES += $(wildcard tests/rosettacode/*.scm) $(wildcard tests/rosettacode/sorting_algorithms/*.scm)

.PHONY: rosettacode-tests

regression-tests: rosettacode-tests

# tests without ^.ok file are legal but can't be used in batch mode

.SILENT: rosettacode-tests
rosettacode-tests: olvm-binaries
rosettacode-tests: $(wildcard tests/rosettacode/*.scm) $(wildcard tests/rosettacode/sorting_algorithms/*.scm)
	echo " "
	echo "ROSETTRACODE.ORG OL EXAMPLES"
	$(eval F1LEN=$(shell for F in $(TEST_FILES); do echo $${#F}; done |sort -n| tail -1))
	$(call table-header, $(F1LEN))
	for F in $(filter %.scm %.bin,$^); do \
	   if [ -e $$F.ok ] ;then \
	      printf "|%-$(F1LEN)s " "$$F" |sed 's/ /*/; s/ /./g; s/*/ /' ;\
	      $(MAKE) -s -B $$F.ok ;\
	   fi ;\
	done
	if [ -e $(FAILED) ] ;then rm -f $(FAILED); exit 1 ;fi
	echo "$(green)passed!$(done)"

endif
