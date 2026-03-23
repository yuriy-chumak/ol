#!/usr/bin/env -S bash -c "make -s -C ../../.. ffi-tier-1-tests"

# ------ ffi tier-1 tests --------------------------------------
.SILENT: ffi-tier-1-tests
ffi-tier-1-tests: ffi-binaries
ffi-tier-1-tests: RUNNER="FFI-Tier-1"
ffi-tier-1-tests: $(wildcard tests/ffi/tier-1/*.scm) \
                  $(wildcard tests/ffi/tier-1/*.bin)
	echo " "
	echo "# $(RUNNER) TESTS"
	FAILMARK=$(shell mktemp -u tmp/fail.FFI.XXXXXXXXX) \
	BACKEND_URL=$(BACKEND_URL) RUNNER=$(RUNNER) SESSION=$(SESSION) \
	TESTS="$^" EXECUTABLE=ffi TEST_HOME=tests/ffi $(MAKE) tests
