#!/usr/bin/env -S bash -c "make -s -C ../../.. ffi-tier-2-tests"

# ------ ffi tier-2 tests --------------------------------------
.SILENT: ffi-tier-2-tests
ffi-tier-2-tests: ffi-binaries
ffi-tier-2-tests: RUNNER="FFI-Tier-2"
ffi-tier-2-tests: $(wildcard tests/ffi/tier-2/*.scm) \
                  $(wildcard tests/ffi/tier-2/*.bin)
	echo " "
	echo "# $(RUNNER) TESTS"
	FAILMARK=$(shell mktemp -u tmp/fail.FFI.XXXXXXXXX) \
	BACKEND_URL=$(BACKEND_URL) RUNNER=$(RUNNER) SESSION=$(SESSION) \
	TESTS="$^" EXECUTABLE=ffi TEST_HOME=tests/ffi $(MAKE) tests
