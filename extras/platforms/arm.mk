HAVE_ARMV7 ?= 0

test-matrix-header: test-matrix-header-arm
test-matrix-header-arm:
	case "`expr $(HAVE_ARMV7) `" in \
		1) printf "| armv7   ";;\
	esac

test-matrix-subheader: test-matrix-subheader-arm
test-matrix-subheader-arm:
	if [ "$(HAVE_ARMV7)" = "1" ]; then printf "|32-d|32-r"; fi
