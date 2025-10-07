VERSION=$(shell git describe)

BACKEND_URL=http://localhost:8008/ol/test-matrix
SESSION=$(shell curl -s -X POST "$(BACKEND_URL)?build=$(VERSION)")

check:
	# start new test session
	# (already started with $(SESSION)
	
	# run
	BACKEND_URL=$(BACKEND_URL) SESSION=$(SESSION) \
	$(MAKE) -s -C ../.. check
