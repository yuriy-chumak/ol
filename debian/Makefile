ifndef MAKEFILE_MAIN
$(error Use toplevel Makefile, please.)
else

# Note: correct version of fucking debhelper-compact can be found in the
#       https://packages.ubuntu.com/focal/allpackages?format=txt.gz file
#       please change the "focal" to yours ubuntu name

ppa:
	git checkout debian/changelog
	sed 's/UNRELEASED/$(UNRELEASED)/g' -i debian/changelog

	git checkout debian/control
	# fix fuxking debhelper-compact for fuxking ubuntu build system
	test "$(UNRELEASED)" = "xenial" && sed -i '/debhelper-compat/c\Build-Depends: debhelper-compat (= 9)' debian/control; true

	rm -f debian/files
	dpkg-buildpackage -us -uc -ui -S -sd \
	   -rfakeroot
	VER=`cat debian/changelog| sed -n 1p| sed 's/^.\+(//' |sed 's/).\+$$//'` ;\
	cd ..; debsign ol_$${VER}_source.changes

ppa-publish:
	VER=`cat debian/changelog| sed -n 1p| sed 's/^.\+(//' |sed 's/).\+$$//'` ;\
	cd ..; dput ol ol_$${VER}_source.changes

ppas:
	for debver in xenial bionic focal jammy noble oracular plucky ;\
	do \
	   UNRELEASED=$$debver $(MAKE) ppa ;\
	done
	git checkout debian/changelog

xenial:
	UNRELEASED=xenial $(MAKE) ppa
	git checkout debian/changelog

noble:
	UNRELEASED=noble $(MAKE) ppa
	git checkout debian/changelog

endif