EXTRA_DIST=Img-1.4-Mac-Universalx86.zip Img-1.4-Linux32.zip \
           Img-1.4-Linux64.zip Img-1.4-win32.zip tclkit-darwin-univ-aqua \
           tclkit-linux-x86 tclkit-linux-x86_64 tclkit-win32.exe \
           Img-1.4-Linuxarmv7l.zip tclkit-linux-armv7l pdf4tcl05.zip \
           emptyindex.ind Tclxml3.1-GenericPureTcl.zip
           

BUILT_SOURCES = tclkit${BEXEEXT}

noinst_SCRIPTS = tclkit${BEXEEXT}

EXTRA_SCRIPTS = tclkit-darwin-univ-aqua tclkit-linux-x86 tclkit-linux-x86_64 \
		 tclkit-win32.exe tclkit-linux-armv7l

tclkit${BEXEEXT}: $(TCLKITBSRC) $(top_builddir)/config.status
	cp $(srcdir)/$(TCLKITBSRC) ./tclkit${BEXEEXT}
	chmod +x ./tclkit${BEXEEXT}

FORCE:

