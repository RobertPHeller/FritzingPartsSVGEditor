##-*- makefile -*-############################################################
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 5 08:13:34 2019
#  Last Modified : <190510.0818>
#
#  Description	
#
#  Notes
#
#  History
#	
#  $Log$
#
##############################################################################
#
#    Copyright (C) 2019  Robert Heller D/B/A Deepwoods Software
#			51 Locke Hill Road
#			Wendell, MA 01379-9728
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
# 
#
##############################################################################


ACLOCAL_AMFLAGS = -I m4

SUBDIRS = BuildScripts BuildData Scripts ScreenShots ExtraDoc
EXTRA_DIST = README.md Doxyfile.in FritzingPartsSVGEditor.png \
	FritzingPartsSVGEditor.desktop

desktopdir = $(datadir)/applications
desktop_DATA = Dispatcher.desktop

pixmapdir = $(datadir)/pixmaps
pixmap_DATA = Dispatcher.png

clean-local:
	-rm -rf man html latex FritzingPartsSVGEditor.pdf

FritzingPartsSVGEditorModules = Scripts/FritzingPartsBreadboardEditor.tcl \
				Scripts/FritzingPartsSchematicEditor.tcl \
				Scripts/FritzingPartsPCBEditor.tcl \
				Scripts/FritzingPartsEditor.tcl

FritzingPartsSVGEditorLib = $(wildcard Scripts/*.tcl) \
			    $(wildcard Scripts/*.gif) \
			    $(wildcard Scripts/*.png) \
			    $(wildcard Scripts/*.xbm) \
			    $(wildcard Scripts/*.xpm)

bin_SCRIPTS = FritzingPartsSVGEditor$(EXEEXT)
man_MANS = man1/FritzingPartsSVGEditor.1
pkgdatadir = $(datadir)/$(PACKAGE)/Doc
pkgdata_DATA = FritzingPartsSVGEditor.pdf


TCLKIT = $(top_builddir)/BuildData/tclkit${BEXEEXT}
BUILT_SOURCES = $(TCLKIT)

FritzingPartsSVGEditor$(EXEEXT) : Scripts/FritzingPartsSVGEditor.tcl \
	$(FritzingPartsSVGEditorLib) $(top_builddir)/html/index.html \
	ExtraDoc/Version.tcl
	-rm -rf FritzingPartsSVGEditor.vfs
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit qwrap $(srcdir)/Scripts/FritzingPartsSVGEditor.tcl
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit unwrap FritzingPartsSVGEditor.kit
	-rm -rf FritzingPartsSVGEditor.kit
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit FritzingPartsSVGEditor \
		lib $(SNITLIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitDir.kit FritzingPartsSVGEditor \
		lib $(URILIB)
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit -srcdir $(srcdir)/Scripts FritzingPartsSVGEditor \
		lib/Common $(FritzingPartsSVGEditorLib) ExtraDoc/Version.tcl
	$(TCLKIT) $(top_srcdir)/BuildScripts/MakePkgIndex.kit FritzingPartsSVGEditor \
		Common
	$(UNZIP) -qq -d FritzingPartsSVGEditor.vfs/lib \
		$(top_srcdir)/BuildData/$(IMG_ZIP)
	$(UNZIP) -qq -d FritzingPartsSVGEditor.vfs/lib \
		$(top_srcdir)/BuildData/pdf4tcl05.zip
	$(UNZIP) -qq -d FritzingPartsSVGEditor.vfs/lib \
		$(top_srcdir)/BuildData/Tclxml3.1-GenericPureTcl.zip
	$(TCLKIT) $(top_srcdir)/BuildScripts/AddKitFile.kit FritzingPartsSVGEditor \
		Help $(top_builddir)/html/*
	$(TCLKIT) $(top_srcdir)/BuildScripts/sdx.kit wrap FritzingPartsSVGEditor${EXEEXT} -runtime $(top_srcdir)/BuildData/$(TCLKITRUN)
	-rm -rf FritzingPartsSVGEditor.vfs FritzingPartsSVGEditor.kit
if MACOSX 
# create application for Mac OSX                                                
	-rm -rf FritzingPartsSVGEditor.app
	mkdir -p FritzingPartsSVGEditor.app/Contents/MacOS/
	cp FritzingPartsSVGEditor$(EXEEXT) FritzingPartsSVGEditor.app/Contents/MacOS/
	mkdir -p FritzingPartsSVGEditor.app/Contents/Resources/
	-rm -rf FritzingPartsSVGEditor.iconset
	mkdir FritzingPartsSVGEditor.iconset
	sips -z 16 16     $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_16x16.png
	sips -z 32 32     $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_16x16@2x.png
	sips -z 32 32     $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_32x32.png
	sips -z 64 64     $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_32x32@2x.png
	sips -z 128 128   $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_128x128.png
	sips -z 256 256   $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_128x128@2x.png
	sips -z 256 256   $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_256x256.png
	sips -z 512 512   $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_256x256@2x.png
	sips -z 512 512   $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_512x512.png
	sips -z 1024 1024 $(srcdir)/FritzingPartsSVGEditor.png --out FritzingPartsSVGEditor.iconset/icon_512x512@2x.png
	iconutil -c icns FritzingPartsSVGEditor.iconset
	-rm -rf FritzingPartsSVGEditor.iconset
	cp FritzingPartsSVGEditor.icns FritzingPartsSVGEditor.app/Contents/Resources/
	$(TCLKIT) $(top_srcdir)/BuildScripts/MacOSXPlist.kit FritzingPartsSVGEditor 1.0 "1994,1995,2002-2005  Robert Heller D/B/A Deepwoods Software"
	plutil -convert binary1 -o FritzingPartsSVGEditor.app/Contents/Info.plist \
		FritzingPartsSVGEditor.plist
	-rm -f FritzingPartsSVGEditor.plist FritzingPartsSVGEditor.icns

install-exec-local: FritzingPartsSVGEditor.app
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(top_srcdir)/BuildScripts/macappInstall.sh FritzingPartsSVGEditor.app $(DESTDIR)$(bindir)

FritzingPartsSVGEditor.app: FritzingPartsSVGEditor${BEXEEXT}

endif


$(top_builddir)/html/index.html \
	man1/FritzingPartsSVGEditor.1 \
	FritzingPartsSVGEditor.pdf : Doxyfile Scripts/FritzingPartsSVGEditor.tcl \
		$(FritzingPartsSVGEditorModules) $(wildcard ExtraDoc/*.h) \
		ExtraDoc/Version.tcl BuildScripts/tcldoxyfilter.sh
	-rm -rf man1 html latex
	$(DOXYGEN)
	$(MAKE) -C latex
	mv latex/refman.pdf ./FritzingPartsSVGEditor.pdf


Doxyfile: Doxyfile.in
	

$(top_builddir)/BuildData/tclkit${BEXEEXT}: FORCE
	$(MAKE) -C $(top_builddir)/BuildData tclkit${BEXEEXT}

ExtraDoc/Version.tcl: FORCE
	$(MAKE) -C $(top_builddir)/ExtraDoc Version.tcl

FORCE:

