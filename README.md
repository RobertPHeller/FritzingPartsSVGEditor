# FritzingPartsSVGEditor

This program is meant to fill a missing feature of the Fritzing program, an
editor for creating the part graphics.  Inkscape or Illustrator are really
poor choices for creating these sorts of graphics or at least I found 
Inkscape frustrating to use to create breadboard or PCB layouts, esp. when 
compared to KiCAD's footprint editor.  Placing accurately sized graphics in
accurate locations is not really what Inkscape is specifically designed to do.
It is meant for creating artwork.  For my initial parts, I ended up hand 
editing the SVG files with a text editor, with a little scripting help. Then
I wrote this program.  Unlike "typical" graphics editing program, this 
program does not allow for placing or moving graphical elements with the
pointer (mouse, etc.).  Instead, to place or edit a graphical element, a
dialog box is displayed and one enters the exact numerical location, size,
etc.  This might seem clunky to some people, but it is designed to allow 
direct transfer from a datasheet drawing, which after all is loaded down with
actual measurements (numbers!).  For people who have an aversion to their
keyboards, there is always Inkscape or Illustrator.

Fritzing should eventually have its own built-in graphical part editor (much
like KiCAD (and I guess EagleCAD) does.  I have no problem with the Fritzing
developers using this program as a basis or inspiration.

# Building

Tools needed:

GNU Make, Automake, Autoconf, etc.
Doxygen 1.6.0 or later.
Tcl 8.5 or later
Tcllib 
LaTeX (doxygen-latex, pdflatex, makeindex)
unzip
sed
bash

It should build with little or no trouble on a Linux or UNIX system (this
includes MacOSX with all of the build support tools, eg XCode). I built the
MS-Windows (32-bit) using a cross-build environment on an Ubuntu VM. I suspect
that building on a *native* MS-Windows machine will be something of a
challange. I don't have a MS-Windows machine and don't do any actual
development work under MS-Windows. The executable I built for MS-Windows is
not signed -- people with current versions of MS-Windows will likely get nasty
messages and popups from their virus protection software. You should be able
to tell the virus protection software that the program is ok and to run the
program anyway. The MacOSX build was built on an older version of MacOSX, but
it *should* run on current versions of MacOSX. If not, it should be fairly
easy to build from source.  The x86_64 Linux version was built on a (mostly) 
up-to-date CentOS 6 machine and the arm Linux version was built on a (mostly) 
up-to-date Raspbian system (a Raspberry Pi 2B).  The Linux versions should run 
without trouble on any Linux system of the proper arch.  (I did not bother to 
build a 32-bit x86 version, since I doubt many people still use 32-bit 
x86 machines.

Binaries are on my FTP site:

ftp://ftp.deepsoft.com/pub/deepwoods/Other/FritzingPartsSVGEditor/
or
http://files.deepsoft.com/Other/FritzingPartsSVGEditor/

# Questions?

Send me an E-Mail: Robert Heller <heller@deepsoft.com>

