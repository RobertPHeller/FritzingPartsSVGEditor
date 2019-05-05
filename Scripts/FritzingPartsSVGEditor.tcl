#*****************************************************************************
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 5 08:29:23 2019
#  Last Modified : <190505.1022>
#
#  Description	
#
#  Notes
#
#  History
#	
#*****************************************************************************
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
#*****************************************************************************

# $Id$

## @mainpage Fritzing Parts SVG Editor 
# @anchor toc
# <div class="contents">
# <div class="textblock"><ol type="1">
# </ol></div></div>
# @endhtmlonly
#
# @section FritzingPartsSVGEditorSYNOPSIS SYNOPSIS
#
# FritzingPartsSVGEditor [X11 Resource Options] [options] [fileprefix]
#  
# @section FritzingPartsSVGEditorDESCRIPTION DESCRIPTION
#
# The Fritzing Parts SVG Editor creates and edits the SVG files used as the 
# images for the Fritzing Parts editor.  It is \b NOT drawing program (like
# inkscape or Adobe Illustrator).  It is oriented towards placing graphical
# elements precisely, using exact numberical placement value.  Be prepared to
# make extensive use of the oblong thing with the 100 or so buttons on it and
# expect to make little use of the other thing you move around!  There is no
# dragging or dropping or click to place elements.  The lack of such features
# is deliberate.  Please read the whole manual for complete documentation on
# the use of this program.
#
# @section FritzingPartsSVGEditorOPTIONS OPTIONS
#
#
# @section FritzingPartsSVGEditorPARAMETERS PARAMETERS
#
# An option filename prefix.  The files the program uses are named using this 
# prefix, with "_Breadboard.svg", "_Schematic.svg", and "_PCB.svg" appended.
#
# @section FritzingPartsSVGEditorFILES FILES
#
# Preferences file (Tcl/Tk otions format):
#
# $(HOME)/.fritzingpartssvgeditor
#
# @section FritzingPartsSVGEditorAUTHOR AUTHOR
# Robert Heller \<heller\@deepsoft.com\>


set argv0 [file join [file dirname [info nameofexecutable]] [file rootname [file tail [info script]]]]

package require snit
package require Tk
package require tile
package require snitStdMenuBar
package require HTMLHelp 2.0
package require Splash
package require MainWindow
package require ReadConfiguration
package require LabelFrames
package require BreadboardEditor
package require SchematicEditor
package require PCBEditor

global ImageDir 
set ImageDir [file join [file dirname [file dirname [info script]]] \
			Common]
global HelpDir
set HelpDir [file join [file dirname [file dirname [file dirname \
                                                        [info script]]]] Help]

image create photo banner -file [file join $ImageDir banner.gif]

image create photo DeepwoodsBanner -file [file join $ImageDir DeepwoodsBanner.gif]

snit::type FritzingPartsSVGEditor {
    
