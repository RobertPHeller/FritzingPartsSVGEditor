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
#  Last Modified : <190509.1059>
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

## 
# 
# @mainpage Fritzing Parts SVG Editor
# @anchor toc
# @htmlonly
# <div class="contents">
# <div class="textblock"><ol type="1">
# <li><a class="el" href="group__FritzingPartsSVGEditor.html">Fritzing Parts SVG Editor</a><ol type="1">
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorSYNOPSIS">SYNOPSIS</a></li>
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorDESCRIPTION">DESCRIPTION</a></li>
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorOPTIONS">OPTIONS</a></li>
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorPARAMETERS">PARAMETERS</a></li>
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorFILES">FILES</a></li>
# <li><a class="el" href="group__FritzingPartsSVGEditor.html#FritzingPartsSVGEditorAUTHOR">AUTHOR</a></li>
# </ol></li>
# <li><a class="el" href="mainGUI.html">Main GUI</a><ol type="a">
# <li><a class="el" href="mainGUI.html#FileMenu">File menu</a></li>
# <li><a class="el" href="mainGUI.html#EditMenu">Edit menu</a></li>
# <li><a class="el" href="mainGUI.html#OptionsMenu">Options menu</a></li>
# <li><a class="el" href="mainGUI.html#HelpMenu">Help menu</a></li>
# </ol></li>
# <li><a class="el" href="preferences.html">Preferences</a></li>
# <li><a class="el" href="FritzingPartsEditor.html">Common GUI elements</a></li>
# <li><a class="el" href="FritzingPartsBreadboardEditor.html">Breadboard Editor</a></li>
# <li><a class="el" href="FritzingPartsSchematicEditor.html">Schematic Editor</a></li>
# <li><a class="el" href="FritzingPartsPCBEditor.html">PCB Editor</a></li>
# <li><a class="el" href="help.html">Help</a></li>
# <li><a class="el" href="Version.html">Version</a></li>
# <li><a class="el" href="Copying.html">Copying</a><ol type="a">
# <li><a class="el" href="Copying.html#Warranty">Warranty</a></li>
# </ol></li>
# </ol></div></div>
# @endhtmlonly
# @latexonly
# This program is meant to fill a missing feature of the Fritzing program, an
# editor for creating the part graphics.  Inkscape or Illustrator are really
# poor choices for creating these sorts of graphics or at least I found 
# Inkscape frustrating to use to create breadboard or PCB layouts, esp. when 
# compared to KiCAD's footprint editor.  Placing accurately sized graphics in
# accurate locations is not really what Inkscape is specifically designed to do.
# It is meant for creating artwork.  For my initial parts, I ended up hand 
# editing the SVG files with a text editor, with a little scripting help. Then
# I wrote this program.  Unlike "typical" graphics editing program, this 
# program does not allow for placing or moving graphical elements with the
# pointer (mouse, etc.).  Instead, to place or edit a graphical element, a
# dialog box is displayed and one enters the exact numerical location, size,
# etc.  This might seem clunky to some people, but it is designed to allow 
# direct transfer from a datasheet drawing, which after all is loaded down with
# actual measurements (numbers!).  For people who have an aversion to their
# keyboards, there is always Inkscape or Illustrator.
#
# Fritzing should eventually have its own built-in graphical part editor (much
# like KiCAD (and I guess EagleCAD) does.  I have no problem with the Fritzing
# developers using this program as a basis or inspiration.
# @endlatexonly
#
# @defgroup FritzingPartsSVGEditor Fritzing Parts SVG Editor
# Create and edit SVG SVG files used as the images for the Fritzing Parts editor.
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
# elements precisely, using exact numberical placement values.  Be prepared to
# make extensive use of the oblong thing with the 100 or so buttons on it and
# expect to make little use of the other thing you move around!  There is no
# dragging or dropping or click to place elements.  The lack of such features
# is deliberate.  Please read the whole manual for complete documentation on
# the use of this program.
#
# @section FritzingPartsSVGEditorOPTIONS OPTIONS
#
# None at present.
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
# \$(HOME)/.fritzingpartssvgeditor
#
# @section FritzingPartsSVGEditorAUTHOR AUTHOR
# Robert Heller \<heller\@deepsoft.com\>
#
# @page mainGUI Main GUI
# The main gui, shown below, has standard menu bar and three tabs, one for 
# each of the three images used in Fritzing: Breadboard view, Schematic, and
# PCB view.
# @image latex Typical_MainGUI.png "The main GUI of the Fritzing Parts SVG Editor program" width=4in
# @image html  Typical_MainGUISmall.png
# There are four menus on the top menu bar, a file menu, an edit menu, an
# options (preferences) menu, and a help menu.
# @section FileMenu File menu
# @image latex FileMenu.png "The File menu"
# @image html  FileMenu.png
# The \b File menu has the has the standard items: \b New, which clears the 
# current part, \b Open, which loads a part from disk, \b Save and 
# \b Save \b As..., which saves the current part to disk, and \b Edit, which
# exits the progra,
# @section EditMenu Edit menu
# @image latex EditMenu.png "The Edit menu"
# @image html  EditMenu.png
# The \b Edit menu has the standard editing related items.
# @section OptionsMenu Options menu
# @image latex OptionsMenu.png "The Options menu"
# @image html  OptionsMenu.png
# The \b Options menu has three items: \b Edit \b Configuration, 
# \b Load \b Configuration, and \b Save \b Configuration.  These items allow
# for editing, loading, and saving the configuration (preferences).  See
# \ref preferences.
# @section HelpMenu Help menu
# @image latex HelpMenu.png "The Help menu" width=4in
# @image html  HelpMenuSmall.png
# The \b Help menu contains a top-level index into the included help pages.
# @page preferences Preferences
# The preferences are stored in a text file in the user's home directory 
# (folder).  The file is named \c .fritzingpartssvgeditor under Linux and 
# MacOSX and \c fritzingpartssvgeditor.rc under MS-Windows.  There are five 
# preferences:
# 
#  <dl>
#  <dt>Units</dt><dd>The units to use for the width and height. Can me either 
#  mm (milimeters) or inch (inches)</dd>
#  <dt>Width</dt><dd>The real world width (in Units above) of the viewport.</dd>
#  <dt>Height</dt><dd>The real world height (in Units above) of the viewport.</dd>
#  <dt>Viewport Width</dt><dd>The numerical width of the viewport</dd>
#  <dt>Viewport Height</dt><dd>The numerical height of the viewport</dd>
#  </dl>
#
# These are the default initial values to use.  When loading a file, the 
# values stored in the file are used.  The aspect ratio of the Width to Height
# should be the same as the aspect ratio of the Viewport Width to Viewport 
# Height to insure square pixels.  Typically the Viewport Width and Viewport 
# Height will be a constant multiple of the Width and Height respectively.  The
# Viewport determines the coordinate system used to place and size graphical 
# elements.  The Viewport origin (upper left corner) is always 0,0.

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
package require Version

global ImageDir 
set ImageDir [file join [file dirname [file dirname [info script]]] \
			Common]
global HelpDir
set HelpDir [file join [file dirname [file dirname [file dirname \
                                                        [info script]]]] Help]

image create photo banner -file [file join $ImageDir banner.gif]

image create photo DeepwoodsBanner -file [file join $ImageDir DeepwoodsBanner.gif]


snit::type FritzingPartsSVGEditorPreferences {
    ::ReadConfiguration::ConfigurationType \
          {Units units enumerated mm {mm inch}} \
          {Width width double 25.4 {0 100}} \
          {Height height double 25.4 {0 100}} \
          {{Viewport Width} vpwidth double 254 {0 1000}} \
          {{Viewport Height} vpheight double 254 {0 1000}}
          
}

snit::type FritzingPartsSVGEditor {
    pragma -hastypeinfo    no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typecomponent splash
    typecomponent mainwindow
    typecomponent   notebook
    typecomponent     breadboardeditor
    typecomponent     schematiceditor
    typecomponent     pcbeditor
    typevariable  _clean
    typevariable  _dirty
    
    typecomponent preferences
    delegate typemethod {preferences *} to preferences

    typevariable _menu {
        {&File} {file:menu} {file} 0 {
            {command {&New} {file:new} {New blank SVG files} {Ctrl n} -command "[mytypemethod _new]"}
            {command {&Open} {file:open} {Open existing SVG files} {Ctrl o} -command "[mytypemethod _open]"}
            {command {&Save} {file:save} {Save} {Ctrl s} -command "[mytypemethod _save]"}
            {command {Save &As...} {file:saveas} {Save} {Ctrl a} -command "[mytypemethod _saveas]"}
            {command {&Exit} {file:exit} {Exit} {Ctrl q} -command "[mytypemethod _exit]"}
        } {&Edit} {edit edit:menu} {edit} 0 {
            {command {Cu&t} {edit:cut edit:havesel} {Cut selection to the paste buffer} {Ctrl x} -command {StdMenuBar EditCut} -state disabled}
            {command {&Copy} {edit:coy edit:havesel} {Copy selection to the paste buffer} {Ctrl c} -command {StdMenuBar EditCopy} -state disabled}
            {command {&Paste} {edit:paste} {Paste selection from the paste buffer} {Ctrl c} -command {StdMenuBar EditPaste}}
            {command {C&lear} {edit:clear edit:havesel} {Clear selection} {} -command {StdMenuBar EditClear} -state disabled}
            {command {&Delete} {edit:delete edit:havesel} {Delete selection} {Ctrl d}  -command {StdMenuBar EditClear} -state disabled}
            {separator}
            {command {Select All} {edit:selectall} {Select everything} {} -command {StdMenuBar EditSelectAll}}
            {command {De-select All} {edit:deselectall edit:havesel} {Select nothing} {} -command {StdMenuBar EditSelectNone} -state disabled}
        } {&Options} {options} {options} 0 {
            {command {Edit Configuration} {options:edit} {Edit Configuration} {} -command "[mytypemethod preferences edit]"}
            {command {Load Configuration} {options:read} {Load Configuration} {} -command "[mytypemethod preferences load]"}
            {command {Save Configuration} {options:save} {Save Configuration} {} -command "[mytypemethod preferences save]"}
        } {&Help} {help} {help} 0 {
            {command {On &Help...} {help:help} {Help on help} {} -command {HTMLHelp help Help}}
            {command {On &Version} {help:help} {Version} {} -command {HTMLHelp help Version}}
            {command {Warranty} {help:help} {Warranty} {} -command {HTMLHelp help Warranty}}
            {command {Copying} {help:help} {Copying} {} -command {HTMLHelp help Copying}}
            {command {Invoking from the shell} {help:help} {Invoking} {} -command {HTMLHelp help {Fritzing Parts SVG Editor}}}
            {command {Main GUI} {help:help} {Main GUI} {} -command {HTMLHelp help {Main GUI}}}
            {command {Preferences} {help:help} {Preferences} {} -command {HTMLHelp help {Preferences}}}
            {command {Common GUI elements} {help:help} {Common GUI elements} {} -command {HTMLHelp help {Common GUI elements}}}
            {command {Breadboard Editor} {help:help} {Breadboard Editor} {} -command {HTMLHelp help {Breadboard Editor}}}
            {command {Schematic Editor} {help:help} {Schematic Editor} {} -command {HTMLHelp help {Schematic Editor}}}
            {command {PCB Editor} {help:help} {PCB Editor} {} -command {HTMLHelp help {PCB Editor}}}
        }
    }
    
    typevariable _currentFilePrefix {}
    typevariable _currentProgress 0
    
    typemethod _dirtyHandler {tabid dirty} {
        if {[llength [$notebook tabs]] <= $tabid} {return}
        if {$dirty} {
            $notebook tab $tabid -image $_dirty
        } else {
            $notebook tab $tabid -image $_clean
        }
    }
    
        
    
    typeconstructor {
        global argv
        
        set splash [splash .fritzingpeditSlash \
                    -title [format {Fritzing Parts SVG Editor %s on %s, Copyright (C) 2019 Robert Heller D/B/A Deepwoods Software. The Fritzing Parts SVG Editor comes with ABSOLUTELY NO WARRANTY; for details select 'Warranty...' under the Help menu.  This is free software, and you are welcome to redistribute it under certain conditions; select 'Copying...' under the Help menu.} \
                            $Version::VERSION $Version::target] \
                    -icon banner -image DeepwoodsBanner -background {#2ba2bf} \
                    -titleforeground white -statusforeground {black}]
        $type SplashWorkMessage "Building Main window" 0
        wm protocol . WM_DELETE_WINDOW "[mytypemethod _exit]"
        #puts stderr "*** set up WM_DELETE_WINDOW"
        wm withdraw .
        wm title . [format {Fritzing Parts SVG Editor %s on %s} \
                    $Version::VERSION $Version::target]
        set menu [subst $_menu]
        #puts stderr "*** menu is $menu"
        set mainwindow [mainwindow .main -menu $menu -scrolling no]
        pack $mainwindow -expand yes -fill both
        #$mainwindow toolbar add tools
        #$mainwindow toolbar show tools
        set frame [$mainwindow scrollwindow getframe]
        set notebook [ttk::notebook $frame.notebook]
        $mainwindow scrollwindow setwidget $notebook
        HTMLHelp setDefaults "$::HelpDir" "index.html#toc"
        set preferences FritzingPartsSVGEditorPreferences
        $type preferences load
        set units [$preferences getoption units]
        set width [$preferences getoption width]
        set height [$preferences getoption height]
        set viewport [list 0 0 [$preferences getoption vpwidth] [$preferences getoption vpheight]]
        $type SplashWorkMessage "Building breadboard editor" 20
        set _clean [IconImage image clean]
        set _dirty [IconImage image dirty]
        set breadboardeditor [BreadboardEditor $notebook.breadboardeditor \
                              -dirtyhandler [mytypemethod _dirtyHandler] \
                              -dirtyhandlercontext 0 -units $units \
                              -width $width -height $height \
                              -viewport $viewport]
        $notebook add $breadboardeditor -sticky news -text Breadboard \
              -image $_clean -compound right
        $type SplashWorkMessage "Building schematic editor" 40
        set schematiceditor [SchematicEditor $notebook.schematiceditor \
                             -dirtyhandler [mytypemethod _dirtyHandler] \
                             -dirtyhandlercontext 1 -units $units \
                              -width $width -height $height \
                              -viewport $viewport]
        $notebook add $schematiceditor -sticky news -text Schematic \
              -image $_clean -compound right
        $type SplashWorkMessage "Building PCB editor" 60
        set pcbeditor [PCBEditor $notebook.pcbeditor \
                       -dirtyhandler [mytypemethod _dirtyHandler] \
                       -dirtyhandlercontext 2 -units $units \
                              -width $width -height $height \
                              -viewport $viewport]
        $notebook add $pcbeditor -sticky news -text PCB \
              -image $_clean -compound right
        ttk::notebook::enableTraversal $notebook
        $notebook select 0
        $mainwindow showit
        $type SplashWorkMessage "Done" 100
        update idle
        if {[llength $argv] > 0} {
            $type _open [lindex $argv 0]
        }
    }
    typemethod SplashWorkMessage {message percent} {
        incr _currentProgress $percent
        if {$_currentProgress > 100} {set _currentProgress 100}
        .fritzingpeditSlash update "$message" $_currentProgress
        raise .fritzingpeditSlash
        update
        if {$_currentProgress >= 100} {
            after 10000 [list catch "destroy .fritzingpeditSlash"]
        }
    }
        
    typemethod _exit {{dontask no}} {
        if {$dontask} {
            set answer yes
        } else {
            if {[$breadboardeditor isDirty] ||
                [$schematiceditor isDirty] ||
                [$pcbeditor isDirty]} {
                set answer [tk_messageBox -default cancel -icon question -type yesnocancel -message "There are unsaved changes, save before exit?"]
            } else {
                set answer [tk_messageBox -default cancel -icon question -type okcancel -message {Really Exit?}]
            }
        }
        switch -exact $answer {
            cancel {return}
            ok -
            no {
                exit
            }
            yes {
                $type _save
                exit
            }
        }
    }
    typemethod _new {} {
        if {[$breadboardeditor isDirty] || 
            [$schematiceditor isDirty] ||
            [$pcbeditor isDirty]} {
            if {[tk_messageBox -type yesno -default no -message "This will discard the current unsaved changes.  Are you sure?"]} {
                $breadboardeditor clean
                $schematiceditor clean
                $pcbeditor clean
                return yes
            } else {
                return no
            }
        } else {
            return yes
        }
    }
    proc getOpenPrefix {defaultprefix} {
        if {$defaultprefix ne ""} {
            set defaultfile "${defaultprefix}_Breadboard.svg"
        } else {
            set defaultfile {}
        }
        set filename [tk_getOpenFile -defaultextension .svg \
                      -filetypes { {{SVG files} {.svg} } {{All Files} * }} \
                      -initialdir [pwd] -initialfile $defaultfile \
                      -parent .]
        if {$filename eq ""} {return ""}
        if {[regsub {_Breadboard.svg$} $filename {} prefix] ||
            [regsub {_Schematic.svg$} $filename {} prefix] ||
            [regsub {_PCB.svg$} $filename {} prefix]} {
            return $prefix
        } else {
            return [file rootname $filename]
        }
    }
    proc getSavePrefix {defaultprefix} {
        if {$defaultprefix ne ""} {
            set defaultfile "${defaultprefix}_Breadboard.svg"
        } else {
            set defaultfile {}
        }
        set filename [tk_getSaveFile -defaultextension .svg \
                      -filetypes { {{SVG files} {.svg} } {{All Files} * }} \
                      -initialdir [pwd] -initialfile $defaultfile \
                      -parent .]
        if {$filename eq ""} {return ""}
        if {[regsub {_Breadboard.svg$} $filename {} prefix] ||
            [regsub {_Schematic.svg$} $filename {} prefix] ||
            [regsub {_PCB.svg$} $filename {} prefix]} {
            return $prefix
        } else {
            return [file rootname $filename]
        }
    }
    typemethod _open {{fileprefix {}}} {
        if {![$type _new]} {return}
        if {$fileprefix eq ""} {set fileprefix [getOpenPrefix ""]}
        if {$fileprefix eq ""} {return}
        if {[file readable "${fileprefix}_Breadboard.svg"]} {
            $breadboardeditor read "${fileprefix}_Breadboard.svg"
        }
        if {[file readable "${fileprefix}_Schematic.svg"]} {
            $schematiceditor read "${fileprefix}_Schematic.svg"
        }
        if {[file readable "${fileprefix}_PCB.svg"]} {
            $pcbeditor read "${fileprefix}_PCB.svg"
        }
        set _currentFilePrefix $fileprefix
    }
    typemethod _save {} {
        $type _saveas $_currentFilePrefix
    }
    typemethod _saveas {{fileprefix {}}} {
        if {$fileprefix eq ""} {set fileprefix [getSavePrefix ""]}
        if {$fileprefix eq ""} {return}
        $breadboardeditor write "${fileprefix}_Breadboard.svg"
        $schematiceditor write "${fileprefix}_Schematic.svg"
        $pcbeditor write "${fileprefix}_PCB.svg"
        set _currentFilePrefix $fileprefix
    }
}

