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
#  Last Modified : <190507.2302>
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
# <li><a class="el" href="mainGUI.html">Main GUI</a></li>
# <li><a class="el" href="preferences.html">Preferences</a></li>
# <li><a class="el" href="help.html">Help</a></li>
# <li><a class="el" href="Version.html">Version</a></li>
# <li><a class="el" href="Copying.html">Copying</a><ol type="a">
# <li><a class="el" href="Copying.html#Warranty">Warranty</a></li>
# </ol></li>
# </ol></div></div>
# @endhtmlonly
# @latexonly
# Front page content. TBD
# @endlatexonly
#
# @defgroup FritzingPartsSVGEditor Fritzing Parts SVG Editor
# @brief Create and edit SVG SVG files used as the images for the Fritzing Parts editor.
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
# TBD
# @page preferences Preferences
# TBD


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
            {command {Reference Manual} {help:help} {} {} -command {HTMLHelp help "Fritzing Parts SVG Editor Reference"}}
        }
    }
    
    typevariable _currentFilePrefix {}
    typevariable _currentProgress 0
    
    typemethod _dirtyHandler {tabid dirty} {
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
        if {[llength $argv] > 0} {
            $type SplashWorkMessage "Loading files" 80
            $type _open [lindex $argv 0]
        }
        $type SplashWorkMessage "Done" 100
        $mainwindow showit
                        
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
                $type save
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
            #$schematiceditor read "${fileprefix}_Schematic.svg"
        }
        if {[file readable "${fileprefix}_PCB.svg"]} {
            #$pcbeditor read "${fileprefix}_PCB.svg"
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

