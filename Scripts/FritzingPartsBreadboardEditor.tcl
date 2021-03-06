#*****************************************************************************
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 5 14:37:01 2019
#  Last Modified : <190928.1051>
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

## @page FritzingPartsBreadboardEditor Breadboard Editor
# The breadboard editor implements six graphical elements: pins, 
# rectangles, lines, circles, arcs, and text, using six tool buttons.
# Each of the tool buttons opens a dialog box where the user can enter the
# information about the graphical element.
# @section BreadboardPin Pins
# Breadboard pins are always drawn as filled circles.  The breadboard pin 
# dialog box, shown below, asks for the position (center), the diameter, the
# color, and the pin number.  Pins are automatically auto-incremented starting 
# at the last pin entered.  The position is in viewport coordinates, as is the 
# diameter. The default color is black.
# @image latex BreadboardPin.png "Breadboard Pin Dialog" width=4in
# @image html  BreadboardPinSmall.png
# @section BreadboardRect Rectangles
# Rectangles can be filled with a solid color or just be an outline.  The 
# breadboard rectangle dialog box, shown below, asks for the position (upper 
# left corner), size (width and height), the line thickness (for outline 
# rectangles), color, and if you want the rectangle filled or not.  The 
# position and size are in viewport coordinates. The default color is black.
# @image latex BreadboardRect.png "Breadboard Rectangle Dialog" width=4in
# @image html  BreadboardRectSmall.png
# @section BreadboardLine Lines
# The breadboard line dialog box, shown below, asks for the endpoints of the
# line (X1, Y1, X2, Y2), the line thickness and the color. The endpoints are
# in viewport coordinates. The default color is black.
# @image latex BreadboardLine.png "Breadboard Line Dialog" width=4in
# @image html  BreadboardLineSmall.png
# @section BreadboardCirc Circles
# Breadboard circles can be filled or hollow. The breadboard circle dialog 
# box, shown below, asks for the position (center), the diameter, the center 
# size (hole diameter), and the color.  The position and the diameters are in
# viewport coordinates. The default color is black.
# @image latex BreadboardCirc.png "Breadboard Circle Dialog" width=4in
# @image html  BreadboardCircSmall.png
# @section BreadboardArc  Arcs
# Breadboard arcs can be either pie slices or bare arcs.  If the center 
# diameter, the arc will be a pie slice (filled), otherwise, the arc will be a
# bare arc. The breadboard arc dialog box, shown below, asks for the position 
# (center), the diameter, the center size (hole diameter), and the color.  The
# position  and the diameters are in viewport coordinates. The default color 
# is black.
# @image latex BreadboardArc.png "Breadboard Arc Dialog" width=4in
# @image html  BreadboardArcSmall.png
# @section BreadboardText Text
# The breadboard text dialog box, shown below, asks for the position (lower 
# left), the size in points, the font, the text, and the color. The position 
# is in viewport coordinates. The default color is black.
# @image latex BreadboardText.png "Breadboard Text Dialog" width=4in
# @image html  BreadboardTextSmall.png


package require Tk
package require tile
package require FritzingPartsEditor
package require Dialog
package require LabelFrames
package require ParseXML
package require Version

namespace eval Breadboard {
    snit::widgetadaptor AddPinDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -diameter -default 2.0 -type snit::double
        option -color -default black -type ::Color
        option -pinno -default 1 -type snit::integer
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addpin] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new pin} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set xposLE [LabelSpinBox $frame.xposLE -textvariable [myvar options(-xpos)] \
                        -label "X Position" -range {0.0 1000000.0 .1}]
            pack $xposLE -expand yes -fill x
            set yposLE [LabelSpinBox $frame.yposLE -textvariable [myvar options(-ypos)] \
                        -label "Y Position" -range {0.0 1000000.0 .1}]
            pack $yposLE -expand yes -fill x
            set diameterLE [LabelSpinBox $frame.diameterLE -textvariable [myvar options(-diameter)] \
                         -label "Diameter" -range {0.0 10000.0 .1}]
            pack $diameterLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            set pinnoLE [LabelSpinBox $frame.pinnoLE -textvariable [myvar options(-pinno)] \
                         -label "Pin number" -range {1 1000 1}]
            pack $pinnoLE -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-xpos -ypos -diameter -color -pinno -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
    snit::widgetadaptor AddRectDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -width -default 1.0 -type snit::double
        option -height -default 1.0 -type snit::double
        option -linethickness -default 1.0 -type snit::double
        option -color -default black -type ::Color
        option -filled -default false -type snit::boolean
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addrect] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new rectangle} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set xposLE [LabelSpinBox $frame.xposLE -textvariable [myvar options(-xpos)] \
                        -label "X Position" -range {0.0 1000000.0 .1}]
            pack $xposLE -expand yes -fill x
            set yposLE [LabelSpinBox $frame.yposLE -textvariable [myvar options(-ypos)] \
                        -label "Y Position" -range {0.0 1000000.0 .1}]
            pack $yposLE -expand yes -fill x
            set widthLE [LabelSpinBox $frame.widthLE -textvariable [myvar options(-width)] \
                        -label "Width" -range {1.0 1000000.0 .1}]
            pack $widthLE -expand yes -fill x
            set heightLE [LabelSpinBox $frame.heightLE -textvariable [myvar options(-height)] \
                        -label "Height" -range {1.0 1000000.0 .1}]
            pack $heightLE -expand yes -fill x
            set thickLE [LabelSpinBox $frame.thickLE -textvariable [myvar options(-linethickness)] \
                         -label "Line Thickness" -range {1.0 100.0 .1}]
            pack $thickLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            set filledCK [ttk::checkbutton $frame.filledCK \
                          -text "Filled? " \
                          -offvalue false -onvalue true \
                          -variable [myvar options(-filled)]]
            pack $filledCK -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-xpos -ypos -width -height -linethickness -color -filled -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
    snit::widgetadaptor AddLineDialog {
        option -x1 -default 0.0 -type snit::double
        option -y1 -default 0.0 -type snit::double
        option -x2 -default 1.0 -type snit::double
        option -y2 -default 1.0 -type snit::double
        option -linethickness -default 1.0 -type snit::double
        option -color -default black -type ::Color
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addline] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new line} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set x1LE [LabelSpinBox $frame.x1LE -textvariable [myvar options(-x1)] \
                        -label "X1" -range {0.0 1000000.0 .1}]
            pack $x1LE -expand yes -fill x
            set y1LE [LabelSpinBox $frame.y1LE -textvariable [myvar options(-y1)] \
                        -label "Y1" -range {0.0 1000000.0 .1}]
            pack $y1LE -expand yes -fill x
            set x2LE [LabelSpinBox $frame.x2LE -textvariable [myvar options(-x2)] \
                        -label "X2" -range {1.0 1000000.0 .1}]
            pack $x2LE -expand yes -fill x
            set y2LE [LabelSpinBox $frame.y2LE -textvariable [myvar options(-y2)] \
                        -label "Y2" -range {1.0 1000000.0 .1}]
            pack $y2LE -expand yes -fill x
            set thickLE [LabelSpinBox $frame.thickLE -textvariable [myvar options(-linethickness)] \
                         -label "Line Thickness" -range {1.0 100.0 .1}]
            pack $thickLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-x1 -y1 -x2 -y2 -linethickness -color -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
    snit::widgetadaptor AddCircDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -diameter -default 2.0 -type snit::double
        option -center -default 1.0 -type snit::double
        option -color -default black -type ::Color
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addcirc] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new circle} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set xposLE [LabelSpinBox $frame.xposLE -textvariable [myvar options(-xpos)] \
                        -label "X Position" -range {0.0 1000000.0 .1}]
            pack $xposLE -expand yes -fill x
            set yposLE [LabelSpinBox $frame.yposLE -textvariable [myvar options(-ypos)] \
                        -label "Y Position" -range {0.0 1000000.0 .1}]
            pack $yposLE -expand yes -fill x
            set diameterLE [LabelSpinBox $frame.diameterLE -textvariable [myvar options(-diameter)] \
                         -label "Diameter" -range {0.0 10000.0 .1}]
            pack $diameterLE -expand yes -fill x
            set centerLE [LabelSpinBox $frame.centerLE -textvariable [myvar options(-center)] \
                         -label "Center size" -range {0.0 10000.0 .1}]
            pack $centerLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-xpos -ypos -diameter -center -color -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
    snit::widgetadaptor AddArcDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -diameter -default 2.0 -type snit::double
        option -start -default 0.0 -type ::Angle
        option -extent -default 360.0 -type ::Angle
        option -center -default 0.0 -type snit::double
        option -color -default black -type ::Color
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addarc] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new arc} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set xposLE [LabelSpinBox $frame.xposLE -textvariable [myvar options(-xpos)] \
                        -label "X Position" -range {0.0 1000000.0 .1}]
            pack $xposLE -expand yes -fill x
            set yposLE [LabelSpinBox $frame.yposLE -textvariable [myvar options(-ypos)] \
                        -label "Y Position" -range {0.0 1000000.0 .1}]
            pack $yposLE -expand yes -fill x
            set diameterLE [LabelSpinBox $frame.diameterLE \
                            -textvariable [myvar options(-diameter)] \
                         -label "Diameter" -range {0.0 10000.0 .1}]
            pack $diameterLE -expand yes -fill x
            set startLE [LabelSpinBox $frame.startLE \
                         -textvariable [myvar options(-start)] \
                         -label "Start of arc" -range {-360.0 360.0 .1}]
            pack $startLE -expand yes -fill x
            set extentLE [LabelSpinBox $frame.extentLE \
                         -textvariable [myvar options(-extent)] \
                         -label "Extent of arc" -range {-360.0 360.0 .1}]
            pack $extentLE -expand yes -fill x
            set centerLE [LabelSpinBox $frame.centerLE -textvariable [myvar options(-center)] \
                         -label "Center hole" -range {1.0 100.0 .1}]
            pack $centerLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-xpos -ypos -diameter -start -extent -center -color -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
    snit::widgetadaptor AddTextDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -size -default 1   -type snit::integer
        option -font -default DroidSans -type ::FontName
        option -text -default ""
        option -color -default black -type ::Color
        component attributes
        delegate option -attributes to attributes as -values
        delegate option -parent to hull
        delegate option -title to hull
        constructor {args} {
            installhull using Dialog -image [IconImage image addtext] \
                  -default add -cancel cancel -modal local -transient yes \
                  -side bottom -title {Add new text} -parent [from args -parent]
            $hull add add    -text Add    -command [mymethod _Add]
            $hull add cancel -text Cancel -command [mymethod _Cancel]
            wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
            set frame [$hull getframe]
            set xposLE [LabelSpinBox $frame.xposLE -textvariable [myvar options(-xpos)] \
                        -label "X Position" -range {0.0 1000000.0 .1}]
            pack $xposLE -expand yes -fill x
            set yposLE [LabelSpinBox $frame.yposLE \
                        -textvariable [myvar options(-ypos)] \
                        -label "Y Position" -range {0.0 1000000.0 .1}]
            pack $yposLE -expand yes -fill x
            set sizeLE [LabelSpinBox $frame.sizeLE \
                        -textvariable [myvar options(-size)] \
                        -label "Size" -range {1  1000 1}]
            pack $sizeLE -expand yes -fill x
            set fontLE [LabelComboBox $frame.fontLE \
                        -textvariable [myvar options(-font)] \
                        -label "Font" -values [::FontName cget -values] \
                        -editable no]
            pack $fontLE -expand yes -fill x
            set textLE [LabelEntry $frame.textLE \
                        -textvariable [myvar options(-text)] \
                        -label "Text"]
            pack $textLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            install attributes using ::AttributesBox $frame.attributes 
            pack $attributes -expand yes -fill both
            $self configurelist $args
            $attributes resetGUI
        }
        method _Add {} {
            $hull withdraw
            $attributes updateAttrs
            return [$hull enddialog [$self _getallopts]]
        }
        method _getallopts {} {
            set result [list]
            foreach o {-xpos -ypos -size -font -text -color -attributes} {
                lappend result $o [$self cget $o]
            }
            return $result
        }
        method _Cancel {} {
            $hull withdraw
            return [$hull enddialog {}]
        }
        method draw {args} {
            $self configurelist $args
            $attributes resetGUI
            return [$hull draw]
        }
    }
}

snit::widgetadaptor BreadboardEditor {
    CommonEditorFunctions
    
    constructor {args} {
        installhull using FritzingPartsEditor -parent $win -bg white \
              -contextmenu [mymethod _canvasContextMenu] \
              -edititems   [mymethod _editItems] \
              -deleteitems [mymethod _deleteItems]
        $self CommonInit Breadboard
        $self configurelist $args
        $hull makeVpRect
        $self _setClean
    }
    method addpin {} {
        incr _pinno
        set result [$addpindialog draw -title {Add new pin} -pinno $_pinno]
        if {[llength $result] == 0} {return}
        #puts stderr "*** $self addpin: result is $result"
        array set opts $result
        set _pinno $opts(-pinno)
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "pinno=$opts(-pinno)"
        lappend tags "group=breadboard"
        lappend tags "type=pin"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        #puts stderr "*** $self addpin: coords $x1 $y1 $x2 $y2"
        set fill $opts(-color)
        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {}
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editPin $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid pin %X %Y]
        $self _setDirty
    }
    method _editPin {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x1 y1 x2 y2
        #puts stderr "*** $self _editPin: coords $x1 $y1 $x2 $y2"
        set xpos [expr {($x1+$x2)/2.0}]
        set ypos [expr {($y1+$y2)/2.0}]
        set diameter [expr {($x2-$x1)}]
        set opts(-xpos) $xpos
        set opts(-ypos) $ypos
        set opts(-diameter) $diameter
        set radius [expr {$diameter / 2.0}]
        set opts(-color) [$hull itemcget $tag -fill]
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags opts(-pinno)]
        set opts(-attributes) $attrs
        #puts stderr "*** $self _editPin: opts are [array get opts]"
        set result [eval [list $addpindialog draw -title {Edit pin}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        #puts stderr "*** $self _editPin: result is $result"
        array set opts $result
        set _pinno $opts(-pinno)
        set tags [gettagsfromattrs $opts(-attributes)]                       
        lappend tags "gid=$gid"
        lappend tags "pinno=$opts(-pinno)"
        lappend tags "group=breadboard"
        lappend tags "type=pin"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        #puts stderr "*** $self _editPin: coords $x1 $y1 $x2 $y2"
        set fill $opts(-color)
        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {}
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editPin $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid pin %X %Y]
        $self _setDirty
    }
    method addrect {} {
        set result [$addrectdialog draw -title {Add new rectangle}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=rect"
        set x1 $opts(-xpos)
        set y1 $opts(-ypos)
        set x2 [expr {$x1 + $opts(-width)}]
        set y2 [expr {$y1 + $opts(-height)}]
        set outline $opts(-color)
        set fill $outline
        set width $opts(-linethickness)
        if ($opts(-filled)) {
            $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {} 
        } else {
            $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill {} -outline $outline -width $width
        }
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editRect $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid rect %X %Y]
        $self _setDirty
    }
    method _editRect {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x1 y1 x2 y2
        set opts(-xpos) $x1
        set opts(-ypos) $y1
        set opts(-width) [expr {$x2-$x1}]
        set opts(-height) [expr {$y2-$y1}]
        set outline [$hull itemcget $tag -outline]
        set fill    [$hull itemcget $tag -fill]
        if {$fill eq ""} {
            set opts(-color) $outline
            set opts(-linethickness) [$hull itemcget $tag -width]
            set opts(-filled) false
        } else {
            set opts(-color) $fill
            set opts(-filled) true
        }
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set rectpts(-attributes) $attrs
        set result [eval [list $addrectdialog draw -title {Edit rectangle}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$gid"
        lappend tags "group=breadboard"
        lappend tags "type=rect"
        set x1 $opts(-xpos)
        set y1 $opts(-ypos)
        set x2 [expr {$x1 + $opts(-width)}]
        set y2 [expr {$y1 + $opts(-height)}]
        set outline $opts(-color)
        set width $opts(-linethickness)
        set outline $opts(-color)
        set fill $outline
        set width $opts(-linethickness)
        if ($opts(-filled)) {
            $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {} 
        } else {
            $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill {} -outline $outline -width $width
        }
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editRect $_gid]
        #$hull bind "gid=$_gid" <Shift-KeyPress-e> [mymethod _editRect $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid rect %X %Y]
        $self _setDirty
    }
    method addline {} {
        set result [$addlinedialog draw -title {Add new line}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=line"
        set x1 $opts(-x1)
        set y1 $opts(-y1)
        set x2 $opts(-x2)
        set y2 $opts(-y2)
        set fill $opts(-color)
        set width $opts(-linethickness)
        $hull create line $x1 $y1 $x2 $y2 -tags $tags -fill $fill -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editLine $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid line %X %Y]
        $self _setDirty
    }
    method _editLine {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x1 y1 x2 y2
        set opts(-x1) $x1
        set opts(-y1) $y1
        set opts(-x2) $x2
        set opts(-y2) $y2
        set opts(-linethickness) [$hull itemcget $tag -width]
        set opts(-color) [$hull itemcget $tag -fill]
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set opts(-attributes) $attrs
        set result [eval [list $addlinedialog draw -title {Edit line}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=line"
        set x1 $opts(-x1)
        set y1 $opts(-y1)
        set x2 $opts(-x2)
        set y2 $opts(-y2)
        set outline $opts(-color)
        set width $opts(-linethickness)
        $hull create line $x1 $y1 $x2 $y2 -tags $tags -fill $fill -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editLine $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid line %X %Y]
        $self _setDirty
    }
    method addcirc {} {
        set result [$addcircdialog draw -title {Add new circle}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=circ"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        if {$opts(-center) == 0} {
            set fill $opts(-color)
            set outline {}
            set width 0
        } else {
            set dradius [expr {$opts(-center)/2.0}]
            set fill {}
            set outline $opts(-color)
            set width [expr {$radius - $dradius}]
        }
        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline $outline -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editCirc $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid circ %X %Y]
        $self _setDirty
    }
    method _editCirc {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x1 y1 x2 y2
        set xpos [expr {($x1+$x2)/2.0}]
        
        set ypos [expr {($y1+$y2)/2.0}]
        set diameter [expr {($x2-$x1)}]
        set opts(-xpos) $xpos
        set opts(-ypos) $ypos
        set opts(-diameter) $diameter
        set radius [expr {$diameter / 2.0}]
        set fill [$hull itemcget $tag -fill]
        set outline [$hull itemcget $tag -outline]
        set width [$hull itemcget $tag -width]
        if {$fill ne ""} {
            set opts(-center) 0
            set opts(-color) $fill
        } else {
            set dradius [expr {$radius - $width}]
            set opts(-center) [expr {$dradius * 2.0}]
            set opts(-color) $outline
        }
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set opts(-attributes) $attrs
        set result [eval [list $addcircdialog draw -title {Edit circle}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        set tags [gettagsfromattrs $opts(-attributes)]                       
        lappend tags "gid=$gid"
        lappend tags "group=breadboard"
        lappend tags "type=circ"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        if {$opts(-center) == 0} {
            set fill $opts(-color)
            set outline {}
            set width 0
        } else {
            set dradius [expr {$opts(-center)/2.0}]
            set fill {}
            set outline $opts(-color)
            set width [expr {$radius - $dradius}]
        }
        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline $outline -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editCirc $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid circ %X %Y]
        $self _setDirty
    }
    method addarc {} {
        set result [$addarcdialog draw -title {Add new arc}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=arc"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        if {$opts(-center) == 0} {
            set fill $opts(-color)
            set outline {}
            set width 0
            set arcstyle pieslice
        } else {
            set dradius [expr {$opts(-center)/2.0}]
            set fill {}
            set outline $opts(-color)
            set width [expr {$radius - $dradius}]
            set arcstyle arc
        }
        
        $hull create arc $x1 $y1 $x2 $y2 -style $arcstyle -start $opts(-start) -extent $opts(-extent) -tags $tags -fill $fill -outline $outline -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editArc $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid arc %X %Y]
        $self _setDirty
    }
    method _editArc {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x1 y1 x2 y2
        set xpos [expr {($x1+$x2)/2.0}]
        set ypos [expr {($y1+$y2)/2.0}]
        set diameter [expr {($x2-$x1)}]
        set opts(-xpos) $xpos
        set opts(-ypos) $ypos
        set opts(-diameter) $diameter
        set radius [expr {$diameter / 2.0}]
        set fill [$hull itemcget $tag -fill]
        set outline [$hull itemcget $tag -outline]
        set width [$hull itemcget $tag -width]
        if {$fill ne ""} {
            set opts(-center) 0
            set opts(-color) $fill
        } else {
            set dradius [expr {$radius - $width}]
            set opts(-center) [expr {$dradius * 2.0}]
            set opts(-color) $outline
        }
        set opts(-start) [$hull itemcget $tag -start]
        set opts(-extent) $hull itemcget $tag -extent]
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set opts(-attributes) $attrs
        set result [eval [list $addarcdialog draw -title {Edit arc}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        set tags [gettagsfromattrs $opts(-attributes)]                       
        lappend tags "gid=$gid"
        lappend tags "group=breadboard"
        lappend tags "type=arc"
        set radius [expr {$opts(-diameter)/2.0}]
        set x1 [expr {$opts(-xpos) - $radius}]
        set x2 [expr {$opts(-xpos) + $radius}]
        set y1 [expr {$opts(-ypos) - $radius}]
        set y2 [expr {$opts(-ypos) + $radius}]
        if {$opts(-center) == 0} {
            set fill $opts(-color)
            set outline {}
            set width 0
            set arcstyle pieslice
        } else {
            set dradius [expr {$opts(-center)/2.0}]
            set fill {}
            set outline $opts(-color)
            set width [expr {$radius - $dradius}]
            set arcstyle arc
        }
        $hull create arc $x1 $y1 $x2 $y2 -style $arcstyle -start $opts(-start) -extent $ots(-extent) -tags $tags -fill $fill -outline $outline -width $width
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editArc $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid arc %X %Y]
        $self _setDirty
    }
    method addtext {} {
        set result [$addtextdialog draw -title {Add new text}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=text"
        set x $opts(-xpos)
        set y $opts(-ypos)
        set fill $opts(-color)
        set font [FontMapping MapToTk $opts(-font) $opts(-size)]
        $hull create text $x $y -text $opts(-text) -font $font -tags $tags -fill $fill -anchor sw
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editText $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid text %X %Y]
        $self _setDirty
    }
    method _editText {gid} {
        set tag "gid=$gid"
        set coords [$hull coords $tag]
        lassign $coords x y
        set opts(-xpos) $x
        set opts(-ypos) $y
        set opts(-text) [$hull itemcget $tag -text]
        set opts(-color) [$hull itemcget $tag -fill]
        FontMapping MapFromTk [$hull itemcget $tag -font] opts(-font) opts(-size)
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set opts(-attributes) $attrs
        set result [eval [list $addtextdialog draw -title {Edit text}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=breadboard"
        lappend tags "type=text"
        set x $opts(-xpos)
        set y $opts(-ypos)
        set fill $opts(-color)
        set font [FontMapping MapToTk $opts(-font) $opts(-size)]
        $hull create text $x $y -text $opts(-text) -font $font -tags $tags -fill $fill -anchor sw
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editText $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid text %X %Y]
        $self _setDirty
    }
    method read {filename} {
        if {[catch {open $filename r} fp]} {
            tk_messageBox -type ok -icon error -message [format {Could not open %s for reading: %s} $filename $fp]
            return
        }            
        set xml [ParseXML %AUTO% [read $fp]]
        close $fp
        set svg [$xml getElementsByTagName svg]
        $hull processSVGView [$svg attribute height] [$svg attribute width] [$svg attribute viewBox]
        set groups [$svg getElementsByTagName g -depth 1]
        set breadboardGroup {}
        foreach g $groups {
            if {[$g attribute id] eq "breadboard"} {
                set breadboardGroup $g
                break
            }
        }
        if {$breadboardGroup eq ""} {
            tk_messageBox -type ok -icon error -message "Breadboard group not found!"
            return
        }
        $self _processGroup $breadboardGroup [list "breadboard"] unrecognized
        if {[info exists unrecognized]} {
            # -- displayed unrecognized tags
            parray unrecognized
        }
        $self _setClean
    }
    method write {filename} {
        if {[catch {open $filename w} fp]} {
            tk_messageBox -type ok -icon error -message [format {Could not open %s for writing: %s} $filename $fp]
            return
        }
        switch [$hull cget -units] {
            mm {set units mm}
            inch {set units in}
        }
        set vp [$hull cget -viewport]
        lassign $vp x1 y1 x2 y2
        set width [$hull cget -width]
        set height [$hull cget -height]
        set emptySVG [format $emptySVGFormat $units $units $width $units \
                      $height $units $x1 $y1 $x2 $y2]

        set newxml [ParseXML %AUTO% $emptySVG]
        set root [$newxml getElementsByTagName svg]
        set breadboardGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id breadboard]]
        $root addchild $breadboardGroup
        set items [$hull find withtag "group=breadboard"]
        foreach i $items {
            set tags [$hull itemcget $i -tags]
            catch {unset pinno}
            set attrs [getattrsfromtags $tags pinno]
            set gid [getgid $tags]
            lappend attrs fpe:gid $gid
            switch [$hull type $i] {
                oval {
                    # Pin or Circle
                    if {[info exists pinno]} {
                        # pins have pin numbers
                        lappend attrs id [format {connector%dpin} $pinno]
                    }
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    set xpos [expr {($x1+$x2)/2.0}]
                    set ypos [expr {($y1+$y2)/2.0}]
                    set radius [expr {($x2-$x1)/2.0}]
                    lappend attrs cx $xpos cy $ypos r $radius
                    set fill [$hull itemcget $i -fill]
                    set outline [$hull itemcget $i -outline]
                    set width [$hull itemcget $i -width]
                    if {$fill ne ""} {
                        lappend attrs fill $fill
                    } else {
                        lappend attrs fill none
                        lappend attrs stroke $outline
                        lappend attrs stroke-width $width
                    }
                    set ele [SimpleDOMElement create %AUTO% -tag circle -attributes $attrs]
                    $breadboardGroup addchild $ele
                }
                rectangle {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    lappend attrs x $x1 y $y1
                    lappend attrs width [expr {$x2-$x1}] \
                          height [expr {$y2-$y1}]
                    set fill [$hull itemcget $i -fill]
                    set outline [$hull itemcget $i -outline]
                    set width [$hull itemcget $i -width]
                    if {$fill ne ""} {
                        lappend attrs fill $fill
                    } else {
                        lappend attrs fill none stroke $outline stroke-width $width
                    }
                    set ele [SimpleDOMElement create %AUTO% -tag rect \
                             -attributes $attrs]
                    $breadboardGroup addchild $ele
                }
                line {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    lappend attrs x1 $x1 y1 $y1
                    lappend attrs x2 $x2 y2 $y2
                    set outline [$hull itemcget $i -outline]
                    set width [$hull itemcget $i -width]
                    lappend attrs stroke $outline stroke-width $width
                    set ele [SimpleDOMElement create %AUTO% -tag line \
                             -attributes $attrs]
                    $breadboardGroup addchild $ele
                }
                arc {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    set cx [expr {($x1+$x2)/2.0}]
                    set cy [expr {($y1+$y2)/2.0}]
                    set r  [expr {($x2-$x1)/2.0}]
                    set fill [$hull itemcget $i -fill]
                    set outline [$hull itemcget $i -outline]
                    set width [$hull itemcget $i -width]
                    if {$fill ne ""} {
                        lappend attrs fill $fill
                    } else {
                        lappend attrs fill none stroke $outline stroke-width $width
                    }
                    set start [_radians [$hull itemcget $i -start]]
                    set extent [_radians [$hull itemcget $i -extent]]
                    set startX [expr {($r*cos($start))+$cx}]
                    set startY [expr {($r*sin($start))+$cy}]
                    set extentX [expr {($r*cos($extent+$start))+$cx}]
                    set extentY [expr {($r*sin($start++$start))+$cy}]
                    set pathData [format {M %f,%f A %f,%f 0 0 1 %f,%f z} \
                                  $startX $startY $r $r $extentX $extentY]
                    lappend attrs d "$pathData"
                    set ele [SimpleDOMElement create %AUTO% -tag path \
                             -attributes $attrs]
                    $breadboardGroup addchild $ele
                }
                text {
                    set coords [$hull coords $i]
                    lassign $coords x y
                    lappend attrs x $x y $y
                    set fill [$hull itemcget $i -fill]
                    lappend attrs fill $fill
                    set font [$hull itemcget $i -font]
                    FontMapping MapFromTk $font fontname size
                    lappend attrs font-family "$fontname" font-size $size
                    set ele [SimpleDOMElement create %AUTO% -tag text \
                             -attributes $attrs]
                    $breadboardGroup addchild $ele
                    $ele setdata "[$hull itemcget $i -text]"
                }
            }
        }
        xmlheader $fp BreadboardEditor
        $newxml displayTree $fp
        close $fp
        $self _setClean
    }
    
}

package provide BreadboardEditor 1.0
