#*****************************************************************************
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 5 14:52:32 2019
#  Last Modified : <190908.1846>
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

## @page FritzingPartsSchematicEditor Schematic Editor
# The schematic editor implements six graphical elements: pins,
# rectangles, lines, circles, arcs, and text, using six tool buttons.
# Each of the tool buttons opens a dialog box where the user can enter the
# information about the graphical element.
# @section SchematicPin Pins
# The Schematic implements "pins" as short lines with a small filled circle
# at the end, meant to be used as a connection point.  The line can have an 
# optional larger outline circle (at the opposite end from the connection) to 
# indicate an inverted input or output.  Also pins on a schematic are labeled 
# with at least a pin number and optionally a pin name.  The schematic pin 
# dialog, shown below, asks for the position (this is the center of the small 
# filled circle at the end of the line), the orientation of the pin (this
# corresponds to the side of the part's (rectangular) body: left, right, top,
# bottom), whether the pin is inverted, the pin number, the (optional) pin
# name, the color, the line thickness, the size of the textual elements, and 
# the font to use for the textual elements.  
# @image latex SchematicPin.png "Schematic Pin Dialog" width=4in
# @image html  SchematicPinSmall.png
# The length would always be the distance from the part's (rectangular) body 
# to the center of the connection point, as shown here (both pins have the 
# same length):
# @image latex SchematicPinExample.png "Example pins: both oriented left, both the same length."
# @image html SchematicPinExample.png
# @section SchematicRect Rectangles
# Rectangles can only be an outline.  The schematic rectangle dialog box, 
# shown below, asks for the position (upper left corner), size (width and
# height), the line thickness, and color.  The  position and size are in 
# viewport coordinates. The default color is black.
# @image latex SchematicRect.png "Schematic Rectangle Dialog" width=4in
# @image html  SchematicRectSmall.png
# @section SchematicLine Lines
# The schematic line dialog box, shown below, asks for the endpoints of the
# line (X1, Y1, X2, Y2), the line thickness and the color. The endpoints are
# in viewport coordinates. The default color is black.
# @image latex SchematicLine.png "Schematic Line Dialog" width=4in
# @image html  SchematicLineSmall.png
# @section SchematicCirc Circles
# Schematic circles can be filled or hollow. The schematic circle dialog 
# box, shown below, asks for the position (center), the diameter, the center 
# size (hole diameter), and the color.  The position and the diameters are in
# viewport coordinates. The default color is black.
# @image latex SchematicCirc.png "Schematic Circle Dialog" width=4in
# @image html  SchematicCircSmall.png
# @section SchematicArc  Arcs
# Schematic arcs can be either pie slices or bare arcs.  If the center 
# diameter, the arc will be a pie slice (filled), otherwise, the arc will be a
# bare arc. The schematic arc dialog box, shown below, asks for the position 
# (center), the diameter, the center size (hole diameter), and the color.  The
# position  and the diameters are in viewport coordinates. The default color 
# is black.
# @image latex SchematicArc.png "Schematic Arc Dialog" width=4in
# @image html  SchematicArcSmall.png
# @section SchematicText Text
# The schematic text dialog box, shown below, asks for the position (lower 
# left), the size in points, the font, the text, and the color. The position 
# is in viewport coordinates. The default color is black.
# @image latex SchematicText.png "Schematic Text Dialog" width=4in
# @image html  SchematicTextSmall.png



package require Tk
package require tile
package require FritzingPartsEditor
package require Dialog
package require LabelFrames
package require ParseXML
package require Version

namespace eval Schematic {
    
    snit::enum PinOrient -values {left right top bottom}
    
    snit::widgetadaptor AddPinDialog {
        option -xpos -default 0.0 -type snit::double
        option -ypos -default 0.0 -type snit::double
        option -orientation -default left -type Schematic::PinOrient
        option -length -default 1.0 -type snit::double
        option -inverted -default false -type snit::boolean
        option -pinno -default 1 -type snit::integer
        option -pinname -default ""
        option -font -default DroidSans -type ::FontName
        option -size -default 1  -type snit::integer
        option -color -default black -type ::Color
        option -linethickness -default 1.0 -type snit::double
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
            set orientationLE [LabelComboBox $frame.orientationLE \
                               -textvariable [myvar options(-orientation)] \
                               -label "Orientation" \
                               -values [Schematic::PinOrient cget -values] \
                               -editable no]
            pack $orientationLE -expand yes -fill x
            set lengthLE [LabelSpinBox $frame.lengthLE \
                          -textvariable [myvar options(-length)] \
                          -label "Length" -range {0.0 10000.0 .1}]
            pack $lengthLE -expand yes -fill x
            set invertedCK [ttk::checkbutton $frame.invertedCK \
                            -text "Inverted? " \
                            -offvalue false -onvalue true \
                            -variable [myvar options(-inverted)]]
            pack $invertedCK -expand yes -fill x
            set pinnoLE [LabelSpinBox $frame.pinnoLE -textvariable [myvar options(-pinno)] \
                         -label "Pin number" -range {1 1000 1}]
            pack $pinnoLE -expand yes -fill x
            set pinnameLE [LabelEntry $frame.pinnameLE \
                           -textvariable [myvar options(-pinname)] \
                           -label "Pin name"]
            pack $pinnameLE -expand yes -fill x
            set colorLE [LabelSelectColor $frame.colorLE \
                         -textvariable [myvar options(-color)] \
                         -label "Color"]
            pack $colorLE -expand yes -fill x
            set thickLE [LabelSpinBox $frame.thickLE -textvariable [myvar options(-linethickness)] \
                         -label "Line Thickness" -range {1.0 100.0 .1}]
            pack $thickLE -expand yes -fill x
            set sizeLE [LabelSpinBox $frame.sizeLE \
                        -textvariable [myvar options(-size)] \
                        -label "Label and Number Size" -range {1  1000 1}]
            pack $sizeLE -expand yes -fill x
            set fontLE [LabelComboBox $frame.fontLE \
                        -textvariable [myvar options(-font)] \
                        -label "Label and Number Font" \
                        -values [::FontName cget -values] \
                        -editable no]
            pack $fontLE -expand yes -fill x
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
            foreach o {-xpos -ypos -orientation -length -inverted -pinno -pinname -color -linethickness -font -size -attributes} {
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
            foreach o {-xpos -ypos -width -height -linethickness -color -attributes} {
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


snit::widgetadaptor SchematicEditor {
    CommonEditorFunctions
    
    constructor {args} {
        installhull using FritzingPartsEditor -parent $win -bg white \
              -contextmenu [mymethod _canvasContextMenu] \
              -edititems   [mymethod _editItems] \
              -deleteitems [mymethod _deleteItems]
        $self CommonInit Schematic
        $self configurelist $args
        $hull makeVpRect
        $self _setClean
    }
    method addpin_1 {result} {
        array set opts $result
        set _pinno $opts(-pinno)
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "type=pin"
        lappend tags "group=schematic"
        lappend tags  $opts(-pinno)
        set connectiontags $tags
        lappend connectiontags "pinno=$_pinno" "group=pin_connections"
        set pintags   $tags
        lappend pintags "group=pins"
        set pinlabeltags $tags
        lappend pinlabeltags "group=pin_labels"
        set pinnotags $tags
        lappend pinnotags    "group=pin_numbers"
        set orientation $opts(-orientation)
        set length      $opts(-length)
        set cr          [expr {.1*$length}]
        set inverted    $opts(-inverted)
        if {$inverted} {
            set ir [expr {.2*$length}]
        }
        lappend pintags   "orientation:$orientation" \
                         "length:$length" "inverted:$inverted"
        set color       $opts(-color)
        set width       $opts(-linethickness)
        set x $opts(-xpos)
        set y $opts(-ypos)
        set font [FontMapping MapToTk $opts(-font) $opts(-size)]
        
        $hull create oval [expr {$x-$cr}] [expr {$y-$cr}] \
              [expr {$x+$cr}] [expr {$y+$cr}] -fill $color -outline {} \
              -tags $connectiontags
        switch $orientation {
            left {
                set px2 [expr {$x + $length}]
                set py2 $y
                set lx  [expr {$px2 + ($width * 2)}]
                set ly  $py2
                set la  w
                set nx  [expr {$x + ($length / 2.0)}]
                set ny  [expr {$y - ($width * 2)}]
                set na  s
                if {$inverted} {
                    set px2 [expr {$px2 - ($ir*2)}]
                    set ix [expr {$px2 + $ir}]
                    set iy $y
                }
            }
            right {
                set px2 [expr {$x-$length}]
                set py2 $y
                set lx  $px2
                set ly  [expr {$py2 - ($width * 2)}]
                set la  e
                set nx  [expr {$x - ($length / 2.0)}]
                set ny  [expr {$y - ($width * 2)}]
                set na  s
                if {$inverted} {
                    set px2 [expr {$px2 + ($ir*2)}]
                    set ix [expr {$px2 - $ir}]
                    set iy $y
                }
            }
            top {
                set px2 $x
                set py2 [expr {$y + $length}]
                set lx  $px2
                set ly  $py2
                set la  n
                set nx  [expr {$x + ($width * 2)}]
                set ny  [expr {$y + ($length / 2.0)}]
                set na  w
                if {$inverted} {
                    set py2 [expr {$py2 - ($ir*2)}]
                    set ix $x
                    set iy [expr {$py2 + $ir}]
                }
            }
            bottom {
                set px2 $x
                set py2 [expr {$y-$length}]
                set lx  $px2
                set ly  $py2
                set la  s
                set nx  [expr {$x + ($width * 2)}]
                set ny  [expr {$y - ($length / 2.0)}]
                set na  w
                if {$inverted} {
                    set py2 [expr {$py2 + ($ir*2)}]
                    set ix $x
                    set iy [expr {$py2 - $ir}]
                }
            }
        }
        $hull create line $x $y $px2 $py2 -fill $color \
              -tags $pintags -width $width
        if {$inverted} {
            $hull create oval [expr {$ix-$ir}] [expr {$iy-$ir}] \
                  [expr {$ix+$ir}] [expr {$iy+$ir}] -fill {} -outline $color \
                  -tags $pintags
        }
        set label $opts(-pinname)
        if {$label ne ""} {
            set id [$hull create text $lx $ly -anchor $la -font $font \
                    -text $label -fill $color -tags $pinlabeltags]
            set l_coords [$hull bbox $id]
            lassign $l_coords x0 dummy dummy y0
            $hull delete $id
            $hull create text $x0 $y0 -anchor sw -font $font -text $label \
                  -fill $color -tags $pinlabeltags
        }
        set id [$hull create text $nx $ny -anchor $na -font $font \
                -text $_pinno -fill $color -tags $pinnotags]
        set n_coords [$hull bbox $id]
        lassign $n_coords x0 dummy dummy y0
        $hull delete $id
        $hull create text $x0 $y0 -anchor sw -font $font -text $_pinno \
              -fill $color -tags $pinnotags
        #$hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
        #$hull bind "gid=$_gid" <KeyPress-e> [mymethod _editPin $_gid]
        #$hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid pin %X %Y]
        $self _setDirty
    }
    method addpin {} {
        incr _pinno
        set result [$addpindialog draw -title {Add new pin} -pinno $_pinno]
        if {[llength $result] == 0} {return}
        #puts stderr "*** $self addpin: result is $result"
        $self addpin_1 $result
    }
    method _editPin {gid} {
        set tag "gid=$gid"
        set matchtagC "${tag}&&group=pin_connections"
        set matchtagO "${tag}&&group=pins"
        set matchtagN "${tag}&&group=pin_numbers"
        set matchtagL "${tag}&&group=pin_labels"
        set coords [$hull coords $matchtagC]
        lassign $coords x1 y1 x2 y2
        set opts(-xpos) [expr {($x1+$x2)/2.0}]
        set opts(-ypos) [expr {($y1+$y2)/2.0}]
        set tags [$hull itemcget $matchtagC -tags]
        set attrs [getattrsfromtags $tags opts(-pinno)]
        set opts(-attributes) $attrs
        set tags [$self getunionoftags $gid]
        foreach t $tags {
            regexp {^orientation:(.*)$} $t => opts(-orientation)
            regexp {^length:(.*)$} $t => opts(-length)
            regexp {^inverted:(.*)$} $t => opts(-inverted)
        }
        
        foreach i [$hull find withtag $matchtagO] {
            if {[$hull type $i] eq "line"} {
                set opts(-color) [$hull itemcget $i -fill]
            }
            if {[$hull type $i] eq "line"} {
                set opts(-linethickness) [$hull itemcget $i -width]
            }
        }
        FontMapping MapFromTk [$hull itemcget $matchtagN -font] opts(-font) opts(-size)
        set opts(-pinname) [$hull itemcget $matchtagL -text]
        set result [eval [list $addpindialog draw -title {Edit pin}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        $self addpin_1 $result        
    }
    method addrect {} {
        set result [$addrectdialog draw -title {Add new rectangle}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=schematic"
        lappend tags "type=rect"
        set x1 $opts(-xpos)
        set y1 $opts(-ypos)
        set x2 [expr {$x1 + $opts(-width)}]
        set y2 [expr {$y1 + $opts(-height)}]
        set outline $opts(-color)
        set width $opts(-linethickness)
        $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill {} -outline $outline -width $width
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
        set opts(-linethickness) [$hull itemcget $tag -width]
        set opts(-color) [$hull itemcget $tag -outline]
        set tags [$hull itemcget $tag -tags]
        set attrs [getattrsfromtags $tags]
        set rectpts(-attributes) $attrs
        set result [eval [list $addrectdialog draw -title {Edit rectangle}] [array get opts]]
        if {[llength $result] == 0} {return}
        $self _delete $gid
        array set opts $result
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$gid"
        lappend tags "group=schematic"
        lappend tags "type=rect"
        set x1 $opts(-xpos)
        set y1 $opts(-ypos)
        set x2 [expr {$x1 + $opts(-width)}]
        set y2 [expr {$y1 + $opts(-height)}]
        set outline $opts(-color)
        set width $opts(-linethickness)
        $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill {} -outline $outline -width $width
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
        lappend tags "group=schematic"
        lappend tags "type=line"
        set x1 $opts(-x1)
        set y1 $opts(-y1)
        set x2 $opts(-x2)
        set y2 $opts(-y2)
        set outline $opts(-color)
        set width $opts(-linethickness)
        $hull create line $x1 $y1 $x2 $y2 -tags $tags -fill $outline -width $width
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
        lappend tags "group=schematic"
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
    method addcirc {} {
        set result [$addcircdialog draw -title {Add new circle}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=schematic"
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
        lappend tags "group=schematic"
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
        lappend tags "group=schematic"
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
        lappend tags "group=schematic"
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
    method addtext {} {
        set result [$addtextdialog draw -title {Add new text}]
        if {[llength $result] == 0} {return}
        array set opts $result
        incr _gid
        set tags [gettagsfromattrs $opts(-attributes)]
        lappend tags "gid=$_gid"
        lappend tags "group=schematic"
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
        lappend tags "group=schematic"
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
        set schematicGroup {}
        foreach g $groups {
            if {[$g attribute id] eq "schematic"} {
                set schematicGroup $g
                break
            }
        }
        if {$schematicGroup eq ""} {
            tk_messageBox -type ok -icon error -message "Schematic group not found!"
            return
        }
        $self _processGroup $schematicGroup [list "schematic"] unrecognized
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
        set schematicGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id schematic]]
        $root addchild $schematicGroup
        set pinsGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id pins]]
        $schematicGroup addchild $pinsGroup
        set connectionsGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id pin_connections]]
        $pinsGroup addchild $connectionsGroup
        set labelsGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id pin_labels]]
        $pinsGroup addchild $labelsGroup
        set numbersGroup [SimpleDOMElement create %AUTO% -tag g -attributes [list id pin_numbers]]
        $pinsGroup addchild $numbersGroup
        set items [$hull find withtag "group=schematic"]
        foreach i $items {
            set tags [$hull itemcget $i -tags]
            catch {unset pinno}
            set attrs [getattrsfromtags $tags pinno]
            set groups [getgroups $tags]
            set gid [getgid $tags]
            lappend attrs fpe:gid $gid
            switch [$hull type $i] {
                oval {
                    # Circle
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
                    #puts stderr "*** $self write: getting outline for oval $gid"
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
                    if {"pin_connections" in $groups} {
                        $connectionsGroup addchild $ele
                    } elseif {"pins" in $groups} {
                        $pinsGroup addchild $ele
                    } else {
                        $schematicGroup addchild $ele
                    }
                }
                rectangle {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    lappend attrs x $x1 y $y1
                    lappend attrs width [expr {$x2-$x1}] \
                          height [expr {$y2-$y1}]
                    set fill [$hull itemcget $i -fill]
                    #puts stderr "*** $self write: getting outline for rectangle $gid"
                    set outline [$hull itemcget $i -outline]
                    set width [$hull itemcget $i -width]
                    if {$fill ne ""} {
                        lappend attrs fill $fill
                    } else {
                        lappend attrs fill none stroke $outline stroke-width $width
                    }
                    set ele [SimpleDOMElement create %AUTO% -tag rect \
                             -attributes $attrs]
                    $schematicGroup addchild $ele
                }
                line {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    lappend attrs x1 $x1 y1 $y1
                    lappend attrs x2 $x2 y2 $y2
                    #puts stderr "*** $self write: getting outline for line $gid"
                    set fill [$hull itemcget $i -fill]
                    set width [$hull itemcget $i -width]
                    lappend attrs stroke $fill stroke-width $width
                    foreach t $tags {
                        if {[regexp {^orientation:(.*)$} $t => orientation] > 0} {
                            lappend attrs fpe:orientation $orientation
                        } elseif {[regexp {^length:([[:digit:].]+)$} $t => length] > 0} {
                            lappend attrs fpe:length $length
                        } elseif {[regexp {^inverted:(.*)$} $t => inverted] > 0} {
                            lappend attrs fpe:inverted $inverted
                        }
                    }
                    set ele [SimpleDOMElement create %AUTO% -tag line \
                             -attributes $attrs]
                    if {"pins" in $groups} {
                        $pinsGroup addchild $ele
                    } else {
                        $schematicGroup addchild $ele
                    }
                }
                arc {
                    set coords [$hull coords $i]
                    lassign $coords x1 y1 x2 y2
                    set cx [expr {($x1+$x2)/2.0}]
                    set cy [expr {($y1+$y2)/2.0}]
                    set r  [expr {($x2-$x1)/2.0}]
                    set fill [$hull itemcget $i -fill]
                    #puts stderr "*** $self write: getting outline for arc $gid"
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
                    $schematicGroup addchild $ele
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
                    if {"pin_labels" in $groups} {
                        $labelsGroup addchild $ele
                    } elseif {"pin_numbers" in $groups} {
                        $numbersGroup addchild $ele
                    } elseif {"pins" in $groups} {
                        $pinsGroup addchild $ele
                    } else {
                        $schematicGroup addchild $ele
                    }
                    $ele setdata "[$hull itemcget $i -text]"
                }
            }
        }
        xmlheader $fp SchematicEditor
        $newxml displayTree $fp
        close $fp
        $self _setClean
    }
}




package provide SchematicEditor 1.0
