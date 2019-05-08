#*****************************************************************************
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sun May 5 15:59:34 2019
#  Last Modified : <190508.1318>
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

## @page FritzingPartsEditor Common GUI elements
# TBD


package require Tk
package require tile
package require ScrollWindow
package require ButtonBox

snit::type Color {
    pragma -hastypeinfo    no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typemethod validate {value} {
        if {[catch {winfo rgb . $value} err]} {
            return -code error -errorcode INVALID $err
        } else {
            return $value
        }
    }
}

snit::enum FontName -values {DroidSans DroidSans-Bold DroidSansMono OCRA}

snit::type FontMapping {
    pragma -hastypeinfo    no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typevariable _mappingToTk -array {}
    typevariable _mappingFromTk -array {}
    typevariable _familyMapToTk -array {}
    typevariable _familyMapFromTk -array {}
    
    typeconstructor {
        foreach f [FontName cget -values] {
            switch $f {
                DroidSans {
                    set font [font create -family Helvetica]
                    set tk_key [list [font actual $font -family] [font actual $font -weight]]
                    set _familyMapToTk(DroidSans) $tk_key
                    set _familyMapFromTk($tk_key) DroidSans
                }
                DroidSans-Bold {
                    set font [font create -family Helvetica -weight bold]
                    set tk_key [list [font actual $font -family] [font actual $font -weight]]
                    set _familyMapToTk(DroidSans-Bold) $tk_key 
                    set _familyMapFromTk($tk_key) DroidSans-Bold
                }
                DroidSansMono {
                    set font [font create -family Courier]
                    set tk_key [list [font actual $font -family] [font actual $font -weight]]
                    set _familyMapToTk(DroidSansMono) $tk_key
                    set _familyMapFromTk($tk_key) DroidSansMono
                }
                OCRA {
                    set font [font create -family Roman]
                    set tk_key [list [font actual $font -family] [font actual $font -weight]]
                    set _familyMapToTk(OCRA) $tk_key
                    set _familyMapFromTk($tk_key) OCRA
                }
            }
        }
    }
    typemethod MapToTk {fontname size} {
        FontName validate $fontname
        if {[info exists _mappingToTk([list $fontname $size])]} {
            return $_mappingToTk([list $fontname $size])
        }
        set tk_font $_familyMapToTk($fontname)
        lassign $tk_font family weight
        set font [font create -family $family -weight $weight -size $size]
        set _mappingToTk([list $fontname $size]) $font
        set _mappingFromTk($font) [list $fontname $size]
        return $font
    }
    typemethod MapFromTk {font namevar sizevar} {
        upvar $namevar fontname
        upvar $sizevar size
        if {[info exists _mappingFromTk($font)]} {
            lassign $_mappingFromTk($font) fontname size
        }
    }
}


snit::double Angle -min -360.0 -max 360.0



snit::type AttributeList {
    pragma -hastypeinfo    no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typemethod validate {v} {
        if {([llength $v] % 2) != 0} {
            return -code error -errorcode INVALID  "Invalid AttributeList in \{$v\}, should be an even element list {key1 value1 key2 value2 ... keyN valueN}"
        } else {
            return $v
        }
    }
}
snit::widget AttributesBox {
    hulltype ttk::frame
    variable _attrs -array {}
    variable _index 0
    method _buildOne {key val} {
        incr _index
        set _attrs($key) $val
        set frame [LabelFrame $win.attr$_index -text $key]
        pack $frame -expand yes -fill x
        set valentry [ttk::entry $frame.value \
                      -textvariable [myvar _attrs($key)]]
        pack $valentry -side left -expand yes -fill x
        set delbutton [ttk::button $frame.del -text "Delete" \
                       -command [mymethod _delete $_index]]
        pack $delbutton -side right
    }
    option -values -default {} -type AttributeList 
    component addkv
    variable _newkey {}
    variable _newval {}
    
    constructor {args} {
        $self configurelist $args
        foreach {k v} $options(-values) {
            $self _buildOne $k $v
        }
        install addkv using LabelFrame $win.addkv -text "New key"
        pack $addkv -side bottom -expand yes -fill x
        set keyentry [ttk::entry $addkv.keyentry -textvariable [myvar _newkey]]
        pack $keyentry -side left -expand yes -fill x
        pack [ttk::label $addkv.space -text " "] -side left
        set valentry [ttk::entry $addkv.value -textvariable [myvar _newval]]
        pack $valentry -side left -expand yes -fill x
        set addbutton [ttk::button $addkv.addbutton -text "Add" -command [mymethod _add]]
        pack $addbutton -side right
    }
    method updateAttrs {} {
        set options(-values) [array get _attrs]
    }
    method resetGUI {args} {
        foreach c [winfo children $win] {
            if {[regexp {attr([[:digit:]]+)$} $c => index] > 0} {
                $self _delete $index
            }
        }
        $self configurelist $args
        foreach {k v} $options(-values) {
            $self _buildOne $k $v
        }
    }
    method _delete {index} {
        set frame $win.attr${index}
        if {[winfo exists $frame]} {
            set key [$frame cget -text]
            unset _attrs($key)
            destroy $frame
            set height [winfo height $win]
            set reqheight [winfo reqheight $win]
            #puts stderr "*** $self _add: height is $height"
            #puts stderr "*** $self _add: reqheight is $reqheight"
            if {$reqheight < $height} {
                set toplevel [winfo toplevel $win]
                set toplevelH [winfo height $toplevel]
                set toplevelRH [winfo reqheight $toplevel]
                #puts stderr "*** $self _configure: toplevel is $toplevel"
                #puts stderr "*** $self _configure: toplevelH is $toplevelH"
                #puts stderr "*** $self _configure: toplevelRH is $toplevelRH"
                set g [wm geometry $toplevel]
                regexp {^([[:digit:]]+)x([[:digit:]]+)\+([[:digit:]]+)\+([[:digit:]]+)$} $g =>  \
                      gwidth gheight gx gy
                #puts stderr "*** $self _configure: gheight = $gheight"
                set hincr [expr {$reqheight-$height}]
                #puts stderr "*** $self _configure: hincr = $hincr"
                incr gheight $hincr
                #puts stderr "*** $self _configure: (after incr) gheight = $gheight"
                wm geometry $toplevel ${gwidth}x${gheight}+${gx}+${gy}
            }
        }
    }
    method _add {} {
        if {$_newkey eq ""} {return}
        $self _buildOne $_newkey $_newval
        update idle
        set _newkey {}
        set _newval {}
        set height [winfo height $win]
        set reqheight [winfo reqheight $win]
        #puts stderr "*** $self _add: height is $height"
        #puts stderr "*** $self _add: reqheight is $reqheight"
        if {$reqheight > $height} {
            set toplevel [winfo toplevel $win]
            set toplevelH [winfo height $toplevel]
            set toplevelRH [winfo reqheight $toplevel]
            #puts stderr "*** $self _configure: toplevel is $toplevel"
            #puts stderr "*** $self _configure: toplevelH is $toplevelH"
            #puts stderr "*** $self _configure: toplevelRH is $toplevelRH"
            set g [wm geometry $toplevel]
            regexp {^([[:digit:]]+)x([[:digit:]]+)\+([[:digit:]]+)\+([[:digit:]]+)$} $g =>  \
                  gwidth gheight gx gy
            #puts stderr "*** $self _configure: gheight = $gheight"
            set hincr [expr {$reqheight-$height}]
            #puts stderr "*** $self _configure: hincr = $hincr"
            incr gheight $hincr
            #puts stderr "*** $self _configure: (after incr) gheight = $gheight"
            wm geometry $toplevel ${gwidth}x${gheight}+${gx}+${gy}
        }
    }
}

snit::macro CommonEditorFunctions {} {
    variable _gid 0
    variable _pinno 0
    variable _isdirty no
    
    typevariable PI [expr {asin(1.0)*2.0}]
    proc _radians {degrees} {
        return [expr {($degrees/180.0)*$PI}]
    }
    proc _degrees {radians} {
        return [expr {($radians/$PI)*180.0}]
    }
    proc _square {x} {
        return [expr {$x * $x}]
    }
    proc _findCenter {x1 y1 x2 y2 radius cxvar cyvar startvar extentvar} {
        upvar $cxvar xc
        upvar $cyvar yc
        upvar $startvar a1
        upvar $extentvar a2
        
        set a $x1
        set b $y1
        set c $x2
        set d $y2
        set r $radius
        
        set J [expr {2.0*($a-$c)}]
        set G [expr {2.0*($b-$d)}]
        set T [expr {double([_square $a]+[_square $b]) - double([_square $c]+[_square $d])}]
            
        set u [expr {(1.0 + ([_square $J] / [_square $G]))}]
        set v [expr {(-2.0*$a) - ((2.0*$J*$T)/[_square $G]) + ((2.0*$J*$b)/$G)}]
        set w [expr {[_square $a]+[_square $b] + [_square $T]/[_square $G] - 2*$b*$T/$G - [_square $r]}]
        
        set sqrt [expr {sqrt([_square $v]-4.0*$u*$w)}]
        
        set m1 [expr {(-$v + $sqrt)/(2.0*$u)}]
        set n1 [expr {($T-$J*$m1)/$G}]
        
        set m2 [expr {(-$v - $sqrt)/(2.0*$u)}]
        set n2 [expr {($T-$J*$m2)/$G}]
        
        set at1 [expr atan2($c-$m1,$d-$n1)]
        set at2 [expr atan2($a-$m1,$b-$n1)]
        
        
        
        #  set a11 [RadiansToDegrees [expr -atan2($y1-$n1,$x1-$m1)]]
        #  set a12 [RadiansToDegrees [expr -atan2($y1-$n2,$x1-$m2)]]
        #  set a21 [RadiansToDegrees [expr -atan2($y2-$n1,$x2-$m1)]]
        #  set a22 [RadiansToDegrees [expr -atan2($y2-$n2,$x2-$m2)]]
        #
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: a11 = $a11"
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: a12 = $a12"
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: a21 = $a21 ([expr $a21 - $a11])"
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: a22 = $a22 ([expr $a22 - $a12])"
        
        
        
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: at1 = $at1, at2 = $at2"
        set sn [expr sin($at1 - $at2)]
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: sn = $sn"
        
        if {$sn > 0} {
            set m $m1
            set n $n1
        } else {
            set m $m2
            set n $n2
        }
        
        set xc $m
        set yc $n
        
        set a1 [$type _RadiansToDegrees [expr {-atan2($y1-$yc,$x1-$xc)}]]
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: a1 = $a1"
        set a2 [$type _RadiansToDegrees [expr {-atan2($y2-$yc,$x2-$xc)}]]
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: (1) a2 = $a2 ([expr $a2 - $a1])"
        if {$a2 < 0} {set a2 [expr $a2 + 360]}
        #  puts stderr "*** CTCPanel::CurvedBlock_Create: (2) a2 = $a2 ([expr $a2 - $a1])"
    }
                     
    option -dirtyhandler -default {} 
    option -dirtyhandlercontext -default {}
    delegate option -width to hull
    delegate option -height to hull
    delegate option -viewport to hull
    delegate option -units to hull
    component addpindialog
    component addrectdialog
    component addlinedialog
    component addcircdialog
    component addarcdialog
    component addtextdialog
    
    component itemcontextmenu
    variable _itemContext_gid
    variable _itemContext_itemtype
    component canvascontextmenu
    
    method _delete {gid} {
        $hull delete "gid=$gid"
    }
    proc gettagsfromattrs {attrs} {
        set tags [list]
        foreach {k v} $attrs {
            if {$k in {fpe:gid width height x y cx cy x1 x2 y1 y2 d r fill stroke stroke-width font-family font-size id}} {continue}
            lappend tags "${k}=${v}"
        }
        return $tags
    }
    proc getattrsfromtags {tags {pinnovar {}}} {
        set attrs [list]
        foreach t $tags {
            if {[regexp {^([^=]+)=(.*)$} $t => key val] > 0} {
                if {$pinnovar ne "" && $key eq "pinno"} {
                    upvar $pinnovar pinno
                    set pinno $val
                } elseif {$key ni {type gid group}} {
                    lappend attrs $key $val
                }
            }
        }
        return $attrs
    }
    proc getgid {tags} {
        foreach t $tags {
            if {[regexp {^fpe:gid=([[:digit:]]+)$} $t => gid] > 0} {return $gid}
        }
        return -1
    }
    proc getgroups {tags} {
        set result [list]
        foreach t $tags {
            if {[regexp {^group=(.+)$} $t => group] > 0} {
                lappend result $group
            }
        }
        return $result
    }
    method _itemContextMenu {gid itemtype X Y} {
        #puts stderr "*** $self _itemContextMenu $gid $itemtype"
        set _itemContext_gid $gid
        set _itemContext_itemtype $itemtype
        switch $itemtype {
            pin {
                set tags [$hull itemcget "gid=$gid" -tags]
                set pinno -1
                foreach t $tags {
                    if {[regexp {^pinno=([[:digit:]]+)$} $t => pinno] > 0} {
                        break
                    }
                }
                $itemcontextmenu configure -title [format {Pin %d} $pinno]
            }
            rect {
                $itemcontextmenu configure -title [format {Rectangle %d} $gid]
            }
            line {
                $itemcontextmenu configure -title [format {Line %d} $gid]
            }
            circ {
                $itemcontextmenu configure -title [format {Circle %d} $gid]
            }
            arc {
                $itemcontextmenu configure -title [format {Arc %d} $gid]
            }
            text {
                $itemcontextmenu configure -title [format {Text %d} $gid]
            }
        }
        $itemcontextmenu post $X $Y
    }
    method _canvasContextMenu {items X Y} {
        #puts stderr "*** $self _canvasContextMenu $items"
        $canvascontextmenu delete 0 end
        foreach i $items {
            set tags [$hull itemcget $i -tags]
            set gid -1
            set itemtype {}
            set pinno -1
            foreach t $tags {
                regexp {^gid=([[:digit:]]+)$} $t => gid
                regexp {^type=(.+)$} $t -> itemtype
                regexp {^pinno=([[:digit:]]+)$} $t => pinno
            }
            if {$gid < 0 || $itemtype eq {}} {continue}
            switch $itemtype {
                pin {
                    $canvascontextmenu add command \
                          -label [format {Pin %d} $pinno] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
                rect {
                    $canvascontextmenu add command \
                          -label [format {Rectangle %d} $gid] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
                line {
                    $canvascontextmenu add command \
                          -label [format {Line %d} $gid] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
                circ {
                    $canvascontextmenu add command \
                          -label [format {Circle %d} $gid] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
                arc {
                    $canvascontextmenu add command \
                          -label [format {Arc %d} $gid] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
                text {
                    $canvascontextmenu add command \
                          -label [format {Text %d} $gid] \
                          -command [mymethod _itemContextMenu $gid $itemtype $X $Y]
                }
            }
        }
        $canvascontextmenu add command -label Cancel \
              -command [mymethod _contextCancel $canvascontextmenu]
        $canvascontextmenu post  $X $Y
    }
    method _contextDelete {} {
        #$itemcontextmenu unpost;# Do I need this?
        $self _delete $_itemContext_gid
    }
    method _contextEdit {} {
        #puts stderr "*** $self _contextEdit"
        #$itemcontextmenu unpost;# Do I need this?
        #puts stderr "*** $self _contextEdit: _itemContext_itemtype is $_itemContext_itemtype"
        #puts stderr "*** $self _contextEdit: _itemContext_gid is $_itemContext_gid"
        switch "$_itemContext_itemtype" {
            pin {
                $self _editPin $_itemContext_gid
            }
            rect {
                $self _editRect $_itemContext_gid
            }
            line {
                $self _editLine $_itemContext_gid
            }
            circ {
                $self _editCirc $_itemContext_gid
            }
            arc {
                $self _editArc $_itemContext_gid 
            }
            text {
                $self _editText $_itemContext_gid
            }
        }
    }
    method _contextCancel {menu} {
        #$menu unpost;# Do I need this?
    }
    
    method _setDirty {args} {
        set _isdirty yes
        if {$options(-dirtyhandler) ne ""} {
            uplevel #0 $options(-dirtyhandler) $options(-dirtyhandlercontext) $_isdirty
        }
    }
    method _setClean {} {
        set _isdirty no
        if {$options(-dirtyhandler) ne ""} {
            uplevel #0 $options(-dirtyhandler) $options(-dirtyhandlercontext) $_isdirty
        }
    }
    
    method isDirty {} {return $_isdirty}
    
    typevariable emptySVGFormat {<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:fpe="http://www.deepsoft.com/fpe" x="0%s" y="0%s" width="%f%s" height="%f%s" viewBox="%f %f %f %f" xml:space="preserve" />}
    
    method clean {} {
        $hull delete !viewport
        set _gid 0
        set _pinno 0
        set _isdirty false
        if {$options(-dirtyhandler) ne ""} {
            uplevel #0 $options(-dirtyhandler) $options(-dirtyhandlercontext) $_isdirty
        }
    }
    proc xmlheader {fp generatorType} {
        puts $fp {<?xml version="1.0" encoding="utf-8"?>}
        if {0} {
        puts $fp {<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd" [
<!ATTLIST ":circle"
  xmlns:fpe CDATA #FIXED "http://www.deepsoft.com/fpe"
  fpe:gid CDATA #IMPLIED>
<!ATTLIST ":rect"
  xmlns:fpe CDATA #FIXED "http://www.deepsoft.com/fpe"
  fpe:gid CDATA #IMPLIED>
<!ATTLIST ":line"
  xmlns:fpe CDATA #FIXED "http://www.deepsoft.com/fpe"
  fpe:gid CDATA #IMPLIED>
<!ATTLIST ":text"
  xmlns:fpe CDATA #FIXED "http://www.deepsoft.com/fpe"
  fpe:gid CDATA #IMPLIED>
<!ATTLIST ":path"
  xmlns:fpe CDATA #FIXED "http://www.deepsoft.com/fpe"
  fpe:gid CDATA #IMPLIED>
  ]>}
}
        puts $fp "<!-- Generator: [file tail $::argv0] $Version::VERSION on $Version::target ($generatorType) -->"
    }
    method _processGroup {group groups unrecname} {
        upvar $unrecname unrecognized
        foreach c [$group children] {
            #puts stderr "*** $self read: c is $c, <[$c cget -tag]>"
            set gid [$c attribute fpe:gid]
            if {$gid ne ""} {
                if {$gid > $_gid} {
                    set _gid $gid
                }
            }
            switch [$c cget -tag] {
                g {
                    set childgroup [$c attribute id]
                    $self _processGroup $c [linsert $groups 0 $childgroup] unrecognized
                }
                rect {
                    if {$gid eq ""} {
                        incr _gid
                        set gid $_gid
                    }
                    set tags [gettagsfromattrs [$c cget -attributes]]
                    lappend tags "gid=$gid"
                    foreach g $groups {
                        lappend tags "group=$g"
                    }
                    lappend tags "type=rect"
                    set x1 [$c attribute x]
                    set y1 [$c attribute y]
                    set x2 [expr {$x1+[$c attribute width]}]
                    set y2 [expr {$y1+[$c attribute height]}]
                    set fill [$c attribute fill]
                    if {$fill eq "none"} {
                        set fill {}
                        set filled false
                    } else {
                        set filled true
                    }
                    set outline [$c attribute stroke]
                    set width [$c attribute stroke-width]
                    if {$filled} {
                        $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {} 
                    } else {
                        $hull create rectangle $x1 $y1 $x2 $y2 -tags $tags -fill {} -outline $outline -width $width
                    }
                    $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                    $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editRect $_gid]
                    $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid rect %X %Y]
                }
                circle {
                    if {$gid eq ""} {
                        incr _gid
                        set gid $_gid
                    }
                    set tags [gettagsfromattrs [$c cget -attributes]]
                    lappend tags "gid=$gid"
                    foreach g $groups {
                        lappend tags "group=$g"
                    }
                    set id [$c attribute id]
                    set ispin false
                    if {$id ne {}} {
                        if {[regexp {^connector([[:digit:]]+)pin$} $id => pinno] > 0} {
                            lappend tags "pinno=$pinno"
                            if {$pinno > $_pinno} {
                                set _pinno $pinno
                            }
                            set ispin true
                        }
                    }
                    if {$ispin} {
                        lappend tags "type=pin"
                    } else {
                        lappend tags "type=circ"
                    }
                    set xpos [$c attribute cx]
                    set ypos [$c attribute cy]
                    set radius [$c attribute r]
                    set fill [$c attribute fill]
                    set outline [$c attribute stroke]
                    set width [$c attribute strike-width]
                    if {$width eq {}} {set width 0}
                    set x1 [expr {$xpos-$radius}]
                    set y1 [expr {$ypos-$radius}]
                    set x2 [expr {$xpos+$radius}]
                    set y2 [expr {$ypos+$radius}]
                    if {$ispin} {
                        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline {}
                        $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                        $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editPin $_gid]
                        $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid pin %X %Y]
                    } else {
                        $hull create oval $x1 $y1 $x2 $y2 -tags $tags -fill $fill -outline $outline -width $width
                        $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                        $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editCirc $_gid]
                        $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid circ %X %Y]
                    }
                }
                line {
                    if {$gid eq ""} {
                        incr _gid
                        set gid $_gid
                    }
                    set tags [gettagsfromattrs [$c cget -attributes]]
                    lappend tags "gid=$gid"
                    foreach g $groups {
                        lappend tags "group=$g"
                    }
                    lappend tags "type=line"
                    set x1 [$c attribute x1]
                    set y1 [$c attribute y1]
                    set x2 [$c attribute x2]
                    set y2 [$c attribute y2]
                    set outline [$c attribute stroke]
                    set width [$c attribute stroke-width]
                    $hull create line $x1 $y1 $x2 $y2 -tags $tags -fill $outline -width $width
                    $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                    $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editLine $_gid]
                    $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid line %X %Y]
                }
                text {
                    if {$gid eq ""} {
                        incr _gid
                        set gid $_gid
                    }
                    set tags [gettagsfromattrs [$c cget -attributes]]
                    lappend tags "gid=$gid"
                    foreach g $groups {
                        lappend tags "group=$g"
                    }
                    lappend tags "type=text"
                    set x [$c attribute x]
                    set y [$c attribute y]
                    set fill [$c attribute fill]
                    set font [FontMapping MapToTk [$c attribute font-family] [$c attribute font-size]]
                    $hull create text $x $y -text [$c data] -font $font -tags $tags -fill $fill -anchor nw
                    $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                    $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editText $_gid]
                    $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid text %X %Y]
                }
                path {
                    set pathData [$c attribute d]
                    if {[regexp {^M[[:space:]]*([[:digit:].]+),([[:digit:].]+) A[[:space:]]*([[:digit:].]+),([[:digit:].]+) 0 0 1 ([[:digit:].]+),([[:digit:].]+) z$} => startX startY r1 r2 ententX extentY] > 0 &&
                        abs($r1-$r2) < .00001} {
                        if {$gid eq ""} {
                            incr _gid
                            set gid $_gid
                        }
                        set tags [gettagsfromattrs [$c cget -attributes]]
                        lappend tags "gid=$gid"
                        foreach g $groups {
                            lappend tags "group=$g"
                        }
                        lappend tags "type=arc"
                        set fill [$c attribute fill]
                        if {$fill eq "none"} {
                            set fill {}
                            set arcstyle arc
                            set outline [$c attribute stroke]
                            set width [$c attribute strike-width]
                        } else {
                            set arcstyle pieslice
                            set outline {}
                            set width 0
                        }
                        _findCenter $startX $startY $extentX $extentY $r1 cx cy start extent
                        set x1 [expr {$cx-$r1}]
                        set y1 [expr {$cy-$r1}]
                        set x2 [expr {$cx+$r1}]
                        set y2 [expr {$cy+$r1}]
                        $hull create arc $x1 $y1 $x2 $y2 -style $arcstyle -start $start -extent $extent -tags $tags -fill $fill -outline $outline -width $width
                        $hull bind "gid=$_gid" <KeyPress-Delete> [mymethod _delete $_gid]
                        $hull bind "gid=$_gid" <KeyPress-e> [mymethod _editArc $_gid]
                        $hull bind "gid=$_gid" <Button-3> [mymethod _itemContextMenu $_gid arc %X %Y]
                    } else {
                        incr unrecognized([$c cget -tag])
                    }
                }
                default {
                    incr unrecognized([$c cget -tag])
                }
            }
        }
    }
    
    method CommonInit {AddDialogNamespace} {
        install addpindialog using ${AddDialogNamespace}::AddPinDialog $win.addpindialog -parent $win
        install addrectdialog using ${AddDialogNamespace}::AddRectDialog $win.addrectdialog -parent $win
        install addlinedialog using ${AddDialogNamespace}::AddLineDialog $win.addlinedialog -parent $win
        install addcircdialog using ${AddDialogNamespace}::AddCircDialog $win.addcircdialog -parent $win
        install addarcdialog using ${AddDialogNamespace}::AddArcDialog $win.addarcdialog -parent $win
        install addtextdialog using ${AddDialogNamespace}::AddTextDialog $win.addtextdialog -parent $win
        
        install itemcontextmenu using menu $win.itemcontextmenu -tearoff no \
              -title {Item nnn}
        $itemcontextmenu add command -label Delete \
              -command [mymethod _contextDelete]
        $itemcontextmenu add command -label Edit   \
              -command [mymethod _contextEdit]
        $itemcontextmenu add command -label Cancel \
              -command [mymethod _contextCancel $itemcontextmenu]
        install canvascontextmenu using menu $win.canvascontextmenu \
              -tearoff no -title {Select item}
        trace add variable [myvar _gid] write [mymethod _setDirty]
        $hull configure -setdirty [mymethod _setDirty]
    }
}    

snit::listtype ViewPort -type snit::double -minlen 4 -maxlen 4
snit::enum Units -values {inch mm}

snit::widgetadaptor SetSizeDialog {
    option -width  -default 25.4 -type snit::double
    option -height -default 25.4 -type snit::double
    option -viewport -default {0 0 254 254} -type ViewPort 
    option -units -default mm -type Units
    variable _minx 0
    variable _miny 0
    variable _maxx 254
    variable _maxy 254
    delegate option -parent to hull
    constructor {args} {
        installhull using Dialog -image [IconImage image setsize] \
              -default set -cancel cancel -modal local -transient yes \
              -side bottom -title {Set size and viewport} \
              -parent [from args -parent]
        $hull add set    -text Set    -command [mymethod _Set]
        $hull add cancel -text Cancel -command [mymethod _Cancel]
        wm protocol [winfo toplevel $win] WM_DELETE_WINDOW [mymethod _Cancel]
        set frame [$hull getframe]
        set unitsLE [LabelComboBox $frame.unitsLE -textvariable [myvar options(-units)] \
                     -label "Units" -values [Units cget -values]]
        pack $unitsLE -expand yes -fill x
        set widthLE [LabelSpinBox $frame.widthLE -textvariable [myvar options(-width)] \
                     -label "Width" -range {0.0 100 .1}]
        pack $widthLE -expand yes -fill x
        set heightLE [LabelSpinBox $frame.heightLE -textvariable [myvar options(-height)] \
                     -label "Height" -range {0.0 100 .1}]
        pack $heightLE -expand yes -fill x
        set vpLF [LabelFrame $frame.vpLF -text "Viewport"]
        pack $vpLF -expand yes -fill x
        set vpframe [$vpLF getframe]
        foreach w {minx miny maxx maxy} v [list [myvar _minx] [myvar _miny] [myvar _maxx] [myvar _maxy]] {
            set e [ttk::entry $vpframe.$w -textvariable $v]
            pack $e -side left -expand yes -fill x
        }
        $self configurelist $args
    }
    method _Set {} {
        $hull withdraw
        set options(-viewport) [list $_minx $_miny $_maxx $_maxy]
        return [$hull enddialog [$self _getallopts]]
    }
    method _getallopts {} {
        set result [list]
        foreach o {-units -width -height -viewport} {
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
        lassign [$self cget -viewport] _minx _miny _maxx _maxy
        return [$hull draw]
    }
}

snit::widget SizeAndViewport {
    hulltype ttk::frame
    option -width  -default 25.4 -type snit::double
    option -height -default 25.4 -type snit::double
    option -viewport -default {0 0 254 254} -type ViewPort \
          -configuremethod _setviewport
    method _setviewport {o vp} {
        set options($o) $vp
        lassign $vp _x1 _y1 _x2 _y2
    }
    option -units -default mm -type Units
    variable _x1 0
    variable _y1 0
    variable _x2 0
    variable _y2 0
    variable _xpos 0
    variable _ypos 0
    
    constructor {args} {
        pack [ttk::label $win.l1 -text "Width: "] -side left
        pack [ttk::entry $win.w -width 4 -textvariable [myvar options(-width)]] \
              -side left -fill x -expand yes
        $win.w state readonly
        pack [ttk::label $win.u1 -textvariable [myvar options(-units)]] \
              -side left
        pack [ttk::label $win.l2 -text " Height: "] -side left
        pack [ttk::entry $win.h -width 4 -textvariable [myvar options(-height)]] \
              -side left  -fill x -expand yes
        $win.h state readonly
        pack [ttk::label $win.u2 -textvariable [myvar options(-units)]] \
              -side left
        pack [ttk::label $win.vp -text " Viewport: "] -side left
        pack [ttk::entry $win.x1 -width 4 -textvariable [myvar _x1]] \
              -side left -fill x -expand yes
        $win.x1 state readonly
        pack [ttk::entry $win.y1 -width 4 -textvariable [myvar _y1]] \
              -side left -fill x -expand yes
        $win.y1 state readonly
        pack [ttk::entry $win.x2 -width 4 -textvariable [myvar _x2]] \
              -side left -fill x -expand yes
        $win.x2 state readonly
        pack [ttk::entry $win.y2 -width 4 -textvariable [myvar _y2]] \
              -side left -fill x -expand yes
        $win.y2 state readonly
        pack [ttk::label $win.posl -text " Position: "] -side left
        pack [ttk::entry $win.xpos -width 4 -textvariable [myvar _xpos]] \
              -side left -fill x -expand yes
        $win.xpos state readonly
        pack [ttk::label $win.l3 -text ","] -side left
        pack [ttk::entry $win.ypos -width 4 -textvariable [myvar _ypos]] \
              -side left -fill x -expand yes
        $win.ypos state readonly
    }
    method updatexyposition {x y} {
        set _xpos $x
        set _ypos $y
    }
}

snit::widget FritzingPartsEditor {    
    # button-constructor name args...
    typevariable _Tools {
        {ttk::button addpin -text "Pin" -image "[IconImage image small_addpin]" -compound right -command "[list $options(-parent) addpin]"}
        {ttk::button addrect -text "Rect" -image "[IconImage image small_addrect]" -compound right -command "[list $options(-parent) addrect]"}
        {ttk::button addline -text "Line" -image "[IconImage image small_addline]" -compound right -command "[list $options(-parent) addline]"}
        {ttk::button addcirc -text "Circle" -image "[IconImage image small_addcirc]" -compound right -command "[list $options(-parent) addcirc]"}
        {ttk::button addarc -text "Arc" -image "[IconImage image small_addarc]" -compound right -command "[list $options(-parent) addarc]"}
        {ttk::button addtext -text "Text" -image "[IconImage image small_addtext]" -compound right -command "[list $options(-parent) addtext]"}
        {ttk::button setsize -text "Size" -image "[IconImage image small_setsize]" -compound right -command "[mymethod _setsize]"}
        {ttk::button shrinkw -text "ShringWrap" -image "[IconImage image small_shrinkwrap]" -compound right -command "[mymethod _shrinkwrap]"}
        {ttk::menubutton zoom -text "Zoom 1:1" -menu $zoomMenu}
    }
    
    hulltype ttk::frame
    
    component scroll
    component   canvas
    component zoomMenu
    component setsizedialog
    delegate method * to canvas except {cget configure postscript xview yview 
        bbox coords scale create}
    delegate option -background to canvas
    delegate option -bg to canvas
    component toolbuttons
    delegate method {toolbuttons *} to toolbuttons \
          except {_configDefault _configState}
    component sizeandvp
    delegate option -width to sizeandvp
    delegate option -height to sizeandvp
    delegate option -viewport to sizeandvp
    delegate option -units to sizeandvp
    
    option -parent -default {} -readonly yes -type snit::window
    option -contextmenu -default {}
    option -setdirty -default {}
    variable _zoomScale 1.0
    variable _vpscaleX 1.0
    variable _vpscaleY 1.0
    
    constructor {args} {
        set options(-parent) [from args -parent]
        set f1 [ttk::frame $win.upper]
        pack $f1 -expand yes -fill both
        install scroll using ScrolledWindow $f1.scroll -auto both \
              -scrollbar both
        pack $scroll -side left -expand yes -fill both
        set bg [from args -background white]
        set bg [from args -bg $bg]
        install canvas using canvas [$scroll getframe].canvas -bg $bg \
              -relief sunken -takefocus 1
        $scroll setwidget $canvas
        update idle
        set initialSR [list 0 0 [winfo width $canvas] [winfo height $canvas]]
        $canvas configure -scrollregion $initialSR
        install toolbuttons using ButtonBox $f1.toolbuttons -orient vertical
        pack $toolbuttons -side right -fill y
        install zoomMenu using menu $toolbuttons.zoomMenu -tearoff no
        $zoomMenu add command -label {16:1} -command "[mymethod setZoom 16]"
        $zoomMenu add command -label {8:1} -command "[mymethod setZoom 8]"
        $zoomMenu add command -label {4:1} -command "[mymethod setZoom 4]"
        $zoomMenu add command -label {2:1} -command "[mymethod setZoom 2]"
        $zoomMenu add command -label {1:1} -command "[mymethod setZoom 1]"
        $zoomMenu add command -label {1:2} -command "[mymethod setZoom .5]"
        $zoomMenu add command -label {1:4} -command "[mymethod setZoom .25]"
        $zoomMenu add command -label {1:8} -command "[mymethod setZoom .125]"
        $zoomMenu add command -label {1:16} -command "[mymethod setZoom .0625]"
        foreach tool $_Tools {
            eval [list $toolbuttons add] [subst $tool]
        }
        bind $canvas <F1> [mymethod zoomBy 2]
        bind $canvas <F2> [mymethod zoomBy .5]
        bind $canvas <F3> [mymethod setZoom 1]
        bind $canvas <Button-3> [mymethod _contextMenu %x %y]
        install sizeandvp using SizeAndViewport $win.sizeandvp
        pack $sizeandvp -fill x
        bind $canvas <Motion> [mymethod xyposition %x %y]
        #bind $canvas <Configure> [mymethod updateSR %W %h %w]
        install setsizedialog using SetSizeDialog $win.setsizedialog -parent $win
        $self configurelist $args
        $self makeVpRect
        bind $win <FocusIn> [mymethod _FocusIn]
        #$canvas bind all <Enter> [mymethod _Enter %x %y %m %s]
        #$canvas bind all <Leave> [mymethod _Leave %x %y %m %s]
    }
    method _FocusIn {} {
        #puts stderr "*** $self _FocusIn"
        focus $canvas
        $canvas focus all
    }
    method _Enter {x y m s} {
        #puts stderr "*** $self _Enter $x $y $m $s"
        set items [$canvas find closest $x $y 50]
        #puts stderr "*** $self _Enter: items found: $items"
        
    }
    method _Leave {x y m s} {
        #puts stderr "*** $self _Leave $x $y $m $s"
        set items [$canvas find closest $x $y 50]
        #puts stderr "*** $self _Leave: items found: $items"
    }
    method zoomBy {zoomFactor} {
        $canvas scale all 0 0 $zoomFactor $zoomFactor
        set _zoomScale [expr {$_zoomScale * $zoomFactor}]
        $toolbuttons itemconfigure zoom -text "[formatZoom $_zoomScale]"
        $self updateSR $canvas [winfo height $canvas] [winfo width $canvas]
    }
    method setZoom {zoomFactor} {
        if {$_zoomScale != 1} {
            set inv [expr {1.0 / double($_zoomScale)}]
            $canvas scale all 0 0 $inv $inv
        }
        $canvas scale all 0 0 $zoomFactor $zoomFactor
        set _zoomScale $zoomFactor
        $toolbuttons itemconfigure zoom -text "[formatZoom $_zoomScale]"
        $self updateSR $canvas [winfo height $canvas] [winfo width $canvas]
    }
    method xyposition {mx my} {
        set cx [$canvas canvasx $mx]
        set cy [$canvas canvasy $my]
        if {$_zoomScale != 1} {
            set inv [expr {1.0 / double($_zoomScale)}]
            set cx [expr {$cx * $inv}]
            set cy [expr {$cy * $inv}]
        }
        set x [expr {$cx / $_vpscaleX}]
        set y [expr {$cy / $_vpscaleY}]
        $sizeandvp updatexyposition $x $y
    }
    method coords {tagorid} {
        if {$_zoomScale != 1} {
            set inv [expr {1.0 / double($_zoomScale)}]
            $canvas scale all 0 0 $inv $inv
            set coords [$canvas coords $tagorid]
            $canvas scale all 0 0 $_zoomScale $_zoomScale
        } else {
            set coords [$canvas coords $tagorid]
        }
        set result [list]
        foreach {x y} $coords {
            lappend result [expr {$x / $_vpscaleX}] [expr {$y / $_vpscaleY}]
        }
        return $result
    }
    method bbox  {tagorid} {
        if {$_zoomScale != 1} {
            set inv [expr {1.0 / double($_zoomScale)}]
            $canvas scale all 0 0 $inv $inv
            set bbox [$canvas bbox $tagorid]
            $canvas scale all 0 0 $_zoomScale $_zoomScale
        } else {
            set bbox [$canvas bbox $tagorid]
        }
        set result [list]
        foreach {x y} $bbox {
            lappend result [expr {$x / $_vpscaleX}] [expr {$y / $_vpscaleY}]
        }
        return $result
    }
        
    method create {args} {
        if {$_zoomScale != 1} {
            set inv [expr {1.0 / double($_zoomScale)}]
            $canvas scale all 0 0 $inv $inv
            set result [eval [list $canvas create] $args]
            $canvas scale all 0 0 $_zoomScale $_zoomScale
        } else {
            set result [eval [list $canvas create] $args]
        }
        $canvas scale $result 0 0 $_vpscaleX $_vpscaleY
        return $result
    }
    proc formatZoom {z} {
        if {$z > 1} {
            return [format {Zoom %.0f:1} $z]
        } else {
            return [format {Zoom 1:%.0f} [expr {1.0 / $z}]]
        }
    }
    
    method getZoom {} {

        return $_zoomScale
    }
    method updateSR {c newheight newwidth} {
        set newSR 0
        set curSR [$c cget -scrollregion]
        set bbox  [$c bbox all]
        if {[lindex $bbox 2] != [lindex $curSR 2]} {
            set curSR [lreplace $curSR 2 2 [lindex $bbox 2]]
            set newSR 1
        }
        if {[lindex $curSR 2] < $newwidth} {
            set curSR [lreplace $curSR 2 2 $newwidth]
            set newSR 1
        } 
        
        if {[lindex $bbox 3] != [lindex $curSR 3]} {
            set curSR [lreplace $curSR 3 3 [lindex $bbox 3]]
            set newSR 1
        }
        if {[lindex $curSR 3] < $newheight} {
            set curSR [lreplace $curSR 3 3 $newheight]
            set newSR 1
        }
        
        if {$newSR} {
            $c configure -scrollregion $curSR
            foreach cpbox [$c find withtag All_CPs] {
                set bbox [$c bbox $cpbox]
                set bbox [lreplace $bbox 1 1 [lindex $curSR 1]]
                set bbox [lreplace $bbox 3 3 [lindex $curSR 3]]
                $c coords $cpbox $bbox
            }
        }
    }
    method _setsize {} {
        set result [$setsizedialog draw -width [$self cget -width] \
                    -height [$self cget -height] \
                    -viewport [$self cget -viewport] \
                    -units [$self cget -units]]
        if {[llength $result] == 0} {return}
        $self configurelist $result
        if {$options(-setdirty) ne ""} {
            uplevel #0 $options(-setdirty)
        }
        $self makeVpRect
    }
    method processSVGView {heightUnits widthUnits vp} {
        if {[regexp {^([[:digit:].]+)(.+)$} $heightUnits => height units] > 0} {
            $self configure -height $height
            if {$units eq {mm}} {
                $self configure -units mm
            } elseif {$units eq {in}} {
                $self configure -units inch
            }
        }
        if {[regexp {^([[:digit:].]+)(.+)$} $widthUnits => width units] > 0} {
            $self configure -width $width
            if {$units eq {mm}} {
                $self configure -units mm
            } elseif {$units eq {in}} {
                $self configure -units inch
            }
        }
        $self configure -viewport $vp
        $self makeVpRect
    }
    method _shrinkwrap {} {
        #puts stderr "*** $self _shrinkwrap: original bbox (vp coords) is [$self bbox {!viewport}]"
        set bbox [$canvas bbox {!viewport}]
        #puts stderr "*** $self _shrinkwrap: original bbox (canvas coords) is $bbox"
        lassign $bbox x1 y1 dummy1 dummy2
        $canvas move {!viewport} [expr {0-$x1}] [expr {0-$y1}]
        set bbox [$self bbox {!viewport}]
        #puts stderr "*** $self _shrinkwrap: moved bbox (vp coords) is $bbox"
        lassign $bbox dummy1 dummy2 objectWidth objectHeight
        set vp [$self cget -viewport]
        #puts stderr "*** $self _shrinkwrap: vp is $vp"
        lassign $vp dummy1 dummy2 vpwidth vpheight
        set scaleW [expr {double($objectWidth) / double($vpwidth)}]
        set scaleH [expr {double($objectHeight) / double($vpheight)}]
        #puts stderr "*** $self _shrinkwrap: size scaling: $scaleW,$scaleH"
        set newwidth [expr {[$self cget -width]*$scaleW}]
        set newheight [expr {[$self cget -height]*$scaleH}]
        set newvp [list 0 0 [expr {$vpwidth*$scaleW}] [expr {$vpheight*$scaleH}]]
        #puts stderr "*** $self _shrinkwrap: newwidth is $newwidth, newheight is $newheight, newvp is $newvp"
        $self configure -viewport $newvp -width $newwidth -height $newheight
        if {$options(-setdirty) ne ""} {
            uplevel #0 $options(-setdirty)
        }
        $self makeVpRect
    }
    method makeVpRect {} {
        set vp [$self cget -viewport]
        set width [$self cget -width]
        set height [$self cget -height]
        set vpwidth [expr {[lindex $vp 2]-[lindex $vp 0]}]
        set vpheight [expr {[lindex $vp 3]-[lindex $vp 1]}]
        #puts stderr "*** $self makeVpRect: vpwidth is $vpwidth, vpheight is $vpheight"
        set vpxscale [expr {double($width)/double($vpwidth)}]
        set vpyscale [expr {double($height)/double($vpheight)}]
        #puts stderr "*** $self makeVpRect: vpxscale is $vpxscale, vpyscale is $vpyscale"
        set aspect   [expr {$vpyscale / $vpxscale}]
        #puts stderr "*** $self makeVpRect: aspect is $aspect"
        update idle
        set inv [expr {1.0 / double($_zoomScale)}]
        $canvas scale all 0 0 $inv $inv
        set ch [winfo reqheight $canvas]
        set cw [winfo reqwidth  $canvas]
        #puts stderr "*** $self makeVpRect: ch is $ch, cw is $cw"
        if {$ch < $cw} {
            set c $ch
        } else {
            set c $cw
        }
        set _vpscaleY [expr {($c / $vpheight) * $aspect}]
        set _vpscaleX [expr {($c / $vpwidth) *  $aspect}]
        #puts stderr "*** $self makeVpRect: _vpscaleY is $_vpscaleY, _vpscaleX is $_vpscaleX"
        catch {$canvas delete viewport}
        $canvas scale all 0 0 $_vpscaleX $_vpscaleY
        $self create rectangle $vp -tags viewport -fill {} \
              -outline black -dash .
        #puts stderr "*** $self makeVpRect ch = $ch, cw = $cw"
        if {$vpwidth < $cw} {
            set halfspace [expr {($cw - $vpwidth)/2.0}]
            set srx1 [expr {0-$halfspace}]
            set srx2 [expr {$vpwidth+$halfspace}]
        } else {
            set srx1 0
            set srx2 $vpwidth
        }
        if {$vpheight < $ch} {
            set halfspace [expr {($ch - $vpheight)/2.0}]
            set sry1 [expr {0-$halfspace}]
            set sry2 [expr {$vpheight+$halfspace}]
        } else {
            set sry1 0
            set sry2 $vpheight
        }
        set sr [list $srx1 $sry1 $srx2 $sry2]
        #puts stderr "*** $self makeVpRect: sr is $sr"
        $canvas configure -scrollregion $sr
        $canvas scale all 0 0 $_zoomScale $_zoomScale
        $self updateSR $canvas [winfo height $canvas] [winfo width $canvas]
    }
    method _contextMenu {x y} {
        #puts stderr "*** $self _contextMenu $x $y"
        set items [$canvas find closest [$canvas canvasx  $x] \
                   [$canvas canvasy $y] 20]
        set vp [$canvas find withtag viewport]
        set vpindex [lsearch -exact $items $vp]
        if {$vpindex >= 0} {
            set items [lreplace $items $vpindex $vpindex]
        }
        if {[llength $items] == 0} {return}
        if {$options(-contextmenu) ne ""} {
            set X [expr {[winfo rootx $canvas]+$x}]
            set Y [expr {[winfo rooty $canvas]+$y}]
            uplevel #0 $options(-contextmenu) $items $X $Y
        }
    }
}

package provide FritzingPartsEditor 1.0
