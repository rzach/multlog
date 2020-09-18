##############   open new window for  operator-definition

proc editOP {{op_name {}}} { global WinCnt
    set name {}; set arity {}; set def {}; set type {}
    if {$op_name != {}} { global arr
        splitOPstr $op_name name arity
        catch { set 0 $arr($op_name); 
            set type [lindex $0 0]; set def [lindex $0 1]
        }
    }
    set type_b $type; set changed 0; incr WinCnt
    # make a unique windowname 
    set w .defOP ; while {[winfo exists $w]} {append w 1}
    # make unique variables
    uplevel #0 [list array set $w [
        list op_name $op_name name $name arity $arity def $def\
             type $type type_b $type_b changed $changed
    ]]
    # array contents:   op_name    parameter to  editOP
    #                   name       content of Name-Entry
    #                   arity      content of Arity-Entry
    #                   type       RadioButton-variable
    #                   type_b     backup for type
    #                   def        rest of definition
    #                   changed    has definition been changed ?

    toplevel $w 
    wm protocol $w WM_DELETE_WINDOW [list ifnograb editOPquit $w]
    wm title $w "Edit operator"
    wm iconname $w Edit
    #wm transient $w [winfo toplevel [winfo parent $w]]
    #wm resizable $w 0 0
    
    frame $w.1 -relief raised -bd 2
    set f $w.1; pack $w.1 -side top -fill x
    #-----------------------------------------------------------------
    frame $f.1 -relief sunken -bd 1
    frame $f.2 -relief sunken -bd 1
    frame $f.3 -relief sunken -bd 1
    frame $f.1.o -relief sunken -bd 1
    frame $f.1.o.n
    frame $f.1.o.s
    frame $f.1.o.a

    label $f.1.lab -text "Specify Operator" -pady 5
    label $f.1.o.n.lab -text Name
    entry $f.op -textvar ${w}(name) -width 10 
    label $f.1.o.s.1 -text "/"
    label $f.1.o.s.2 -text "/"
    label $f.1.o.a.lab -text Arity 
    entry $f.arity -textvar ${w}(arity) -width 3 

    radiobutton $f.map -variable ${w}(type) -value mapping -text "as Mapping"\
        -anchor w -bd 2 -command [list editOPsettype $w]
    radiobutton $f.tab -variable ${w}(type) -value table -text "as Table"\
        -anchor w -bd 2 -command [list editOPsettype $w]
    radiobutton $f.bop -variable ${w}(type) -value bop   -text "by Order"\
        -anchor w -bd 2 -command [list editOPsettype $w]

    button $f.accept -text "Accept Definition"  \
        -command [list editOPfinish $w]
    button $f.cancel -text "Cancel Definition" \
        -command [list editOPcancel $w]
    button $f.quit -text "Quit Editing" \
        -command [list editOPquit $w]


    pack $f.1.lab -side top -fill x
    pack $f.1.o.n.lab $f.op -in $f.1.o.n -side top
    pack $f.1.o.s.1 $f.1.o.s.2 -side top
    pack $f.1.o.a.lab $f.arity -in $f.1.o.a -side top
    pack $f.1.o.n $f.1.o.s $f.1.o.a -side left
    pack $f.1.o

    pack $f.map $f.tab $f.bop -in $f.2 -side top -fill both -padx 5 -expand yes

    pack $f.accept $f.cancel $f.quit -in $f.3 -side top -fill x

    pack $f.1 -fill y -ipadx 20 -side left -fill y -expand yes
    pack $f.2 -fill y -expand yes -side left
    pack $f.3 -fill y -expand yes -side left
    upvar #0 ${w}(minx) minx ${w}(miny) miny; update idletasks 
    set minx [winfo reqwidth $w]; set miny [winfo reqheight $w]
    editOPreset2 $w

#  whatelse is to be done now ?
    if {$type != ""} {
        if {$type != "bop"} {
            $f.arity configure -state disabled -relief flat
        }
        editOPview $w
    } else {
        disable $f  accept
        focus $f.op
    }
}

proc editOPsettype {w} { global MaxArity MinArity
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type \
             ${w}(type_b) type_b ${w}(def) def ${w}(changed) changed

    switch -exact -- $type {
        table   { global MaxSize_for_Op ntv
            if {[info exists MaxSize_for_Op] && $MaxSize_for_Op >= 0} {
                if {[catch {expr {$ntv*$ntv > $MaxSize_for_Op}} res] || $res} {
                    global errstr errind; set errind -1; set type $type_b; 
                    set errstr "Operator too large for table"
                    seterrpos $w.1.bop; return
                }
            }
            if {"$arity" == ""} { set arity 2 }
            if {![validInt $arity 2 2]} {
                 set type $type_b; seterrpos $w.1.arity
                 return 
            }
            $w.1.arity conf -state disabled -relief flat
        }
        mapping { global MaxSize_for_Op ntv
            if {![validInt $arity $MinArity $MaxArity]} {
                set type $type_b; seterrpos $w.1.arity
                return 
            }
            if {[info exists MaxSize_for_Op] && $MaxSize_for_Op >= 0} {
                if {[catch {expr {pow($ntv,$arity)>$MaxSize_for_Op}} res] || $res} {
                    global errstr errind; set errind -1; set type $type_b; 
                    set errstr "Operator too large for mapping"
                    seterrpos $w.1.bop; return
                }
            }
            $w.1.arity conf -state disabled -relief flat 
        }
    }
    set type_b $type; editOPview $w
}
    
proc editOPview {w} { global FF; editOPreset2 $w
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type \
             ${w}(type_b) type_b ${w}(def) def ${w}(changed) changed

    frame $w.2 -relief flat -bd 0
    pack $w.2 -side top -fill both -expand yes
    switch -exact -- $type {
        mapping { 
            frame $w.2.1 -relief raised -bd 2
            pack $w.2.1 -side left -fill none -expand no \
                -padx 2 -pady 2 -anchor nw
            frame $w.2.2 -relief raised -bd 2
            pack $w.2.2 -fill none -expand yes -padx 2 -pady 2
            set c $w.2.1; set f $w.2.2; 
            disable $w 1.bop; if {$arity != 2} {disable $w 1.tab}
            if {$def == ""} {set def [newOPlist $arity]}

            global natlist ntv maxlen
            set ht highlightthickness

            canvas $c.1 -$ht 0
            frame $c.1.f -bd 1 -relief sunken
            foreach i $natlist { set rbut $c.1.f.$i
                radiobutton $rbut -text [n2t $i] -anchor w -padx 5\
                    -width $maxlen -bd 1 -relief sunken -font $FF \
                    -$ht 1 -value $i -variable ${w}(selection)
                bindtags $rbut [concat mselect [bindtags $rbut]]
                pack $rbut -fill x -expand 1 -side top\
                    -ipady 2 -ipadx 5
            }

            canvas $f.1 -$ht 0
            frame $f.1.f -bd 1 -relief sunken

            frame $f.1.f.par
            frame $f.1.f.y -width 2 -background black
            frame $f.1.f.val

            set width [expr {$arity * ($maxlen + 1) + 1}]
            frame $f.1.f.par.x -relief sunken -bd 0 -$ht 1 -height 2
            frame $f.1.f.par.x1 -height 2 -background black
            pack $f.1.f.par.x -fill x -expand 1 -side top \
                -ipady 2 -ipadx 5
            pack $f.1.f.par.x1 -fill x -expand 1 -side top \
                -ipady 1

            frame $f.1.f.val.x -relief sunken -bd 0 -$ht 1 -height 2
            frame $f.1.f.val.x1 -height 2 -background black
            pack $f.1.f.val.x -fill x -expand 1 -side top \
                -ipady 2 -ipadx 5
            pack $f.1.f.val.x1 -fill x -expand 1 -side top \
                -ipady 1

            if {$arity > 0} {
                forallp par i $arity {
                    label $f.1.f.par.$i -text "([join [list2etvlist $par] ,])" \
                        -width $width -takefocus 0 -font $FF \
                        -relief sunken -bd 1 -$ht 1
                    label $f.1.f.val.$i -text [n2t [lindex $def $i]] \
                        -width $maxlen -takefocus 1 -font $FF \
                        -relief sunken -bd 1 -$ht 1
                    bindtags $f.1.f.val.$i [list mpaste $w all]
                    pack $f.1.f.par.$i -fill x -expand 1 -side top\
                        -ipady 2 -ipadx 5
                    pack $f.1.f.val.$i -fill x -expand 1 -side top\
                        -ipady 2 -ipadx 5
                }
            } else {
                label $f.1.f.par.0 -takefocus 0 \
                    -relief flat -bd 1 -$ht 1 
                label $f.1.f.val.0 -text [n2t [lindex $def 0]] \
                    -width $maxlen -takefocus 1 -font $FF \
                    -relief sunken -bd 1 -$ht 1
                bindtags $f.1.f.val.0 [list mpaste $w all]
                pack $f.1.f.par.0 -fill x -expand 1 -side top\
                    -ipady 2
                pack $f.1.f.val.0 -fill x -expand 1 -side top\
                    -ipady 2 -ipadx 5
            }
            pack $f.1.f.par $f.1.f.y $f.1.f.val -ipadx 1 -fill y \
                -expand yes -side left

            update idletasks
            $f.1 create window 0 0 -window $f.1.f -anchor nw
            scrollbar $f.sbv -command [list $f.1 yview] -takefocus 0 -$ht 0
            scrollbar $f.sbh -orient horiz -command [list $f.1 xview] \
                -takefocus 0 -$ht 0
            $f.1 conf -yscrollcommand [list editOPsbupd $f.sbv set]\
                      -xscrollcommand [list editOPsbupd $f.sbh set]
            editOPcalcLayout $f.1

            $c.1 create window 0 0 -window $c.1.f -anchor nw
            scrollbar $c.sbv -command [list $c.1 yview] -takefocus 0 -$ht 0
            $c.1 conf -yscrollcommand [list editOPsbupd $c.sbv set]
            editOPcalcLayout $c.1
            
            pack $c.sbv -fill y -side right
            pack $c.1 -expand no -fill both -side left

            pack $f.sbh -fill x -side bottom
            pack $f.sbv -fill y -side right
            pack $f.1 -expand yes -fill both
            wm geometry $w {}; catch {wm resizable $w 1 1}
        }
        table   { 
            frame $w.2.1 -relief raised -bd 2
            pack $w.2.1 -side left -fill none -expand no \
                -padx 2 -pady 2 -anchor nw
            frame $w.2.2 -relief raised -bd 2
            pack $w.2.2 -fill none -expand yes -padx 2 -pady 2
            set c $w.2.1; set f $w.2.2; 
            disable $w 1.bop
            if {$def == ""} {set def [newOPlist $arity]}

            global natlist ntv maxlen
            set ht highlightthickness

            canvas $c.1 -$ht 0
            frame $c.1.f -bd 1 -relief sunken
            foreach i $natlist { set rbut $c.1.f.$i
                radiobutton $rbut -text [n2t $i] -anchor w -padx 5\
                    -width $maxlen -bd 1 -relief sunken -font $FF \
                    -$ht 1 -value $i -variable ${w}(selection)
                bindtags $rbut [concat mselect [bindtags $rbut]]
                pack $rbut -fill x -expand 1 -side top\
                    -ipady 2 -ipadx 5
            }

            canvas $f.1 -$ht 0
            frame $f.1.f -bd 1 -relief sunken
            frame $f.1.f.x
            label $f.1.f.x.y -text "" -width $maxlen -takefocus 0\
                -relief sunken -bd 1 -$ht 1 -foreground grey50 -font $FF
            frame $f.1.f.x.y1 -height 2 -background black
            pack $f.1.f.x.y -fill x -expand 1 -side top \
                -ipady 2 -ipadx 5
            pack $f.1.f.x.y1 -fill x -expand 1 -side top \
                -ipady 1
            foreach y $natlist {
                label $f.1.f.x.$y -width $maxlen -takefocus 0 -relief sunken \
                    -text [n2t $y] -bd 1 -$ht 1 -font $FF
                pack $f.1.f.x.$y -fill x -expand 1 -side top -ipady 2 -ipadx 5
                bindtags $f.1.f.x.$y [list tselect $w all]
            }
            frame $f.1.f.x1 -width 2 -background black
            pack $f.1.f.x $f.1.f.x1 -ipadx 1 -fill y -expand yes -side left
            
            foreach x $natlist {
                frame $f.1.f.$x
                label $f.1.f.$x.y -text [n2t $x] -width $maxlen -takefocus 0\
                    -relief sunken -bd 1 -$ht 1 -font $FF
                bindtags $f.1.f.$x.y [list tselect $w all]
                frame $f.1.f.$x.y1 -background black -height 2
                pack $f.1.f.$x.y -fill x -expand 1 -side top\
                    -ipady 2 -ipadx 5
                pack $f.1.f.$x.y1 -fill x -expand 1 -side top\
                    -ipady 1
                foreach y $natlist {
                    label $f.1.f.$x.$y -width $maxlen -takefocus 1 \
                        -relief sunken -bd 1 -$ht 1 -font $FF \
                        -text [n2t [lindex $def [expr {$ntv*$y+$x}]]]
                    pack $f.1.f.$x.$y -fill x -expand 1 -side top -ipady 2 \
                        -ipadx 5
                    bindtags $f.1.f.$x.$y [list tpaste $w all]
                }
                pack $f.1.f.$x -fill y -expand yes -side left
            }
            update idletasks
            $f.1 create window 0 0 -window $f.1.f -anchor nw
            scrollbar $f.sbv -command [list $f.1 yview] -takefocus 0 -$ht 0
            scrollbar $f.sbh -orient horiz -command [list $f.1 xview] \
                -takefocus 0 -$ht 0
            $f.1 conf -yscrollcommand [list editOPsbupd $f.sbv set]\
                      -xscrollcommand [list editOPsbupd $f.sbh set]
            editOPcalcLayout $f.1
            
            $c.1 create window 0 0 -window $c.1.f -anchor nw
            scrollbar $c.sbv -command [list $c.1 yview] -takefocus 0 -$ht 0
            $c.1 conf -yscrollcommand [list editOPsbupd $c.sbv set]
            editOPcalcLayout $c.1

            pack $c.sbv -fill y -side right
            pack $c.1 -expand no -fill both -side left

            pack $f.sbh -fill x -side bottom
            pack $f.sbv -fill y -side right
            pack $f.1 -expand yes -fill both
            wm geometry $w {}; catch {wm resizable $w 1 1}
        }
        bop     { 
            frame $w.2.2 -relief raised -bd 2
            pack $w.2.2 -fill none -expand yes -padx 2 -pady 2
            set f $w.2.2
            upvar #0 ${w}(bop) bop ${w}(ord) ord
            set bop [lindex $def 0]; set ord [lindex $def 1]
            disable $w 1.map 1.tab
            frame $f.1 -relief sunken -bd 1
            frame $f.2 -relief sunken -bd 1

            label $f.1.lab -text "Select a BinOp"
            bopmenu $f.1.bop ${w}(bop)
            focus $f.1.bop

            label $f.2.lab -text "Enter an Ordername"
            entry $f.2.ord -textvar ${w}(ord) -width 10
  
            pack $f.1.lab $f.1.bop -side top
            pack $f.2.lab $f.2.ord -side top
            pack $f.1 $f.2 -side left -fill y
  
            enable $w.1 accept
            wm geometry $w {}
        }
    }
}

proc editOPcalcLayout {w} {
    set bboxw [winfo reqwidth $w.f]
    set bboxh [winfo reqheight $w.f]
    $w conf -scrollregion [list 0 0 $bboxw $bboxh]
    $w conf -height $bboxh -width $bboxw
}
proc editOPtselectKey {f a} { global tv1st; set w [winfo toplevel $f]
    set value [lsearch -exact $tv1st $a]
    if {$value < 0} {return 0}
    upvar #0 ${w}(selection) sel; set sel $value
    return 1
}
proc editOPtpasteKey {f a} { if [editOPtselectKey $f $a] {editOPtpasteTo $f} }
proc editOPtpasteTo {f} { global ntv; set w [winfo toplevel $f]
    upvar #0 ${w}(selection) sel; if {$sel == ""} return
    upvar #0 ${w}(def) def ${w}(changed) changed
    set y [winfo name $f]; set x [winfo name [winfo parent $f]]
    set index [expr {$y * $ntv + $x}]
    set def [lreplace $def $index $index $sel]
    $f conf -text [n2t $sel]; set changed 1; enable $w.1 accept
}
proc editOPtcutFrom {f} { 
    set w [winfo toplevel $f]
    set value [winfo name $f]
    if {$value == "y"} { set value [winfo name [winfo parent $f]] }
    upvar #0 ${w}(selection) sel; set sel $value
}
proc editOPmselectKey {f a} { global tv1st; set w [winfo toplevel $f]
    set value [lsearch -exact $tv1st ${a}]
    if {$value < 0} {return 0}
    upvar #0 ${w}(selection) sel; set sel $value
    return 1
}
proc editOPmpasteKey {f a} { if [editOPmselectKey $f $a] {editOPmpasteTo $f} }
proc editOPmpasteTo {f} { global ntv; set w [winfo toplevel $f]
    upvar #0 ${w}(selection) sel; if {$sel == ""} return
    upvar #0 ${w}(def) def ${w}(changed) changed
    set index [winfo name $f]; set def [lreplace $def $index $index $sel]
    $f conf -text [n2t $sel]; set changed 1; enable $w.1 accept
}
proc editOPsbupd {w args} {
    if {$args == {set 0 1}} {
        eval [list $w] $args; $w conf -width 0 -bd 0
    } else {
        eval [list $w] $args; $w conf -width 15 -bd 2
    }
}

proc editOPfinish {w} { global MinArity MinInducedArity MaxArity errpos
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type \
             ${w}(type_b) type_b ${w}(def) def ${w}(changed) changed
    if {"$type" == "bop"} { upvar #0 ${w}(bop) bop ${w}(ord) ord
            set def [list $bop $ord]
            set minarity $MinInducedArity
    } else {
        set minarity $MinArity
    }
    if {![validInt $arity $minarity $MaxArity]} { 
        editOPdoErr $w {arity}; return 
    }
    set op_str "$name/$arity"; set op_def [list $type $def]
    if [validDef $op_str $op_def] { 
        global arr itemlist curitem; 
        if [info exists arr($op_str)] {
# todo: warning: $op_str does already exist  !
        } else {
            lappend itemlist $op_str
        }
        set arr($op_str) $op_def; set curitem $op_str; upd.items
        editOPreallyquit $w; return
    } else { editOPdoErr $w $errpos }
}

proc editOPcancel {w} {
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type \
             ${w}(type_b) type_b ${w}(def) def ${w}(changed) changed
    if {$def != {} && $changed} {
        dialog retval .editOPquit { 
            "confirm" "Changes will be lost" warning 0 
            "Discard changes" "go back" 
        }
        if {$retval == 0} { editOPreallycancel $w }
    } else { editOPreallycancel $w }
}
proc editOPreallycancel {w} {
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type \
             ${w}(type_b) type_b ${w}(def) def ${w}(changed) changed
    set type {}; set type_b {}; set def {}; set changed 0
    $w.1.arity configure -state normal -relief sunken
    editOPreset2 $w
    enable $w 1.map 1.tab 1.bop; disable $w 1.accept
}  
proc editOPreset2 {w} {
    upvar #0 ${w}(minx) minx ${w}(miny) miny
    catch {destroy $w.2}; wm geometry $w {}
    catch {wm resizable $w 0 0} 
    wm minsize $w $minx $miny
}
proc editOPquit {w} {
    upvar #0 ${w}(changed) changed
    if $changed {
        dialog retval .editOPquit {
            "confirm" "Changes will be lost" warning 0 
            "Discard changes" "go back"
        }
        if {$retval == 0} { editOPreallyquit $w }
    } else { editOPreallyquit $w }
}
proc editOPreallyquit {w} { global $w WinCnt
    destroy $w; incr WinCnt -1; unset $w
}

proc editOPdoErr {w pos} {global errind errstr
    upvar #0 ${w}(name) name ${w}(arity) arity ${w}(type) type ${w}(def) def
    
    # bell; status "$errstr {$pos $errind}"
    set pos1 [lindex $pos 0]; set pos2 [lindex $pos 1] 
    switch -exact -- $pos1 {
        name  { seterrpos $w.1.op }
        arity { seterrpos $w.1.arity }
        type  { seterrpos $w.1.map }
        bug   {
            tk_dialog $w.buginfo "Oh no, an error!" \
                "Internal Error:\n$errstr" warning 0 "Ok"
        }
        data  {
            switch -exact -- $type {
                mapping {
                    seterrpos $w.2.2.1.f.val.$pos2
                }
                table   { global ntv
                    set row [expr {$pos2 / $ntv}] 
                    set col [expr {$pos2 % $ntv}]
                    seterrpos $w.2.2.1.f.$col.$row
                }
                bop     {
                    switch -exact -- $pos2 {
                        bop { seterrpos $w.2.2.1.bop }
                        name { seterrpos $w.2.2.2.ord }
                    }
                }
            }
        }
    }
}

