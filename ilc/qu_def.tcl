##############   open new window for  operator-definition

proc editQU {{op_name {}}} { global WinCnt
    set name {}; set def {}; set type {}
    if {$op_name != {}} { global arr
        splitOPstr $op_name name arity
        catch {set 0 $arr($op_name); 
            set type [lindex $0 0]; set def [lindex $0 1]
        }
    }
    set changed 0; incr WinCnt
    # make a unique windowname 
    set w .defQU ; while {[winfo exists $w]} {append w 1}
    # make unique variables
    uplevel #0 [list array set $w [
        list op_name $op_name name $name def $def \
             type $type changed $changed
    ]]
    # array contents:   op_name    parameter to  editQU
    #                   name       content of Name-Entry
    #                   type       RadioButton-variable
    #                   def        rest of definition
    #                   changed    has definition been changed ?

    toplevel $w
    wm protocol $w WM_DELETE_WINDOW [list ifnograb editQUquit $w]
    wm title $w "Edit quantifier"
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

    label $f.1.lab -text "Specify Quantifier" -pady 5
    label $f.1.o.lab -text Name
    entry $f.opn -textvar ${w}(name) -width 10

    radiobutton $f.map -variable ${w}(type) -value mapping -anchor w -bd 2 \
        -command [list editQUsettype $w] -text "as Mapping"
    radiobutton $f.op  -variable ${w}(type) -value op      -anchor w -bd 2 \
        -command [list editQUsettype $w] -text "induced by OP"
    radiobutton $f.bop -variable ${w}(type) -value bop     -anchor w -bd 2 \
        -command [list editQUsettype $w] -text "induced by Order"

    global ntv MaxNTV_for_QuMapping
    if {[info exists MaxNTV_for_QuMapping] && $MaxNTV_for_QuMapping >= 0} {
        if {$ntv > $MaxNTV_for_QuMapping} {
            $f.map conf -state disabled
        }
    }

    button $f.accept -text "Accept Definition"  \
        -command [list editQUfinish $w]
    button $f.cancel -text "Cancel Definition" \
        -command [list editQUcancel $w]
    button $f.quit -text "Quit Editing" \
        -command [list editQUquit $w]


    pack $f.1.lab -side top -fill x
    pack $f.1.o.lab $f.opn -in $f.1.o -side top
    pack $f.1.o

    pack $f.map $f.op $f.bop -in $f.2 -side top -fill both -padx 5 -expand yes

    pack $f.accept $f.cancel $f.quit -in $f.3 -side top -fill x

    pack $f.1 -fill y -ipadx 20 -side left -fill y -expand yes
    pack $f.2 -fill y -expand yes -side left
    pack $f.3 -fill y -expand yes -side left
    upvar #0 ${w}(minx) minx ${w}(miny) miny; update idletasks 
    set minx [winfo reqwidth $w]; set miny [winfo reqheight $w]
    editQUreset2 $w

#  whatelse is to be done now ?
    
    if {$type != ""} {
        editQUview $w
    } else {
        disable $f  accept
        focus $f.op
    }
}

proc editQUsettype {w} {editQUview $w}
    
proc editQUview {w} { global FF; editQUreset2 $w
    upvar #0 ${w}(name) name ${w}(type) type ${w}(def) def \
             ${w}(changed) changed
    if {[winfo exists $w.2]} return
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

            disable $w 1.op 1.bop
            if {$def == ""} {set def [newQUlist]}

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

            set width [expr {$ntv * ($maxlen + 1) + 1}]
            frame $f.1.f.par.x -relief sunken -bd 1 -$ht 1 -height 5
            frame $f.1.f.par.x1 -height 2 -background black
            pack $f.1.f.par.x -fill x -expand 1 -side top \
                -ipady 2 -ipadx 5
            pack $f.1.f.par.x1 -fill x -expand 1 -side top \
                -ipady 1

            frame $f.1.f.val.x -relief sunken -bd 1 -$ht 1 -height 5
            frame $f.1.f.val.x1 -height 2 -background black
            pack $f.1.f.val.x -fill x -expand 1 -side top \
                -ipady 2 -ipadx 5
            pack $f.1.f.val.x1 -fill x -expand 1 -side top \
                -ipady 1

            forallq par i {
                label $f.1.f.par.$i -text "{[join [list2etvlist $par] ,]}" \
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
            pack $f.1.f.par $f.1.f.y $f.1.f.val -ipadx 1 -fill y \
                -expand yes -side left

            update idletasks
            $f.1 create window 0 0 -window $f.1.f -anchor nw
            scrollbar $f.sbv -command [list $f.1 yview] -takefocus 0 -$ht 0
            scrollbar $f.sbh -orient horiz -command [list $f.1 xview] \
                -takefocus 0 -$ht 0
            $f.1 conf -yscrollcommand [list editOPsbupd $f.sbv set]\
                      -xscrollcommand [list editOPsbupd $f.sbh set]
			editQUcalcLayout $f.1
            
            $c.1 create window 0 0 -window $c.1.f -anchor nw
            scrollbar $c.sbv -command [list $c.1 yview] -takefocus 0 -$ht 0
            $c.1 conf -yscrollcommand [list editOPsbupd $c.sbv set]
			editQUcalcLayout $c.1

            pack $c.sbv -fill y -side right
            pack $c.1 -expand no -fill both -side left

            pack $f.sbh -fill x -side bottom
            pack $f.sbv -fill y -side right
            pack $f.1 -expand yes -fill both
            wm geometry $w {}; catch {wm resizable $w 1 1}
        }
        op      { 
            frame $w.2.2 -relief raised -bd 2
            pack $w.2.2 -fill none -expand yes -padx 2 -pady 2
            set f $w.2.2;
            disable $w 1.map 1.bop
            upvar #0 ${w}(op) op ${w}(arity) arity
            if {$def != ""} {splitOPstr $def op arity} else {set op ""; set arity ""}
            frame $f.1 -relief sunken -bd 1
            frame $f.2 -relief sunken -bd 1
            frame $f.2.n
            frame $f.2.s
            frame $f.2.a

            label $f.1.lab -text "induced by:"

            label $f.2.n.lab -text "Name of OP"
            entry $f.2.n.op -textvar ${w}(op) -width 10
            label $f.2.s.1 -text "/"
            label $f.2.s.2 -text "/"
            label $f.2.a.lab -text Arity 
            entry $f.2.a.arity -textvar ${w}(arity) -width 3

            focus $f.2.n.op

            pack $f.1.lab -expand yes -padx 10
            pack $f.2.n.lab $f.2.n.op -side top
            pack $f.2.s.1 $f.2.s.2 -side top
            pack $f.2.a.lab $f.2.a.arity -side top
            pack $f.2.n $f.2.s $f.2.a -side left

            pack $f.1 $f.2 -side left -fill y
  
            enable $w.1 accept
            wm geometry $w {}
        }
        bop      { 
            frame $w.2.2 -relief raised -bd 2
            pack $w.2.2 -fill none -expand yes -padx 2 -pady 2
            set f $w.2.2;
            disable $w 1.map 1.op
            upvar #0 ${w}(bop) bop ${w}(ord) ord
            set bop [lindex $def 0]; set ord [lindex $def 1]
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

proc editQUcalcLayout {w} {
    set bboxw [winfo reqwidth $w.f]
    set bboxh [winfo reqheight $w.f]
    $w conf -scrollregion [list 0 0 $bboxw $bboxh]
    $w conf -height $bboxh -width $bboxw
}

proc editQUfinish {w} { global errpos
    upvar #0 ${w}(name) name ${w}(type) type ${w}(def) def ${w}(changed) changed
    switch -exact -- $type {
        bop { upvar #0 ${w}(bop) bop ${w}(ord) ord
            set def [list $bop $ord]
        }
        op  { upvar #0 ${w}(op) op ${w}(arity) arity
            if {$arity == ""} { splitOPstr $op op arity }
            set def [list $op/$arity]
        }
    }
    set op_str "$name/q"
    set op_def [list $type $def]
    if [validDef $op_str $op_def] { 
        global arr itemlist curitem; 
        if [info exists arr($op_str)] {
# todo: warning: $op_str does already exist  !
        } else {
            lappend itemlist $op_str 
        }
        set arr($op_str) $op_def; set curitem $op_str; upd.items
        editQUreallyquit $w; return
    } else { editQUdoErr $w $errpos }
}

proc editQUcancel {w} {
    upvar #0 ${w}(def) def ${w}(changed) changed
    if {$def != {} && $changed} {
        dialog retval .editQUquit { 
            "confirm" "Changes will be lost" warning 0 
            "Discard changes" "go back" 
        }
        if {$retval == 0} { editQUreallycancel $w }
    } else { editQUreallycancel $w }
}
proc editQUreallycancel {w} {
    upvar #0 ${w}(type) type ${w}(def) def ${w}(changed) changed
    set type {}; set def {}; set changed 0
    editQUreset2 $w
    enable $w 1.map 1.op 1.bop; disable $w 1.accept

    global ntv MaxNTV_for_QuMapping
    if {[info exists MaxNTV_for_QuMapping] && $MaxNTV_for_QuMapping >= 0} {
        if {$ntv > $MaxNTV_for_QuMapping} {
            disable $w 1.map
        }
    }
}  
proc editQUreset2 {w} {
    upvar #0 ${w}(minx) minx ${w}(miny) miny
    catch {destroy $w.2}; wm geometry $w {}
    catch {wm resizable $w 0 0} 
    wm minsize $w $minx $miny
}
proc editQUquit {w} {
    upvar #0 ${w}(changed) changed
    if $changed {
        dialog retval .editQUquit {
            "confirm" "Changes will be lost" warning 0 
            "Discard changes" "go back"
        }
        if {$retval == 0} { editQUreallyquit $w }
    } else { editQUreallyquit $w }
}
proc editQUreallyquit {w} { global $w WinCnt
    destroy $w; incr WinCnt -1; unset $w
}

proc editQUdoErr {w pos} {global errind errstr
    upvar #0 ${w}(name) name ${w}(type) type ${w}(def) def
    
    # bell; status "$errstr {$pos $errind}"
    set pos1 [lindex $pos 0]; set pos2 [lindex $pos 1] 
    switch -exact -- $pos1 {
        name  { seterrpos $w.1.opn }
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
                op      {
                    switch -exact -- $pos2 {
                        name { seterrpos $w.2.2.2.n.op }
                        arity {seterrpos $w.2.2.2.a.arity }
                    }
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





