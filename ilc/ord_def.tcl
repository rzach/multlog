proc editORD {{ord_name {}}} { global WinCnt
    set name {}; set def {}
    if {$ord_name != {}} { global arr
        splitOPstr $ord_name name arity
        catch {set def $arr($ord_name)}
    }
    
    # make a unique windowname 
    incr WinCnt; set w .defORD ; while {[winfo exists $w]} {append w 1}
    # make unique variables
    uplevel #0 [list array set $w [
        list ord_name $ord_name name $name def $def
    ]]
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW [list ifnograb editORDquit $w]
    wm title $w "Edit ordering"
    wm iconname $w Edit

    frame $w.1 -relief raised -bd 2
    set f $w.1; pack $w.1 -side top -fill x

    frame $f.1 -relief sunken -bd 1
    frame $f.3 -relief sunken -bd 1
    frame $f.1.o -relief sunken -bd 1

    label $f.1.lab -text "Specify Ordering" -pady 5
    label $f.1.o.lab -text Name
    entry $f.1.o.opn -textvar ${w}(name) -width 10

    button $f.accept -text "Accept Definition"  \
        -command [list editORDfinish $w]
    button $f.cancel -text "Cancel Definition" \
        -command [list editORDcancel $w]
    button $f.quit -text "Quit Editing" \
        -command [list editORDquit $w]

    pack $f.1.lab -side top -fill x
    pack $f.1.o.lab $f.1.o.opn -side top
    pack $f.1.o

    pack $f.accept $f.cancel $f.quit -in $f.3 -side top -fill x

    pack $f.1 -fill y -ipadx 20 -side left -fill y -expand yes
    pack $f.3 -fill y -expand yes -side left
    upvar #0 ${w}(minx) minx ${w}(miny) miny; update idletasks 
    set minx [winfo reqwidth $w]; set miny [winfo reqheight $w]
    wm geometry $w {}; wm minsize $w $minx $miny

    set f $w.2
    frame $f -relief raised -bd 2
    frame $f.1 -relief sunken -bd 1
    label $f.1.lab -text "Definition:"
    entry $f.1.def -textvar ${w}(def)
    button $f.1.fill -text "Set to Linear Ordering" \
        -command [list editORDfill $w]

    pack $f.1.fill -side bottom
    pack $f.1.lab -side left
    pack $f.1.def -fill x -expand yes -ipady 2
    pack $f.1 -fill both -expand yes
    pack $f -fill x -expand yes

    if {$ord_name == ""} {
        focus $w.1.1.o.opn
    } else { 
        focus $w.2.1.def
    }
    editORDdefchg $w; upvar #0 ${w}(changed) changed; set changed 0
    uplevel #0 [list trace variable ${w}(def) w [list editORDdefchg $w]] 
}

proc editORDfill w { global tv; upvar #0 ${w}(def) def
    if {$def != ""} {
        dialog retval .confirm { confirm "Previous definition will be lost"
            warning 0 "Replace" "go back"
        }
        if {$retval != 0} return 
    }
    set def " [join $tv " < "] "
}    

proc editORDdefchg {w args} { upvar #0 ${w}(def) def ${w}(changed) changed
    if {$def != ""} {
        set changed 1; $w.1.accept conf -state normal 
    } else {
        set changed 0; $w.1.accept conf -state disabled
    }
}
    
proc editORDquit {w} {
    upvar #0 ${w}(changed) changed
    if $changed {
        dialog retval .editORDquit {
            "confirm" "Changes will be lost" warning 0 
            "Discard changes" "go back"
        }; if {$retval != 0} return
    }; editORDreallyquit $w 
}

proc editORDreallyquit w { global WinCnt $w
    destroy $w; incr WinCnt -1; unset $w
}

proc editORDcancel {w} {
    upvar #0 ${w}(def) def ${w}(changed) changed
    if {$def != {}} {
        dialog retval .editORDquit { 
            "confirm" "Really discard definition ?" warning 0 
            "Discard definiton" "go back" 
        }; if {$retval != 0} return
    } 
    upvar #0 ${w}(def) def; set def {}
}

proc editORDfinish {w} { global arr itemlist curitem; 
    upvar #0 ${w}(def) def ${w}(name) name ${w}(ord_name) ord_name

    set ord_str "$name/ord"
    if {[validDef $ord_str $def]} {
        if [info exists arr($ord_str)] {
# todo: warning: $ord_str does already exist  !
        } else {
            lappend itemlist $ord_str
        }
        set arr($ord_str) $def; set curitem $ord_str; upd.items
        editORDreallyquit $w; return
    } else { global errpos; editORDdoErr $w $errpos }
}

proc editORDdoErr {w pos} {global errind errstr
    # bell; status "$errstr {$pos $errind}"
    set pos1 [lindex $pos 0]; set pos2 [lindex $pos 1] 
    switch -exact -- $pos1 {
        name  { seterrpos $w.1.1.o.opn }
        bug   {
            tk_dialog $w.buginfo "Oh no, an error!" \
                "Internal Error:\n$errstr" warning 0 "Ok"
        }
        data  { seterrpos $w.2.1.def }
    }
}

