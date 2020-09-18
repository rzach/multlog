#  dialog, tk_dialog  parameters
# retvar: variable to store the number of the chosen button in
# w: pathname (will be destroyed if it exists already)
# winname: will be displayed as window-title (by the window-manager)
# message: will be displayed in the window itself
# bitmap: will be displayed leftside to the message
#      (error, gray25, gray50, hourglass, info, question, questhead, warning)
# default: the default'th button will have a frame and the focus
# {label label ...} : each label a button 
# { action action ...} : action for the corresponding button
proc cmd_dialog {w rest}  { if {[llength $rest] != 6} {error "wrong # args"}
    set title [lindex $rest 0]; set text [lindex $rest 1]
    set bitmap [lindex $rest 2]; set default [lindex $rest 3]
    set buttlist [lindex $rest 4]; set cmdlist [lindex $rest 5]

    while {[winfo exists $w]} {append w 1}
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w Dialog
    wm transient $w [winfo toplevel [winfo parent $w]]
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both

    label $w.msg -wraplength 3i -justify left -text $text  -font $FBV
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$bitmap != ""} {
	label $w.bitmap -bitmap $bitmap
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    set i 0
    foreach but $buttlist {
	button $w.button$i -text $but -command \
	    [list _cmd_dlg_do $w [lindex $cmdlist $i] ]
	if {$i == $default} {
	    frame $w.default -relief sunken -bd 1
	    raise $w.button$i $w.default
	    pack $w.default -in $w.bot -side left -expand 1 -padx 3m -pady 2m
	    pack $w.button$i -in $w.default -padx 2m -pady 2m
	} else {
	    pack $w.button$i -in $w.bot -side left -expand 1  -padx 3m -pady 2m
	}
	incr i
    }

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2  - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2  - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w
}
# Helper function
proc _cmd_dlg_do {w s} {
    destroy $w
    eval $s
}


proc dialog {retvar w rest}  { upvar 1 $retvar result;
    set result [eval tk_dialog {$w} [eval concat [split $rest ]]]
    return $result
}

proc tk_dialog {w title text bitmap default args} {

    global tkPriv FBV

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w [winfo toplevel [winfo parent $w]]
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both -expand yes
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both

    # 2. Fill the top part with bitmap and message.

    label $w.msg -wraplength 3i -justify left -text $text  -font $FBV
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$bitmap != ""} {
        label $w.bitmap -bitmap $bitmap
        pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	button $w.button$i -text $but -command "set tkPriv(button) $i"
	if {$i == $default} {
	    frame $w.default -relief sunken -bd 1
	    raise $w.button$i $w.default
	    pack $w.default -in $w.bot -side left -expand 1 -padx 3m -pady 2m
	    pack $w.button$i -in $w.default -padx 2m -pady 2m
	    bind $w <Return> "$w.button$i flash; set tkPriv(button) $i"
	} else {
	    pack $w.button$i -in $w.bot -side left -expand 1  -padx 3m -pady 2m
	}
	incr i
    }

    # 4. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2  - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2  - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

    # 5. Set a grab and claim the focus too.

    set oldFocus [focus]
    grab $w
    tkwait visibility $w
    if {$default >= 0} {
	focus $w.button$default
    } else {
	focus $w
    }

    # 6. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.

    tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    destroy $w
    return $tkPriv(button)
}


proc ilc_error {text} {
    if [winfo exists .ilc_error] return    
    tk_dialog .ilc_error  "Error"  $text  error  0  "  OK  "
}



