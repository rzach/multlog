proc getfilename {text fnvar} { global fileDLGname fileDLGok
    upvar 1 $fnvar filename; set w .fileDLG; set fileDLGok 0
    catch {destroy $w}
    toplevel $w -class Dialog
    wm title $w FileDialog
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW {set fileDLGok 0}
    wm transient $w [winfo toplevel [winfo parent $w]]

    label $w.lab -text $text 
    entry $w.e -textvariable fileDLGname
    bind $w.e <Return> {set fileDLGok 1}
    bind $w.e <Escape> {set fileDLGok 0}
    set fileDLGname $filename; $w.e selection range 0 end
    frame $w.b
    button $w.b.ok -text "Ok" -command {set fileDLGok 1}
    button $w.b.cancel -text "go back"  -command {set fileDLGok 0}
    pack $w.b.ok $w.b.cancel -side left -fill x
    pack $w.lab $w.e $w.b -side top -fill x

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2  - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2  - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

    set oldFocus [focus]
    grab $w
    tkwait visibility $w
    focus $w.e

    tkwait variable fileDLGok
    if {$fileDLGok} {set filename $fileDLGname}
    destroy $w
    catch {focus $oldFocus}
    return $fileDLGok
}

# two wrappers:
proc getsavefilename {fnvar} { upvar 1 $fnvar filename
    return [getfilename "Save logic as:" filename ]
}
proc getloadfilename {fnvar} { upvar 1 $fnvar filename
    return [getfilename "Load logic from:" filename ]
}



