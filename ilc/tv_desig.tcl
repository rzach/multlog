proc desigTV {} { global tv ntv maxlen WinCnt
    if {[winfo exist .desigTV]} return
    incr WinCnt
    toplevel .desigTV
    wm title .desigTV "Designated truth values"
    wm iconname .desigTV "des.TV"
    wm protocol .desigTV WM_DELETE_WINDOW {ifnograb desigTVcancel}
    
    listbox .desigTV.lb -exportselection 0 -selectmode multiple \
        -width [expr {$maxlen + 2}] -height $ntv
    foreach t $tv {
        .desigTV.lb insert end " $t "
    }
    button .desigTV.ok -text Ok -command desigTVaccept
    button .desigTV.cancel -text Cancel -command desigTVcancel
    button .desigTV.redo -text Undo -command desigTVundo
   
    pack .desigTV.lb -side left
    pack .desigTV.ok .desigTV.cancel .desigTV.redo -side top -fill x

    desigTVundo
    catch {wm resizable 0 0}
}

proc desigTVaccept {} { global desTV
    set desTV [.desigTV.lb curselection]
    if {"$desTV" == ""} desigTVreset
    desigTVcancel
}

proc desigTVcancel {} { global WinCnt
    destroy .desigTV; incr WinCnt -1
}

proc desigTVundo {} { global desTV
    .desigTV.lb selection clear 0 end
    foreach t $desTV {
        .desigTV.lb selection set $t $t
    }
}
proc desigTVreset {} {global desTV ntv
    set desTV [expr $ntv - 1]
}


