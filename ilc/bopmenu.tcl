proc bopmenu {w varName} { global validbops FF
    upvar #0 $varName var
    if ![info exists var] {
	set var {   }
    }
    menubutton $w -textvariable $varName -indicatoron 1 -menu $w.m \
        -relief raised -bd 2  -highlightthickness 2 \
        -anchor c -takefocus 1 -font $FF -width 4
    menu $w.m -tearoff 0 -font $FF
    foreach i $validbops {
	$w.m add command -label $i -command [list set $varName $i]
    }
    return $w.m
}
