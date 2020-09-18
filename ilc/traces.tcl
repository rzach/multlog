################    traces.tcl
proc set_traces {} { uplevel #0 {
    trace variable tv_str w {OnChangeTruthvalues}
    trace variable WinCnt w {OnChangeWinCnt}
    set trace_list {tv_str}
} }

proc OnChangeTruthvalues args {global changed;
    set changed 1;ResetValid;upd_autogen; disable_gntv
}
proc OnChangeWinCnt args { global WinCnt itemlist
    if {$WinCnt == 0} {
        if {"$itemlist" == ""} {
            .mid.auto conf -state normal
        }
    } else {
        .mid.auto conf -state disabled
    }
}
proc OnSetValid {} { 
    foreach n {1 2} {.bot.butt$n configure -state normal}
    global gntv ntv; set gntv $ntv
    pack .bot -before .status -fill x
    .mid.list conf -state disabled  -relief flat
    .mid.auto conf -text "truthvalues" -command {
    	if {$WinCnt == 0} ResetValid else {
    		bell; status "There are open Editors"
    	}
    }
    .mid.desig conf -state normal; desigTVreset
}
proc OnResetValid {} {
    foreach n {3 4} {.bot.butt$n configure -state disabled}
    global gntv ntv; set gntv {}
    pack forget .bot 
    focus .mid.list; upd_autogen; .mid.list co -state normal -relief sunken
    .mid.desig conf -state disabled
    errorDLGclear
}

proc clear_traces {} { uplevel #0 {
    foreach _name $trace_list {
        foreach trace [trace vinfo $_name] {eval {trace vdelete $_name} $trace}
    }
    unset _name
} }
