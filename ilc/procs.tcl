#####################   procs.tcl
# variables used:  int ntv; list tv,natlist; int maxlen,valid; str RE1,RE2;
#                  str tv_str; str errind, errstr

proc echo {args} { foreach x $args { puts -nonewline " $x," }; puts "." }
#proc bell {} { puts -nonewline "\a" }

proc n2t {num} { global tv; return [ lindex $tv $num ] }
proc t2n {t}   { global tv; return [ lsearch -exact $tv $t ] }
proc n2et {num} { global tv maxlen
    return [format {%-*s} $maxlen [lindex $tv $num]]
}
proc list2etvlist {list} { set tvlist {}
    foreach x $list { lappend tvlist [n2et $x]}; return $tvlist
}
proc natlist {l} { set list {}
    for { set i 0 } { $i < $l } { incr i } { lappend list $i } 
    return $list
}
proc validLogicName {x} {  global RE1 errind errstr MaxLLogicname
    set errind -1; set errstr ""
    if {"$x" == ""} {set errind 0
        set errstr "Error: No logicname given"
        return 0
    }
    if {[string length $x] > $MaxLLogicname} {
        set errind $MaxLLogicname; 
        set errstr "Error: Logicname too long"
        return 0
    }
    if {[regexp -indices "^($RE1)(.*)\$" $x _ __ rest]} {
        set first [lindex $rest 0]; set last [lindex $rest 1]
        if {$first > $last} {return 1}
        set errind $first
    } else {set errind 0}
    set errstr "Error: Invalid character"; return 0
}
proc validItemName {x {MaxLen 0}} { global RE2 errind errstr
    set errind -1; set errstr ""
    if {"$x" == ""} {set errind 0
        set errstr "Error: Zero length identifier"
        return 0
    }
    if {$MaxLen > 0 && [string length $x] > $MaxLen} {
        set errind $MaxLen; 
        set errstr "Error: Identifier too long"
        return 0
    }
    if {[regexp -indices "^($RE2)(.*)\$" $x _ __ rest]} {
        set first [lindex $rest 0]; set last [lindex $rest 1]
        if {$first > $last} {return 1}
        set errind $first
    } else {set errind 0}
    set errstr "Error: Invalid character"; return 0
}
proc validInt {num {min {}} {max {}}} { global errind errstr
    set errind -1; set errstr ""
    if {[regexp -indices {^([-+]?[0-9]+)(.*)$} $num _ _ rest]} {
        set first [lindex $rest 0]; set last [lindex $rest 1]
    } else {
        set first 0; set last 0;
    }
    if {$first <= $last} {
        set errstr "Error: Not an Integer"; 
        set errind $first; return 0
    }    
    if {$min != "" && $num < $min} { set errind 0
        set errstr "Error: Minimum value is $min"; return 0
    }
    if {$max != "" && $num > $max} { set errind 0
        set errstr "Error: Maximum value is $max"; return 0
    } 
    return 1
}

proc makeTV {} {
    global tv ntv tv_str tv1st natlist maxlen errind errstr 
    global MinNtv MaxNtv MaxLTV
    set errind -1; set errstr ""
# making tv
    set list [split $tv_str ,]; set tv {}; set tv1st {}
    foreach v $list { 
        set t [string trim $v]; lappend tv $t
        lappend tv1st [string index $t 0] 
    }
# now checking tv  and setting related variables
    set tv_str [join $tv ,]; set ntv [llength $tv]
    if {$ntv < $MinNtv} {
        set errstr "Error: At least $MinNtv truthvalues"
        set errind [string length $tv_str]; return 0
    }
    if {$ntv > $MaxNtv} {
        set errstr "Error: At most $MaxNtv truthvalues"
        set errind [string length $tv_str]; return 0
    }
    set offset 0; set maxlen 0
    foreach x $tv {
        if ![validItemName $x $MaxLTV] {
            set errind $offset; ResetValid; return 0 
        }; set tvl [string length $x]
        if {$tvl > $maxlen} {set maxlen $tvl}
        incr offset $tvl; incr offset
    }
    set natlist [natlist $ntv]
    foreach i $natlist { 
        if { [t2n [n2t $i]] != $i } {
            set errstr "Error: Truthvalue not unique"
            set errind [string last ",[n2t $i]," ",$tv_str,"]
            ResetValid; return 0
        } 
    }
    SetValid; return 1
}

#  OnSetValid and OnResetValid  are redefined in file traces.tcl
proc OnSetValid {} {}; proc OnResetValid {} {}

proc SetValid {} { global valid
    if {$valid != 1} {set valid 1; OnSetValid}
}
proc ResetValid {} { global valid
    if {$valid != 0} {set valid 0; OnResetValid}
}

# validity-check for  operator/quantifier  (for orderings only name is checked)
# errpos indicates, which part of the definition caused the error.
# it is always a list, with either one or two elements. 
# its first element is one of:
#   "name"  invalid name of checked op/qu/ord  
#   "arity" invalid arity of checked op
#   "type"  invalid type of declaration (mapping, table, is, bop, ...)
#   "data"  error in data (depending on type)  
#             second element: specifies error location more exactly
#   "bug"   Internal Errors  (do never happen ! :-)
proc validDef {op_str op_def} { global errstr errind errpos arr ntv
    set errind -1; set errstr ""; set errpos {}
    splitOPstr $op_str op arity; 
    set type [lindex $op_def 0]; set def [lindex $op_def 1]
    if {$arity == "ord"} { global MaxLOrdname
        if {![validItemName $op $MaxLOrdname]} {
            set errpos {name}; return 0
        }
        return 1
    }
    if {$arity == "q"} { global MaxLQuname
        if {![validItemName $op $MaxLQuname]} {
            set errpos {name}; return 0
        }
        switch -exact -- $type {
            mapping { set result 1; set max [expr $ntv - 1]
                forallq p i {
                    if {$result && ![validInt [lindex $def $i] 0 $max]} {
                        set errstr "Error: Mapping incomplete"
                        set errpos [list data $i]; set result 0
                    }
                }; incr i -1; if {!$result} {return 0}
                if {[llength $def] != $i} {
                    set errstr "Mapping has wrong length"
                    set errpos {bug}; return 0
                }; return 1
            }
            op      { global MinInducedArity MaxArity itemlist
                set q_op_str [lindex $def 0]
                splitOPstr $q_op_str q_op q_arity
                if ![validItemName $q_op] {
                    set errpos {data name}; return 0
                }
                if ![validInt $q_arity $MinInducedArity $MaxArity] {
                    set errpos {data arity}; return 0
                }
                if {[lsearch -exact $itemlist $q_op_str] == -1} {
                    set errstr "Error: \"$q_op_str\" not yet defined"
                    set errpos {data name}; return 2
                }; return 1
            }
            bop     { global validbops itemlist
                set bop [lindex $def 0]; set ord [lindex $def 1]
                if {[lsearch -exact $validbops $bop ] == -1} {
                    set errstr "Error: None of [join $validbops ,] selected"
                    set errpos {data bop}; return 0
                }
                if ![validItemName $ord] {
                    set errpos {data name}; return 0
                }
                if {[lsearch -exact $itemlist ${ord}/ord] == -1} {
                    set errstr "Error: \"$ord\" not yet defined"
                    set errpos {data name}; return 2
                }; return 1
            }  
            default {
                set errstr "Error: No type of definition selected"
                set errpos {type}; return 0
            }
        }
    } else { global MinArity MaxArity MaxLOpname
        if {![validItemName $op $MaxLOpname]} {
            set errpos {name}; return 0
        }
        if ![validInt $arity $MinArity $MaxArity] {
            set errpos {arity}; return 0
        }
        switch -exact -- $type {
            mapping { set result 1; set max $ntv; incr max -1
                forallp p i $arity {
                    if {$result && ![validInt [lindex $def $i] 0 $max]} {
                        set errstr "Error: Mapping incomplete" 
                        set result 0; set errpos [list data $i]
                    }
                }; if {!$result} {return 0}
                if {[llength $def] != $i} {
                    set errstr "Mapping has wrong length"
                    set errpos {bug}; return 0
                }; return 1
            }
            table  { 
                if {$arity != 2} {
                    set errstr "Non-binary table-defined operator"
                    set errpos {bug}; return 0
                }
                set max [expr $ntv - 1]; set result 1 
                forallp p i $arity {
                    if {$result && ![validInt [lindex $def $i] 0 $max]} {
                        set errstr "Error: Mapping incomplete" 
                        set errpos [list data $i]; set result 0
                    }
                }; if {!$result} {return 0}
                if {[llength $def] != $i} {
                    set errstr "Mapping has wrong length"
                    set errpos {bug}; set result 0
                }; return $result
            }
            bop    { global validbops MinInducedArity itemlist
                if ![validInt $arity $MinInducedArity] {
                    set errpos {arity}; return 0
                }
                set bop [lindex $def 0]; set ord [lindex $def 1]
                if {[lsearch -exact $validbops $bop ] == -1} {
                     set errstr "Error: None of [join $validbops ,] selected"
                     set errpos {data bop}; return 0
                }
                if ![validItemName $ord] {
                     set errpos {data name}; return 0
                }
                if {[lsearch -exact [array names arr] ${ord}/ord] == -1} {
                     set errstr "Error: \"$ord\" not yet defined"
                     set errpos {data name}; return 2
                }; return 1
            }
            default {
                set errstr "Error: No type of definition selected"
                set errpos {type}; return 0
            }
        }
    }
}


