#####################   opqu.tcl
#  routines handling the internal format of operators and quantifiers

#  l: name of list-var;  c: name of counter-var;  
#  d: arity;             s: what to execute each time
proc forallp {l c d s} { upvar 1 $l pa $c cnt 
    set cnt 0; set e [expr [info level] - 1]
    _forallp {} $d 
}
# _forallp  may ONLY be called from itself and "forallp"
proc _forallp {param depth} { global natlist; upvar 1 s s cnt cnt pa pa e e
    if {$depth <= 0} {set pa $param;catch {uplevel "#$e" $s};incr cnt;return}
    foreach x $natlist {
        _forallp [concat $param $x] [expr $depth - 1]
    }
}
#  l: name of list-var;  c: name of counter-var;  s: what to execute each time
proc forallq {l c s} { upvar 1 $l pa $c cnt
    set cnt 0; set e [expr [info level] - 1] 
    _forallq {} 0 
}
# _forallq  may ONLY be called from itself and "forallq"
proc _forallq {param depth } { upvar 1 s s cnt cnt pa pa e e; global ntv
    if {$depth >= $ntv} { if {$param != {}} { 
        set pa $param; catch {uplevel "#$e" $s}
    }; incr cnt; return }; set d [expr {$depth + 1}]
    _forallq [concat $param $depth] $d
    _forallq $param $d
}
proc splitOPstr {str opn arityn} {upvar 1 $opn op $arityn ar
    # set 0 [split $str /]; set op [lindex $0 0]; set ar [lindex $0 1]
    if {![regexp -- {^(.*)/([^/]*)$} $str _ op ar]} {
        set op $str; set ar ""
    }
}
# for   mappings and tables:
proc newOPlist {arity} {
    set list {}; forallp p i $arity {lappend list -1}
    return $list 
}
proc newQUlist {} {
    set list {}; forallq p i {lappend list -1}
    return $list
}

# the following proc's are obsolete ...
#proc calcOPindex {param} { global ntv; set arity [llength $param]
#    set index 0; for {set i 0} {$i < $arity} {incr i} { 
#        set index [expr {$index*$ntv+[lindex $param $i]}] 
#    }; return $index
#}
#proc calcQUindex {param} { global natlist
#    set index 0; foreach i $natlist {
#        set index [expr {$index << 1} ]
#        if {[lsearch -exact $param $i] == -1} { incr index }
#    }; return $index
#}
#
#proc setOPvalue {op_str param {value get}} { global ntv; 
#    splitOPstr $op_str op arity
#    if {$arity == "q"} { return [setQUvalue $op $param $value] }
#    upvar #0 arr($op_str) curr;set 0 [lindex $curr 0];set list [lindex $curr 1]
#    if {$0 != "mapping" && $0 != "table"} {error "can't set $op_str piecewise"}
#    set index [calcOPindex $param]
#    if {$value != "get"} { 
#        set curr [list $0 [lreplace $list $index $index $value]]
#    } else {
#        return [lindex $list $index]
#    }
#}
#proc setQUvalue {op param {value get}} { global natlist
#    upvar #0 arr($op/q) curr;set 0 [lindex $curr 0];set list [lindex $curr 1] 
#    if {$0 != "mapping"} {error "can't set $op_str piecewise"}
#    set index [calcQUindex $param]
#    if {$value >= 0} { 
#        set curr [list $0 [lreplace $list $index $index $value]]
#    } else {
#        return [lindex $list $index]
#    }
#}
#proc ret {arg} {return $arg}
#proc defineOP {op_str p i s} { upvar #0 arr($op_str) list;upvar 1 $p par $i cnt
#    splitOPstr $op_str op arity
#    set list {}; forallp par cnt $arity {lappend list [uplevel 1 $s]}
#}
#proc defineQU {op p i s} { upvar #0 arr($op/q) list; upvar 1 $p par $i cnt
#    set list {}; forallq par cnt {lappend list [uplevel 1 $s]}
#}

