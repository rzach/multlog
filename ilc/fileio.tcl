#####################   fileio.tcl
# save to   and   load from   file  - operations

# specify the converter program used to preprocess .lgc-files
# that program must accept two parameters:
#  inputfile:  basename of the file to convert
#  workdir:    $ILCPATH  (where the "ilc.bp" can be found)
set converter "${ILCPATH}lgc2ilc"

proc fconvert {lname} {global ILCPATH converter; 
    status "Converting $lname"
    catch {exec rm -f $lname.ilc}
    
    exec $converter $lname $ILCPATH
    
    status "Conversion finished"
}

#  Basic Write-Procedures
proc fopenw {lname} {global FileIdw; 
    status "Saving $lname"
    set FileIdw [open "${lname}.lgc" w]
}
proc fputs {{str {}}} {global FileIdw; puts $FileIdw $str }
proc fputsn str {global FileIdw; puts -nonewline $FileIdw $str }
proc fclosew {} {global FileIdw; 
    close $FileIdw 
    status "Finished"
}
set FileIdw stdout

#  Basic Read-Procedures
proc fopenr {lname {conv 1}} {global FileIdr
    if {$conv} {fconvert $lname}
    
    status "Loading $lname"
    set FileIdr [open "${lname}.ilc" r]
}
proc fgets {{strn {}}} {global FileIdr; 
    if {$strn != ""} {upvar 1 $strn str}
    return [gets $FileIdr str]
}
proc fcloser {} {global FileIdr; 
    close $FileIdr 
    status "Finished"
}
set FileIdr stdin

#  Output procedures

proc fsave {lname} { global arr itemlist
# this proc may assume, that valid==1 and logicname is valid.
# checking these, is done in  "save_all" in file  tkprocs.tcl
# Returnvalue:    0: if an error occurred;   1: saved ok 
    if {$lname == ""} {return 0}
    if [catch {
        fopenw $lname
        fputs; writeTV; fputs;
#        foreach ord [array names arr {*/ord}   ] { writeOrd $ord; fputs }
#        foreach qu  [array names arr {*/q}     ] { writeQU $qu; fputs }
#        foreach op  [array names arr {*/[0-9]*}] { writeOP $op; fputs }
        foreach item $itemlist {
            switch -glob -- $item {
            	*/ord { writeOrd $item; fputs }
            	*/q   { writeQU $item; fputs }
            	*/[0-9]* { writeOP $item; fputs }
            }
        }

            
        fclosew
    } msg ] { global errorInfo
        status "Couldn't save $lname"
    	ilc_error "$errorInfo"
    	return 0
    }
    return 1
}
proc writeTV {} { global logicname tv desTV
    regsub -all -- {"} $logicname {""} cur
    fputs "logic \"$cur\"."; fputs
    fputs "truth_values { [join $tv " , "] }."; fputs
    fputs "designated_truth_values { [join [list2etvlist $desTV] " , "] }."
}
proc writeOrd {ord} { global arr
    splitOPstr $ord o 0; regsub -all -- {"} $arr($ord) {""} cur
    fputs "ordering($o,\"$cur\")."
}
    
# for Operators
proc writeOP {op_str} { global arr natlist ntv
    splitOPstr $op_str op arity
    set type [lindex $arr($op_str) 0]; set def [lindex $arr($op_str) 1]
    switch -exact -- $type {
        mapping { 
            fputsn "operator($op/$arity, mapping {\n\t"
            if {$arity > 0} { set colon " : "; set pl "("; set pr ")"
            } else { set colon ""; set pl ""; set pr "" }
            forallp p i $arity { 
                if {$i != 0} { fputsn ",\n\t" }
                fputsn " $pl[join [list2etvlist $p] ,]$pr$colon"
                fputsn "[n2et [lindex $def $i]]"
            }
            fputs "\n\t}\n)."
        }
        table   { 
            fputs  "operator($op/$arity, table \["
            fputsn "\t  [n2et -1]   [join [list2etvlist $natlist] {, }]"
            set start 0; foreach x $natlist {
                fputsn ",\n\t  [n2et $x],  "
                set end [expr $start + $ntv - 1]; 
                set range [lrange $def $start $end]
                fputsn [join [list2etvlist $range] ", "]; 
                incr end; set start $end
            }; fputs "\n\t\]\n)."
        }
        bop { set bop [lindex $def 0]; set ord [lindex $def 1]
             fputs "operator($op/$arity, ${bop}($ord))."
        }
    }
} 
# for Quantifiers
proc writeQU {op_str} {global arr ntv maxlen
    splitOPstr $op_str op 0
    set type [lindex $arr($op/q) 0]; set def [lindex $arr($op/q) 1]
    switch -exact -- $type {
        op      { set opstr [lindex $def 0]
            fputs "quantifier($op, induced_by $opstr)."
        }
        bop     { set bopstr [lindex $def 0]; set ord [lindex $def 1]
            fputs "quantifier($op, induced_by ${bopstr}($ord))."
        } 
        mapping { 
            fputsn "quantifier($op, mapping \{\n\t"
            forallq p i { 
            	if {$i != 0} { fputsn ",\n\t" }
                fputsn " \{[join [list2etvlist $p] ,]\}"
                fputsn [format {%*s} \
                    [expr {($ntv-[llength $p])*($maxlen+1)}] "" ]
                fputsn " : [n2et [lindex $def $i]]"
            }
            fputs "\n\t\}\n)."
        }
    }
}

# Input procedures

# ReturnValue: one of "finished","openerror","bug" or "syserror"
proc fload {lname} { 
    if {$lname == ""} {return cancel}
    set error ""; set count 0; set syntax 0; errorDLGclear 
    if [catch {
        fopenr $lname 
        while {[readnext]} {}
        fcloser
    } msg] { global errorInfo
    	status "Couldn't load $lname"
    	ilc_error "$errorInfo"
    	return "syserror"
    }
    
    if {$error == "openerror"} {
        return $error
    }
    
    if {$syntax > 0} { global errorDLGmessages errorDLGtargets
        set errorDLGtargets(0) {}
        set errorDLGmessages(0) { 
            {The specification seems to be corrupted !}
            {   You may want to repair it with a text-editor}
            {   to avoid loss of whatever remained sane !}       
        }
    }
    if {$count > 0} {
        errorDLG
    }

    return $error
}

# ReturnValues:   0: fatal error, break;  1: go for next item
#   error: "bug", "openerror"
proc readnext {} {global arr logicname tv_str desTV valid itemlist
    upvar 1 error error
    if {[fgets str] < 0} {return 0}
    set 0 [lindex $str 0]
    switch -exact -- $0 {
        logic        { fgets str1
            set logicname $str1 
        }
        truth_values { 
            if $valid {set error "bug"}
            set tv_str [lindex $str 1]
            if ![makeTV] { set error "bug"}
        }
        designated_truth_values { 
            if !$valid {set error "bug"}
            #set desTV [split [lindex $str 1] ,] 
            set desTV [lindex $str 1] 
        }
        ordering     { 
            if !$valid {set error "bug"}
            set 1 [lindex $str 1]; fgets str1
            set arr($1/ord) $str1
            lappend itemlist $1/ord
        }
        operator     { 
            if !$valid {set error "bug"}
            set op [lindex $str 1]; set type [lindex $str 2]
            switch -exact -- $type {
                mapping -
                table { fgets str1
                    set arr($op) [list $type $str1]
                }
                is - bop { set bop [lindex $str 3]; set ord [lindex $str 4]
                    set arr($op) [list bop [list $bop $ord]]
                }
            }
            lappend itemlist $op
        }
        quantifier   { 
            if !$valid {set error "bug"}
            set op [lindex $str 1]; set type [lindex $str 2]
            switch -exact -- $type {
                mapping { fgets str1
                    set arr($op/q) [list mapping $str1]
                }
                op      { set opstr [lindex $str 3]
                    set arr($op/q) [list op [list $opstr]]
                }
                bop     { set bop [lindex $str 3]; set ord [lindex $str 4]
                    set arr($op/q) [list bop [list $bop $ord]]
                }
            }
            lappend itemlist $op/q
        }
        error { set type [lindex $str 1]; set lines [lindex $str 2]
                set info [lindex $str 3] 
                global errorDLGmessages errorDLGtargets
                upvar 1 count count syntax syntax  
                incr count

                switch -exact -- $type {
                    openerror {set error "openerror"; return 0}
                    syntax    {incr syntax}
                    ordsyntax {}
                    semantics {}
                    internal  {}
                }

# openerror: filedialog wiederholen
# syntax, ordsyntax, semantics, ...(?): eintragen in errorDLGmessages
#  solche mit  "info":                  eintragen in errorDLGtargets
# Warnung ins errorDLGmessages(0) eintragen ("Don't use iLC on that crap !")

                set errorDLGmessages($count) ""
                for {set i 0} {$i < $lines} {incr i} {
        	        fgets curline
                    lappend errorDLGmessages($count) $curline
                }
                if {$info != ""} {
                    set errorDLGtargets($count) $info
                } else {
                    set errorDLGtargets($count) {}
                }
        }
        % {}
        default      {}
    }
    return 1
}


