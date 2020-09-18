#################   tkprocs.tcl
# procedures, that directly interact with the widgets

proc enter_gntv {} { global gntv
    if [string length $gntv] {
        EntryReturnPressed .mid.num
    } else {
        .mid.num co -state normal -relief sunken;
        set gntv {}; focus .mid.num
    }
}
proc seterrpos {win} {global errind errstr AlwaysInDialog
    if {$errind != "-1"} {catch {$win icursor $errind}}
    if {[info exists AlwaysInDialog] && $AlwaysInDialog} {
        ilc_error $errstr; update idletasks
    } else {
        bell;  status $errstr
    }
    catch {focus $win}
}

proc EntryReturnPressed {window} { global tv_str tv ntv gntv MinNtv MaxNtv
    switch -exact -- $window {
        .mid.num   { 
            if {! [validInt $gntv $MinNtv $MaxNtv]} {
                seterrpos .mid.num; return
            }; set ntv $gntv
            set tv_str [join [natlist $ntv] ,]
            disable_gntv
        }
        .mid.list  {
            if {$tv_str!=""} { checkTV
            } else { focus .mid.auto }
        }
        default    { focusNext $window }
    }
}

proc DragOn {w dx dy} {
    winfo containing -displayof $w [expr {[winfo rootx $w] + $dx}] \
                                   [expr {[winfo rooty $w] + $dy}]
}
proc disable {w args} {
    foreach win $args {$w.$win conf -state disabled}
}
proc enable {w args} {
    foreach win $args {$w.$win conf -state normal}
}


proc move_up {index} { global itemlist changed
    set item [lindex $itemlist $index]

    set ypos [lindex [.bot.lb yview] 0]
    set itemlist [lreplace $itemlist $index $index]
    .bot.lb delete $index
    incr index -1
    set itemlist [linsert $itemlist $index $item]
    .bot.lb insert $index $item
    .bot.lb yview moveto $ypos
    .bot.lb selection clear 0 end
    .bot.lb selection set $index
    .bot.lb activate $index
    .bot.lb see $index
    set changed 1
}
proc move_down {index} { global itemlist changed
    set item [lindex $itemlist $index]

    set ypos [lindex [.bot.lb yview] 0]
    set itemlist [lreplace $itemlist $index $index]
    .bot.lb delete $index
    incr index
    set itemlist [linsert $itemlist $index $item]
    .bot.lb insert $index $item
    .bot.lb yview moveto $ypos
    .bot.lb selection clear 0 end
    .bot.lb selection set $index
    .bot.lb activate $index
    .bot.lb see $index
    set changed 1
}

proc move_item {src dest} { global itemlist changed
    #assumes that src and dest are valid !
    if {$src == $dest} {return}
    set item [lindex $itemlist $src]

    set itemlist [lreplace $itemlist $src $src]
    set itemlist [linsert $itemlist $dest $item]

    set ypos [lindex [.bot.lb yview] 0]
    .bot.lb delete $src
    .bot.lb insert $dest $item
    .bot.lb yview moveto $ypos

    .bot.lb selection clear 0 end
    .bot.lb selection set $dest
    .bot.lb activate $dest

    set changed 1
}

proc take_item {index} {global move_item
    set move_item $index
}
proc drag_item {index} {global move_item
    drop_item $index
    take_item $index
}
proc drop_item {index} {global move_item
    if {"$move_item" == ""} {return}
    move_item $move_item $index
    set move_item ""
}
    
# focus-change is supposed to happen here:
#proc CursUp {w} { set c [winfo class $w]
#    switch -exact -- $c {
#        Listbox - Menu - Menubutton - Text {}
#        default {focusPrev $w }
#   }
#}
#proc CursDown {w} { set c [winfo class $w]
#    switch -exact -- $c {
#        Listbox - Menu - Menubutton - Text {}
#        default { focusNext $w }
#    }
#}
#proc CursLeft {w} { set c [winfo class $w]
#    switch -exact -- $c {
#        Entry - Menu - Menubutton - Text {}
#        default { focusPrev $w }
#    }
#}
#proc CursRight {w} { set c [winfo class $w]
#    switch -exact -- $c {
#        Entry - Menu - Menubutton - Text {}
#        default { focusNext $w }
#    }
#}

proc focusNext {{w .}} { focus [ tk_focusNext [ focus -lastfor $w] ] }
proc focusPrev {{w .}} { focus [ tk_focusPrev [ focus -lastfor $w] ] }

# designate truthvalues
proc designate {} {
    desigTV
}

#  create new OPERATOR
proc newOP {} {
    editOP
}

# create new QUANTIFIER
proc newQU {} {
    editQU
}

# create new ORDER
proc newORD {} {
    editORD
}

# modify selected item 
proc edit_item {item} {
    if {$item == "-"} {
        set curseln [lindex [.bot.lb curselection] 0]
        if {"$curseln" != ""} {
            set item [.bot.lb get $curseln]
        } else {return}
    }
    switch -glob -- $item {
        */[0-9]* {editOP $item}
        */q {editQU $item}
        */ord {editORD $item}
        desig {desigTV}
        error {errorDLG}
    }       
}

# remove selected item 
proc kill_item {} { global arr itemlist curitem
    set curseln [lindex [.bot.lb curselection] 0]
    if {"$curseln" == ""} {return}; # nothing selected? well, then forget it
    set del [.bot.lb get $curseln]
    catch {unset arr($del)}
    set index [lsearch -exact $itemlist $del]
    if {$index >= 0} {
        set itemlist [lreplace $itemlist $index $index]
    }
    if {$index >= [llength $itemlist]} { set index end } 
    set curitem [lindex $itemlist $index]
    upd.items; 
}

proc load_all {} { global changed WinCnt
    if {$WinCnt > 0} {
        ilc_error "You must first close all editors"
        return
    }
    if {$changed != "0"} {
        dialog retval .confirm {
             confirm "Changes will be lost" warning 0
             "Load new logic" "save first" "go back" 
        } 
        switch -exact -- $retval { 
             0 {really_load_all}
             1 {if [save_all] {really_load_all}}
        }
    } else really_load_all
    return 
}
proc really_load_all {} { global changed curitem
    set again 1; upvar #0 DefFileName filename
    while {$again} { set again 0
        set retval [getloadfilename filename]
        if {!$retval} {return} ;# cancelled
        
        if {![file readable "${filename}.lgc"]} {
            ilc_error "Can't open ${filename}.lgc"
            set again 1; continue
        }
        really_clear_all
        
        set retval [fload $filename]
        if {$retval == "openerror"} {
            ilc_error "Can't open ${filename}.lgc"
            set again 1; continue
        }
    }
    if {$retval != ""} {status $retval}
    set curitem {}; upd.items; set changed 0 
}

proc save_all {} { global logicname valid changed WinCnt
    if {$valid == 0} {ilc_error "Nothing here worth saving !"; return 0}
    if {![validLogicName $logicname]} {
        seterrpos .top.entry; return 0
    }

    if {$WinCnt > 0} {
        dialog retval .cannot {
            warn "Warning! Contents of open editors will not be saved !"
            error 0 "Ok" "go back"
        }
        if {$retval == 1} {return 0}
    }

    # now it's ok ...
    upvar #0 DefFileName filename
    set result [getsavefilename filename]
    if {!$result} {return 0}
    set result [fsave $filename]

    # return  1 if we saved successfully; return 0 otherwise (error/abort/...)
    if $result {set changed 0; return 1} {return 0}
} 

proc clear_all {} { global changed WinCnt
    if {$WinCnt > 0} {
        ilc_error "You must first close all editors"
        return
    }
    if {$changed != "0"} {
        dialog retval .confirm {
             confirm "Changes will be lost" warning 0
             "Clear workspace" "save first" "go back"
        } 
        switch -exact -- $retval {
             0 {really_clear_all}
             1 {if [save_all] {really_clear_all}}
        }
    } else really_clear_all
}
proc really_clear_all {} { 
    global changed arr ntv tv tv_str logicname desTV itemlist curitem
    catch {unset arr}; set itemlist {}; set curitem {}; set desTV {};
    upd.items
    set ntv {}; set tv {}; set tv_str ""; set logicname ""
    upd.items; set changed 0; errorDLGclear 
}

proc ifnograb args { 
    set grabbed [grab current]
    if {$grabbed == ""} {
        uplevel 1 [list eval $args]
    } else {
        bell; catch {raise $grabbed; focus $grabbed}
    } 
}

proc finish {} { global changed
    if {"$changed" != "0"} {
        dialog retval .confirm {
             confirm "Changes will be lost" warning 0
             "Quit program" "Save" "go back"
        } 
        switch -exact -- $retval {
             0 {destroy .}
             1 {if [save_all] {destroy .}}
        }
    } else {destroy .}
}

proc menu.about {} { global ILCPATH
    tk_dialog .about About \
        "interactive Logic Creator\nVersion @ilc_version@\n by A. Leitgeb" \
        @${ILCPATH}ilc.xbm 0 "  Worship  Author  "
}

proc checkTV {} {
    if [makeTV] { 
        focus .bot.butt1 
    } else { 
        seterrpos .mid.list
    }
}
proc upd_autogen {} { global tv_str
    if [string length $tv_str] {
        .mid.auto co -text "Start Editing" -command checkTV \
             -width 12 -state normal
    } else {
        .mid.auto co -text "autogenerate" -command enter_gntv \
             -width 12 -state normal
    }
}
proc OnOpenFileMenu {m} { global valid WinCnt
    if {$valid} {
        $m entryconfigure Save* -state normal
    } else {
        $m entryconfigure Save* -state disabled
    }
    if {$WinCnt == 0} {
        $m entryconfigure New* -state normal
        $m entryconfigure Open* -state normal
    } else {
        $m entryconfigure New* -state disabled
        $m entryconfigure Open* -state disabled
    }
}
proc OnOpenHelpMenu {m} { global errorDLGmessages
    if {[array names errorDLGmessages] != ""} {
        $m entryconfigure Show* -state normal
        $m entryconfigure Reset* -state normal
    } else {
        $m entryconfigure Show* -state disabled
        $m entryconfigure Reset* -state disabled
    }
}
proc disable_gntv {} { global gntv
    .mid.num co -state disabled -relief flat; set gntv {}
    update idletasks; focus .mid.list
}
proc upd.items {} {  global arr changed WinCnt itemlist curitem
    .bot.lb delete 0 end; 
    eval {.bot.lb insert end} $itemlist
    if { [.bot.lb size] != "0" } then {
        .mid.auto conf -state disabled
        .bot.lb   conf -takefocus 1
    } else {
        if {$WinCnt == 0} {.mid.auto conf -state normal}
        .bot.lb   conf -takefocus 0
    }
    if {"$curitem" != ""} {
        set index [lsearch $itemlist $curitem]
        if {$index == -1} { set index 0 }
    } else { set index 0 }
    .bot.lb activate $index
    .bot.lb see $index
    .bot.lb selection clear 0 end
    .bot.lb selection set $index

    set curitem {}; set changed 1 
    upd.seln
}

proc upd.seln {} { global lb_seln curitem
# has the selection changed ?
    
#    .bot.lb selection clear 0 end
#    .bot.lb selection set [.bot.lb index active]
    set cur_seln [.bot.lb curselection]
    if {"$cur_seln" != ""} {
        .bot.lb activate [lindex $cur_seln 0]
    }
    if {"$lb_seln" != "$cur_seln"} {
        set lb_seln $cur_seln
        if { $lb_seln != "" }  {
            .bot.butt3 conf -state normal
            .bot.butt4 conf -state normal                                       
        } else {
            .bot.butt3 conf -state disabled
            .bot.butt4 conf -state disabled
        }
    }
}

proc status {{str {}}} { global PreserveStatus
    if {$str == "" && $PreserveStatus} return
    set PreserveStatus 1 
    .status.lab configure -text $str; update idletasks
    after idle {set PreserveStatus 0}
}






