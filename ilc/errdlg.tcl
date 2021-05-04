# Error-dialog:
# displays the contents of errorlist saved in "errorDLGmessages"
# and makes hypertext-links for those that have a "target" (errorDLGtargets)

proc errorDLG {} { global FV FF FBV errorDLGmessages errorDLGtargets
    if {[array names errorDLGmessages] == ""} {return}
    set w .errorDLG; set HT highlightthickness; errorDLGdismiss
    toplevel $w -class Dialog
    wm title $w Errors
    wm iconname $w Errors
    wm protocol $w WM_DELETE_WINDOW {ifnograb errorDLGdismiss}
    wm transient $w [winfo toplevel [winfo parent $w]]

    label $w.title -font $FBV -text "Errors in specification:" -justify center
    text $w.text -font $FF -wrap none -yscrollcommand [list $w.scroll set] \
              -setgrid true -width 60 -height 24 -$HT 0
    scrollbar $w.scroll -command [list $w.text yview] -$HT 0
    button $w.dismiss -text "Dismiss" -command errorDLGdismiss -$HT 0
    pack $w.title -side top -fill x
    pack $w.dismiss -side bottom -fill x
    pack $w.scroll -side right -fill y
    pack $w.text -expand yes -fill both
    
    set errorlist [lsort -integer [array names errorDLGmessages]] 
    foreach i $errorlist {
        set lineslist $errorDLGmessages($i)
        if [catch {set target $errorDLGtargets($i)}] {set target {}}
        set settag $i; #  only tag the first line of each message
        foreach j $lineslist {
            $w.text insert end "$j\n" $settag
            set settag {}
        }
        $w.text insert end "---\n"
        $w.text tag lower $i
        $w.text tag configure $i -font $FV
        if {$target != ""} {
            $w.text tag configure $i -underline 1
            $w.text tag bind $i <Any-Enter> [list $w.text configure -cursor hand2]
            $w.text tag bind $i <Any-Leave> [list $w.text configure -cursor {}]
            $w.text tag bind $i <Any-1> error_dlg_click
            $w.text tag bind $i <Any-B1-Motion> error_dlg_drag
            $w.text tag bind $i <Any-ButtonRelease-1> [list error_dlg_release $target]
        }
    }
    $w.text configure -state disabled
}
# these procedures make sure, the event only fires on clicking and _not_ on a
#     "click and drag away"-action
proc error_dlg_click {} {global errorDLGreallyclick; set errorDLGreallyclick 1}
proc error_dlg_drag {}  {global errorDLGreallyclick; set errorDLGreallyclick 0}
proc error_dlg_release args { global errorDLGreallyclick
    if {$errorDLGreallyclick} { eval edit_item $args }
}
set errorDLGreallyclick 0

# Error-message-Management
proc errorDLGclear {} { global errorDLGmessages errorDLGtargets
    catch {unset errorDLGmessages}
    catch {unset errorDLGtargets}
    errorDLGdismiss
}
proc errorDLGdismiss {} {
    catch {destroy .errorDLG}
}

errorDLGclear


