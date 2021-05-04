########  mainfrm.tcl  - the main part
#  Execute only through  /bin/sh-script  "ilc"

source ${ILCPATH}errdlg.tcl
source ${ILCPATH}procs.tcl; source ${ILCPATH}b_op_qu.tcl 
source ${ILCPATH}fileio.tcl; source ${ILCPATH}tkprocs.tcl 
source ${ILCPATH}traces.tcl; source ${ILCPATH}dialog.tcl

source ${ILCPATH}op_def.tcl; source ${ILCPATH}qu_def.tcl
source ${ILCPATH}ord_def.tcl; source ${ILCPATH}tv_desig.tcl
source ${ILCPATH}bopmenu.tcl; source ${ILCPATH}filedlg.tcl

# catch {tk_bisque}
catch {wm resizable . 0 0}
wm protocol . WM_DELETE_WINDOW {ifnograb finish}
wm title . "interactive Logic Creator"
wm iconname . "i L C"

# bitmap
# image create bitmap iLC_icon -background {} -file "${ILCPATH}ilc.xbm"
if [catch {wm iconbitmap . @${ILCPATH}ilc.xbm}] exit

set logicname ""; set itemlist {}; set curitem {}
set ntv {}; set gntv {}; set tv_str ""; set tv {}; set desTV {}
set lb_seln {}; set num_items 0; set PreserveStatus 0
set changed 0; set WinCnt 0; set valid 0; set filename {}

# menu
frame .menu -relief raised -bd 2
# menu file
menubutton .menu.file -text "File" -menu .menu.file.m -underline 0 -takefocus 0
menu .menu.file.m -tearoff 0 -postcommand [list OnOpenFileMenu .menu.file.m]
.menu.file.m add command -label "New" -underline 0 -command clear_all
.menu.file.m add command -label "Open ..." -underline 0 -command load_all
.menu.file.m add command -label "Save ..." -underline 0 -command save_all
.menu.file.m add separator
.menu.file.m add command -label "Exit" -underline 1 -command finish
pack .menu.file -side left
# menu window
#menubutton .menu.win -text "Windows" -menu .menu.win.m -underline 0 -takefocus 0
#menu .menu.win.m -tearoff 0 -postcommand [list OnOpenXxxxMenu .menu.xxxx.m]
#.menu.win.m add command -label "" -underline 0 -command { }
#...
# menu help
menubutton .menu.help -text "Help" -menu .menu.help.m -underline 0 -takefocus 0
menu .menu.help.m -tearoff 0 -postcommand [list OnOpenHelpMenu .menu.help.m]
.menu.help.m add command -label "Show errors" -underline 0 -command {edit_item error}
.menu.help.m add command -label "Reset errors" -underline 0 -command {errorDLGclear}
.menu.help.m add separator
.menu.help.m add command -label "About ..." -underline 0 -command menu.about
pack .menu.help -side right

# oben
frame .top -bd 2 -relief raised 
frame .top.1 -relief sunken -bd 1
label .top.label -text "Name of logic:"
entry .top.entry -width 20 -textvariable logicname
pack .top.label .top.entry -in .top.1 -side left -expand yes
pack .top.1 -fill x -expand yes
focus .top.entry

# mitte
frame .mid -bd 2 -relief raised; frame .mid.0 -bd 1 -relief sunken 
# mitte - oben
frame .mid.1
label .mid.lab1 -text "Truth values:\n(separate by comma)"
entry .mid.list -textvariable tv_str
pack .mid.lab1 .mid.list -in .mid.1 -side left -expand yes
pack .mid.1 -in .mid.0 -fill x -side top
# mitte - unten
frame .mid.2 -bd 0; frame .mid.3 -bd 0
button .mid.auto; upd_autogen 
label .mid.lab2 -text "Number:"
entry .mid.num -textvariable gntv -width 3 -state disabled -relief flat
button .mid.desig -text "Designated" -command designate -state disabled
pack .mid.lab2 .mid.num -in .mid.3 -side left -expand no
pack .mid.auto .mid.3 .mid.desig -in .mid.2 -side left -expand yes
pack .mid.2 -in .mid.0 -fill x -side bottom
pack .mid.0 -fill x

# unten
frame .bot -bd 2 -relief raised
# unten - links
frame .bot.1 -bd 1 -relief sunken
label .bot.lab -text "Already defined:"
listbox .bot.lb -exportselection no -selectmode browse \
                -width 15   -yscrollcommand ".bot.sb set" -takefocus 0
scrollbar .bot.sb -takefocus 0 -command ".bot.lb yview"
pack .bot.lab    -in .bot.1 -side top -fill x -expand yes 
pack .bot.sb -in .bot.1 -side right -fill y 
pack .bot.lb     -in .bot.1 -side right -fill both -expand yes
pack .bot.1 -side left -fill both -expand yes
# unten - rechts
frame .bot.2 -bd 1 -relief sunken
button .bot.butt1 -text "New operator" -command newOP 
button .bot.butt2 -text "New quantifier" -command newQU
button .bot.butt5 -text "New ordering" -command newORD
button .bot.butt3 -text "Edit selected" -state disabled -command {edit_item -}
button .bot.butt4 -text "Delete selected" -state disabled -command kill_item
pack .bot.butt1 .bot.butt2 .bot.butt5 .bot.butt3 .bot.butt4 -in .bot.2 \
    -side top -fill both -expand yes
pack .bot.2 -side right -fill y

# statusline
frame .status -bd 1 -relief sunken
label .status.lab -text "" -width 40 -anchor w -padx 5 -background white
pack .status.lab -fill x -expand yes 

pack .menu .top .mid .status -side top -fill x

# Basic  Key-bindings  (other files may contain some more ...)
proc set_bindings {} { global allow_arranging
    bindtags .bot.lb {Listbox .bot.lb . all}
    bind .bot.lb <Key> upd.seln
    bind .bot.lb <ButtonRelease> upd.seln
    bind .bot.lb <Double-1> {edit_item -}
    bind .bot.lb <Return> {edit_item -}

    if {[info exists allow_arranging] && $allow_arranging } {
        bind .bot.lb <Shift-Button-1> {take_item [.bot.lb index @%x,%y]}
        bind .bot.lb <Shift-B1-Motion> {drag_item [.bot.lb index @%x,%y]}
        bind .bot.lb <Shift-ButtonRelease-1> {drop_item [.bot.lb index @%x,%y]}
        bind .bot.lb <Shift-Up> {move_up [.bot.lb index active] }
        bind .bot.lb <Shift-Down> {move_down [.bot.lb index active] }
    }

    bind Button <Return> { tkButtonInvoke %W }
    set script [bind Menubutton <space>]
    bind Menubutton <Return> $script
    bind Menubutton <Up> $script
    bind Menubutton <Down> $script
    bind mpaste <1> {editOPmpasteTo %W}
    bind mpaste <Return> {editOPmpasteTo %W}
    bind mpaste <Key> {editOPmpasteKey %W %A}
    bind mselect <Key> {editOPmselectKey %W %A}
    bind tpaste <1> {editOPtpasteTo %W}
    bind tpaste <Return> {editOPtpasteTo %W}
    bind tpaste <Key> {editOPtpasteKey %W %A}
    bind tselect <1> {editOPtcutFrom %W}
    bind tselect <Return> {editOPtcutFrom %W}
    bind tselect <Key> {editOPtselectKey %W %A}
    #
    #        This line is needed because of a  bug/feature  and might be 
    #        not needed in versions of tk later than 4.0b3
    #  catch {tkEntryBind FocusIn}
    #        now i can define an additional binding for the Entry-widget 
    bind Entry <Return> { EntryReturnPressed %W }
    #bind all <Up>    { CursUp %W    }
    #bind all <Down>  { CursDown %W  }
    #bind all <Left>  { CursLeft %W  }
    #bind all <Right> { CursRight %W }
    bind all <1> +status
    bind all <3> {+ if [tkFocusOK %W] {focus %W}}
    bind all <Key> +{status}
}

set_bindings
set_traces

