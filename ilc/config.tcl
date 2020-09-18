#############  config.tcl

########################################################################
# user preferences:

# variable allow_arranging: if set to "1", then you can use the Shift-key
#   in connection with "Up"/"Down" or Mouse-dragging to manipulate the 
#   order of operators/quantifiers/orderings.
set allow_arranging 1

# Variable DefFileName: this will be the default-name initially offered
#   by the File-open and File-save dialogs.
set DefFileName "demo"

# Error-messages:
# 0: most errors will be reported in the status-line of the Main-window.
#       (will also ring the bell, so you'll know when something's wrong)
# 1: All errors will be reported in Dialog-windows
#       (will be silent, but you'll have to click "Ok" to continue ...)
set AlwaysInDialog 0

# Quantifier-mappings have 2^n-1 entries (where n denotes the number
#   of truthvalues). For sufficiently large numbers of truthvalues
#   the option "mapping" will be disabled for defining quantifiers.
# Negative values turn limitation off, 0 will disable mappings at all. 
#   A value of 8 is recommended. (You really wouldn't like to enter
#   more than 256 values, anyway ...)
# You can still define Qu's for larger n using Op's or inf/sup on Ord's.
set MaxNTV_for_QuMapping 8

# You can also set a limit on the maximum "size" of Operators,
#   that can be defined element-wise. (size denotes n^arity.)
# Negative values turn limitation off, 0 will disable mappings and tables
#   at all. A value of about 256 is recommended. 
# You can still define larger Op's using inf/sup on Ord's.
set MaxSize_for_Op 256




############################################################################
# program-constants
#   no user-serviceable parts below this line. 

# Fonts:  FF: fixed length,  FV: variable length,  FBV: variable but larger
#   FF  MUST be a NON-PROPORTIONAL font ,  
#   for FV and FBV it's only a matter of taste
set FF -*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*
set FV -*-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-*
set FBV -*-Times-Medium-R-Normal--*-180-*-*-*-*-*-*

# Number of truth_values
set MaxNtv 255
set MinNtv 2

# Arity of operators
set MaxArity 255
set MinArity 0
# Arity of references in quantifiers  and  of operators defined as BOP
set MinInducedArity 2

# Maximum lengths of identifiers
set MaxLLogicname 20
set MaxLOpname 10
set MaxLQuname 10
set MaxLOrdname 10
set MaxLTV 10

# Don't change these Regular Expressions
# RE1  valid  logicnames
# set RE1 {[ !"#$%&'()*+,./0-9:;<=>?@A-Z[]^_`a-z{|}~-]*}
set RE1 {[ -~]*}
# RE2  valid  itemnames (that are truthvalues and operators)
set RE2 {[a-z][A-Za-z0-9_]*|[-+*^<>=~?@#$&]+|0|[1-9][0-9]*}

# list of the operators, that can be used on orderings
# (not necessary  semantically unique)
set validbops {sup inf}

# ----  not  yet effective 
set MaxMapping 100

# set defined fonts
option add *Label*Font $FV
option add *Button*Font $FV
option add *Menubutton*Font $FV
option add *Radiobutton*Font $FV
option add *Menu*Font $FV
option add *Entry*Font $FF
option add *Listbox*Font $FF
