package require Tk

wm title . "Feet to Meters"
grid [ttk::frame .c -padding "3 3 12 12"] -column 0 -row 0 -sticky nwes
grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1

grid [ttk::entry .c.feet -width 7 -textvariable feet] -column 2 -row 1 -sticky we
grid [ttk::label .c.meters -textvariable meters] -column 2 -row 2 -sticky we
grid [ttk::button .c.calc -text "Calculate" -command {set ::meters [calculate $::feet]} ] -column 3 -row 3 -sticky w

grid [ttk::label .c.flbl -text "feet"] -column 3 -row 1 -sticky w
grid [ttk::label .c.islbl -text "is equivalent to"] -column 1 -row 2 -sticky e
grid [ttk::label .c.mlbl -text "meters"] -column 3 -row 2 -sticky w

foreach w [winfo children .c] {grid configure $w -padx 5 -pady 5}
focus .c.feet
bind . <Return> {set ::meters {calculate $::feet}}

#proc calculate {} {
#   if {[catch {
#       set ::meters [expr {round($::feet*0.3048*10000.0)/10000.0}]
#   }]!=0} {
#       set ::meters ""
#   }
#}