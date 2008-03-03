if {[catch {package require Tk 8.4}]} return
set script "load \"[file join $dir shellicon22.dll]\" shellicon"
package ifneeded shellicon 2.2.3 $script
