if {![package vsatisfies [package provide Tcl] 8.5 9]} {
    # PRAGMA: returnok
    return
}
package ifneeded Diagrams 0.2 [list source [file join $dir draw_diagram.tcl]]
