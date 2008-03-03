# This package is built with Tk stubs for 8.4.6+ but we also provide a version
# for that is linked to tk84.dll for Tk 8.4.0-8.4.5.
# The package cannot be used with Tk < 8.4
#
if {![package vsatisfies [package provide Tcl] 8.4]} {return}
if {[package vsatisfies [package provide Tcl] 8.5] 
    || [package vsatisfies [info patchlevel] 8.4.6]} {
    package ifneeded tile 0.7.2 \
        "namespace eval tile {variable library \"$dir\"};\
         load \"[file join $dir tile072t.dll]\""
}
