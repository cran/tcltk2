# Version of framebox that uses the placer.  Effects are only really
# OK when used with a static label of a suitable size of font.
proc framebox {w args} {
    frame $w
    frame $w._border -relief ridge -bd 2
    uplevel label $w._label $args

    set dist [winfo reqheight $w._label]
    set d [expr $dist/2]

    place $w._label -x [expr $dist+2] -y 1
    pack $w._border -padx $d -pady $d -expand 1 -fill both
    pack [frame $w.c] -in $w._border -padx $d -pady $d -expand 1 -fill both
    return $w
}

# An alternate version using grid which should handle different
# heights of label much better.  It uses some of the more interesting
# things you can do with the gridder...
proc framebox_grid {w args} {
    frame $w
    set gap [winfo pixels $w 2m]
    grid [frame $w._border -bd 2 -relief ridge] \
	    -rowspan 3 -columnspan 3 -padx $gap -pady $gap -sticky nsew
    incr gap $gap
    foreach rc {column row} {
	grid $rc . 0 -minsize $gap
	grid $rc . 1 -weight  1
	grid $rc . 2 -minsize $gap
    }
    grid [frame $w.c] -row 1 -column 1
    grid [uplevel label $w._label $args] -row 0 -column 1 -sticky w
    return $w
}

pack [framebox .f -text "Command:"] -expand 1 -fill both

pack [frame .f.c.1] -fill x
pack [button .f.c.1.1 -text Read] -fill x -expand 1 -side left
pack [button .f.c.1.2 -text Write] -fill x -expand 1 -side left
pack [button .f.c.2 -text "Quit" -command exit] -fill x
pack [button .f.c.3 -text "Long label - does nothing"] -fill x
