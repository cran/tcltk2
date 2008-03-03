package require widget::all ; # or widget::superframe


    package require widget::menuentry
    set me [widget::menuentry .me]
    set menu [menu .me.menu -tearoff 0]
    $menu add radiobutton -label "Name" -variable foo -value name
    $menu add radiobutton -label "Abstract" -variable foo -value abstract
    $menu add separator
    $menu add radiobutton -label "Name and Abstract" \
	-variable foo -value [list name abstract]
    $me configure -menu $menu
    pack $me -fill x -expand 1 -padx 4 -pady 4