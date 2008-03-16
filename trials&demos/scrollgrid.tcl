#!/usr/local/bin/wish4.1

proc scrolled_frame {w contents} {
    # Make a canvas to manage our scrolling. Note that it is impractical
    # to want scanning as well without a lot of work...
    canvas ${w}_scrollbox -xscrollcommand [list ${w}_hs set] \
	    -yscrollcommand [list ${w}_vs set] -bd 0
    frame  $w
    scrollbar ${w}_vs -command [list ${w}_scrollbox yview]
    scrollbar ${w}_hs -command [list ${w}_scrollbox xview] -orient horizontal


    # Set up a fairly standard canvas layout
    grid ${w}_scrollbox ${w}_vs -sticky nsew
    ### SUBTLETY!!!
    ### ${w}_filler _is_ necessary, but it is interesting to try without...
    grid ${w}_hs [frame ${w}_filler] -sticky nsew
    grid rowconfigure [winfo parent $w] 0 -weight 1
    grid columnconfigure [winfo parent $w] 0 -weight 1


    # Make the contents
    uplevel $contents


    # I need the geometry manager to do it's stuff on $w, so I need
    # the update idletasks in here. i.e. this is best done withdrawn.
    update idletasks


    # Now plug the scrolled area into the scroller, and set the
    # scrolling area up properly
    ${w}_scrollbox configure -scrollregion [list 0 0 \
	    [winfo reqwidth $w] [winfo reqheight $w]]
    ${w}_scrollbox create window 0 0 -anchor nw -window $w
}
# ----------------------------------------------------------------------


# We need to do some tricks with the update command, so get the
# toplevel hidden until we're ready...
wm withdraw .

# Make a scrolled frame...
scrolled_frame .scrolledbox {
    # Contents of the frame
    foreach i {{1 2 3} {4 5 6} {7 8 9} {10 11 12}} {
	set wlist grid
	foreach j $i {
	    lappend wlist [button .scrolledbox.b$j -text "Button $j" -command [list puts "You pressed button $j"]]
	}
	eval $wlist -sticky nsew
    }
}
# ...and a quit button
grid [button .quit -text Quit -command {destroy .}] - -sticky ew

# ----------------------------------------------------------------------
# OK. Now we can show the user the result of our efforts...
wm geometry . [expr 200+[winfo reqheight .quit]]x150
wm deiconify .
