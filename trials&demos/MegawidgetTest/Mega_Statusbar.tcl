package require widget::all ; # or widget::superframe


     # sample usage
    eval destroy [winfo children .]
    pack [text .t -width 0 -height 0] -fill both -expand 1

    set sbar .s
    widget::statusbar $sbar
    pack $sbar -side bottom -fill x
    set f [$sbar getframe]

    # Specify -width 1 for the label widget so it truncates nicely
    # instead of requesting large sizes for long messages
    set w [tlabel $f.status -width 1 -anchor w -textvariable ::STATUS]
    set ::STATUS "This is a status message"
    # give the entry weight, as we want it to be the one that expands
    $sbar add $w -weight 1

    # BWidget's progressbar
 #   set w [ProgressBar $f.bpbar -orient horizontal \
 #	       -variable ::PROGRESS -bd 1 -relief sunken]
 #   set ::PROGRESS 50
 #   $sbar add $w

	# Tile indeterminate progressbar
	package require tile

	set w [tprogressbar $f.pbar -orient horizontal -length 100 -maximum 10 \
		-mode indeterminate -variable ::PROGRESS]
	set ::PROGRESS 5
	$sbar add $w
	$w start
	
 #+start/stop
 

### Old code not compatible any more with last tile!
#	proc every {ms body} {
#		global everyId
#		if {$ms eq "cancel"} {
#			if {[info exists everyId($body)]} {
#				after cancel $everyId($body)
#				unset everyId($body)
#			}
#		} else {
#			if {[info exists everyId($body)]} {
#				# This makes sure there is only 1 every for this body
#				after cancel $everyId($body)
#			}
#			set everyId($body) [after $ms [info level 0]]
#			uplevel #0 $body
#		}
#	}
#
#	#The second snippet, "Update," was provided by Pat Thoyts, also on the Tile developer's list, sets up the numerical logic for the progress widget:
#	proc Update {w interval} {
#		#global w interval
#
#		set v [expr {[$w get] + $interval}]
#		if {$v > [$w cget -to]} {
#			set v [$w cget -from]
#		}
#		$w set $v
# 	}
#
#	#The third snippet, "reset," sets up the "after cancel" call to close the progress bar loop:
#	proc reset {} {
# 		foreach id [after info] {after cancel $id}
#	}
#
#	#And finally, I put all these procedures and call them from this "Show Progress" procedure, developed with assistance from Pat Thoyts, which can then be called from a menu or button command:
#
#	proc ShowProgress {} {
#		toplevel .progress
#		tbutton .progress.button -width 10 -text close -command "reset; destroy .progress"
#		pack .progress.button -side right -fill x
#		tlabel .progress.text -textvariable show
#		pack .progress.text -side top -fill both
#		pack [tprogressbar .progress.show -orient horizontal -maximum 10] -fill both -side bottom
#		.progress.show state 0
#		grab .progress
#		wm title .progress "TkDarwinPorts Progress"
#		every 250 {Update .progress.show 1}
#	}
#	ShowProgress