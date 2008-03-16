package require widget::all ; # or widget::superframe


    package require widget::dialog
    set dlg [widget::dialog .pkgerr -modal local -separator 1 \
		 -place right -parent . -type okcancel \
		 -title "Dialog Title"]
    set frame [frame $dlg.f]
    label $frame.lbl -text "Type Something In:"
    entry $frame.ent
    grid $frame.lbl $frame.ent -sticky ew
    grid columnconfigure $frame 1 -weight 1
    $dlg setwidget $frame
    puts [$dlg display]
    destroy $dlg

    # Using -synchronous with a -type custom dialog requires that the
    # custom buttons call [$dlg close $reason] to trigger the close
    set dlg [widget::dialog .pkgerr -title "Yes/No Dialog" -separator 1 \
		 -parent . -type custom]
    set frame [frame $dlg.f]
    label $frame.lbl -text "Type Something In:"
    entry $frame.ent
    grid $frame.lbl $frame.ent -sticky ew
    grid columnconfigure $frame 1 -weight 1
    $dlg setwidget $frame
    $dlg add button -text "Yes" -command [list $dlg close yes]
    $dlg add button -text "No" -command [list $dlg close no]
    puts [$dlg display]

