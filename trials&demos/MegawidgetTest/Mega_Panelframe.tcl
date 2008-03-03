package require widget::all ; # or widget::superframe


    lappend auto_path ~/cvs/tcllib/tklib/modules/widget

    package require widget::panelframe
    set f [widget::panelframe .pf -text "My Panel"]
    set sf [frame $f.f -padx 4 -pady 4]
    pack [text $sf.text] -fill both -expand 1
    $f setwidget $sf
    pack $f -fill both -expand 1 -padx 4 -pady 4