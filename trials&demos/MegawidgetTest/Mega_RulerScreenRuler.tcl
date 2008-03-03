package require widget::all ; # or widget::superframe


    package require widget::screenruler
    set dlg [widget::screenruler .r -grid 1 -title "Screen Ruler"]
    $dlg menu add separator
    $dlg menu add command -label "Exit" -command { exit }
    $dlg display