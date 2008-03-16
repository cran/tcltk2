package require widget::all ; # or widget::superframe


    package require widget::scrolledwindow
    #set sw [widget::scrolledwindow .sw -scrollbar vertical]
    #set text [text .sw.text -wrap word]
    #$sw setwidget $text
    #pack $sw -fill both -expand 1

    set sw [widget::scrolledwindow .sw -borderwidth 1 -relief sunken]
    set text [text $sw.text -borderwidth 0 -height 4 -width 20]
    $sw setwidget $text
    pack $sw -fill both -expand 1 -padx 4 -pady 4

    set sw [widget::scrolledwindow .ssw -borderwidth 2 -relief solid]
    set text [text $sw.text -borderwidth 0 -height 4 -width 20]
    $sw setwidget $text
    pack $sw -fill both -expand 1 -padx 4 -pady 4