package require widget::all ; # or widget::superframe


    lappend auto_path ~/cvs/tcllib/tklib/modules/widget

    package require widget::toolbar
	pack [button .but -text test]
	set f [ttk::frame .f -padding 4]
    pack $f -fill both -expand 1
    set tb [widget::toolbar .f.tb]
    pack $tb -fill both -expand 1
    $tb add button foo -text Foo
    $tb add button bar -text Bar -separator 1
    $tb add button baz -text Baz
    set b [ttk::button $tb.zippy -text Zippy]; # -state disabled]
    $tb add $b