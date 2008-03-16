# A Preliminary version of tcltk2 with tile under MacOS X
# I first install the Unix version of Tcl/Tk 8.4.13 in
# universal binary with tile 0.7.5 included

# Then, start R and the X11 server from within R
# Sys.putenv(DISPLAY=":0")
library(tcltk)
addTclPath("/usr/local/tcl-tk-unix/lib")
tclRequire("tile")
tt <- tktoplevel()
but <- tkwidget(tt, "ttk::button", text = "OK",
	command = function() tkdestroy(tt))
tkpack(but)
.Tcl("style theme names") # aqua is NOT there!
.Tcl("style theme use step")
.Tcl("style theme use clam")
.Tcl("style theme use alt")
.Tcl("style theme use default")
.Tcl("style theme use classic")

# It seems that the aqua version of Tcl/Tk should be used
# in order to get the aqua tile theme available
# Aqua Tcl/Tk is currently not compatible with R.app
# => what should I do??? 
