"tk2button" <-
function(parent, tip = "", disp = NULL, use.tile = is.tile(), ...) {
	if (!is.tk()) stop("Tcl package 'Tk' is required but not loaded")
	if (use.tile) {
		if (!is.tile()) stop("Tcl package 'tile' is required but not loaded")
		..w.. <- tkwidget(parent, "ttk::button", ...)
	} else { # Default Tk button widget with font set to TkDefaultFont
		..w.. <- tkwidget(parent, "button", font = "TkDefaultFont", ...)
	}
	if (tip != "") tk2tip(..w.., tip)
	class(..w..) <- c("tk2button", "tk2widget", class(..w..))
	# Do we dispose the widget somewhere (place, pack or grid)?
	eval(substitute(eval(quote(disp))))	# This is done by evaluating it here
	return(..w..)
}

tt <- tktoplevel()
but <- tk2button(tt, text = "OK", disp = tk2pack())

"print.tk2widget" <-
function(x) {
	# A better way to print the content of a tk2widget
	cat("A tk2widget of class '", class(x)[1], "' and ID '", x$ID, "'\n", sep = "")
	invisible(x)
}

### TODO value, value<-, invoke, disp, config, config<-

# tk2 geometry manager to be used with disp() or disp = argument
"tk2place" <-
function(...){
	if (exists("..w..", where = parent.frame(), inherits = FALSE)) {
		tcl("place", get("..w..", envir = parent.frame()), ...)
	} else tcl("place", ...)
}

"tk2pack" <-
function(...) {
	if (exists("..w..", where = parent.frame(), inherits = FALSE)) {
		tcl("pack", get("..w..", envir = parent.frame()), ...)
	} else tcl("pack", ...)
}
	
"tk2grid" <-
function(...) {
	if (exists("..w..", where = parent.frame(), inherits = FALSE)) {
		tcl("grid", get("..w..", envir = parent.frame()), ...)
	} else tcl("grid", ...)
}
	
"tk2raise" <-
function(...) {
	if (exists("..w..", where = parent.frame(), inherits = FALSE)) {
		tcl("raise", get("..w..", envir = parent.frame()), ...)
	} else tcl("raise", ...)
}
	
"tk2lower" <-
function(...) {
	if (exists("..w..", where = parent.frame(), inherits = FALSE)) {
		tcl("lower", get("..w..", envir = parent.frame()), ...)
	} else tcl("lower", ...)
}





