#### TO DO: Img, tree and treeview R functions!!!

"tk2button"      <- function(parent, ...) tkwidget(parent, "ttk::button", ...)
"tk2canvas"      <- function(parent, ...) tkwidget(parent, "canvas", ...)
"tk2checkbutton" <- function(parent, ...) tkwidget(parent, "ttk::checkbutton", ...)
"tk2combobox"    <- function(parent, ...) tkwidget(parent, "ttk::combobox") #{tclRequire("combobox"); tkwidget(parent, "combobox::combobox", ...)}
"tk2entry"       <- function(parent, ...) tkwidget(parent, "ttk::entry", ...)
"tk2frame"       <- function(parent, ...) tkwidget(parent, "ttk::frame", ...)
"tk2label"       <- function(parent, ...) tkwidget(parent, "ttk::label", ...)
"tk2labelframe"  <- function(parent, ...) tkwidget(parent, "ttk::labelframe", ...)
"tk2listbox"     <- function(parent, ...) tkwidget(parent, "ttk::listbox", ...)
"tk2mclistbox"   <- function(parent, ...) {tclRequire("mclistbox"); tkwidget(parent, "mclistbox::mclistbox", ...)}
"tk2menu"        <- function(parent, ...) tkwidget(parent, "menu", ...)
"tk2menubutton"  <- function(parent, ...) tkwidget(parent, "ttk::menubutton", ...)
"tk2message"     <- function(parent, ...) tkwidget(parent, "message", ...)
"tk2notebook"    <- function(parent, ...) tkwidget(parent, "ttk::notebook")
"tk2panedwindow" <- function(parent, ...) tkwidget(parent, "panedwindow", ...)
#"tk2panedwindow" <- function(parent, ...) tkwidget(parent, "ttk::paned", ...)
"tk2progress"    <- function(parent, ...) tkwidget(parent, "ttk::progressbar")
"tk2radiobutton" <- function(parent, ...) tkwidget(parent, "ttk::radiobutton", ...)
"tk2scale"       <- function(parent, ...) tkwidget(parent, "ttk::scale", ...)
"tk2scrollbar"   <- function(parent, ...) tkwidget(parent, "ttk::scrollbar", ...)
"tk2spinbox"     <- function(parent, ...) tkwidget(parent, "spinbox", ...)
"tk2table"       <- function(parent, ...) {tclRequire("Tktable"); tkwidget(parent, "table", ...)}
"tk2text"        <- function(parent, ...) tkwidget(parent, "text", ...)
"tk2tree"        <- function(parent, ...) tkwidget(parent, "tree", ...)
"tk2treeview"    <- function(parent, ...) tkwidget(parent, "treeview", ...)
"tk2separator"   <- function(parent, ...) tkwidget(parent, "ttk::separator", ...)

# Non-Widget commands
"tk2tip" <- function(widget, message) tcl("set_balloon", widget, message)

"tk2killtip" <- function() tcl("kill_balloon")

"tk2notetraverse" <- function(nb) tcl("tile::notebook::enableTraversal", nb)

"tk2column" <-
function(widget, action = c("add", "configure", "delete", "names", "cget", "nearest"), ...) {
    Action <- action[1]
    tcl(widget, "column", Action, ...)
}

"tk2insert.multi" <-
function(widget, where = "end", items) {
    # We insert one or several lines in a multicolumn widget
    items <- as.matrix(items)
    # A vector is coerced into a column matrix and we want a row matrix here
    if (ncol(items) == 1) items <- t(items)
    # Convert the matrix into [list {el1} {el2} {el3}] [list {el4}, {el5}, {el6}], ...
    makeTclList <- function(x) paste("[list {", paste(x, collapse = "} {"), "}]", sep = "")
    TclList <- paste(apply(items, 1, makeTclList), collapse = "\\\n")
    .Tcl(paste(widget, "insert", where, TclList))
}

# TO DO: I have now a different combobox, and need to change this function accordingly!
"tk2listinsert" <- function(widget, ...) tcl(widget, "values", ...)

# Themes management
"tk2theme.elements" <- function() as.character(.Tcl("style element names"))

"tk2theme.list" <- function() as.character(.Tcl("style theme names"))

"tk2theme" <- function(theme = NULL) {
    if (is.null(theme)) { # Get it
        res <- getOption("tk2theme")
    } else { # Set it to theme
        .Tcl(paste("style theme use", theme))
        # And save current theme in option "tk2theme"
        options(tk2theme = theme)
        res <- theme
    }
    return(res)
}
