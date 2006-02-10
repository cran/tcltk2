# last modified 14 January 2005 by Ph. Grosjean

".onLoad" <-
function(lib, pkg) {
    #require(tcltk) || stop("Package 'tcltk' is required!")
	libdir <- file.path(lib, "tcltk2", "tklibs")

    # Here we install the supplementary material in Tcl
    # extend the path
    res <- addTclPath(libdir)
    tclRequire("tile")      # Version 0.6
    # Not loaded automatically!
    #tclRequire("Img")       # Version 1.3
    #tclRequire("Tktable")   # Version 2.9
    #tclRequire("mclistbox")    # Version 1.02
    # This adds a balloon (tooltip)
    tcl("source", file.path(libdir, "balloon1.2", "balloon.tcl"))
    # This adds a tree widget
    tcl("source", file.path(libdir, "tree1.7", "tree.tcl"))
    # Windows only
    if (.Platform$OS.type == "windows") {
        tclRequire("dde")       # Version 1.2.2
        # Not loade automatically!
        #tclRequire("registry")  # Version 1.1.3
        ### Don't work!? tclRequire("winico")
        tcl("load", file.path(libdir, "winico0.5", "winico05.dll"))
        # Also register the DDE server as TclEval|SciViewsR
        tk2dde("SciViewsR")
    }

    # Set a default style, depending on the plateform
	# Rem: this should be now done automatically in tile 0.5 (old tile 0.4 code)
	themes <- as.character(.Tcl("style theme names"))
    theme <- "default"
    if (.Platform$OS.type == "windows") { # This is Windows
	   if ("xpnative" %in% themes) {
		  theme <- "xpnative"
	   } else if ("winnative" %in% themes) {
		  theme <- "winnative"
	   }
    } else if ("aqua" %in% themes) { # This should be MacOS
	   theme <- "aqua"
    } else {	# Use alt, a "revitalized" tk theme
	   theme <- "alt"
	   # Rem: one could also use step or clam
    }
    savedtheme <- getOption("tk2theme")
    if (!is.null(savedtheme)) theme <- savedtheme # Override default style!
    .Tcl(paste("style theme use", theme))
    # Remember the current theme (tcl command to retrieve it?)
    options(tk2theme = theme)
}

# TO DO: .onUnload() that close downloaded tk items (or unload tcltk?)
