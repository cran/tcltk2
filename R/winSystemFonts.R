# winSystemFonts.R - Get Windows system fonts
# Copyright (c), Philippe Grosjean (phgrosjean@sciviews.org)
# Licensed under LGPL 3 or above
#
# Changes:
# - 2007-01-11: fisrt version (for tcltk2_1.0-0)
#
# To do:
# - Implement this as a DLL instead

winSystemFonts <- function() {
    if (.Platform$OS.type != "windows") return(NULL)
	###TODO: implement this as a DLL
	#res <- .C('winSystemFonts', out = character(0))$out
	# Look for a place to find the binary file
	libs <- .libPaths()
	for (lib in libs) {
		sysfontfile <- file.path(lib, "tcltk2", "bin", "SystemFonts.exe")			
		if (file.exists(sysfontfile)) {
			res <- system(sysfontfile, intern = TRUE, invisible = TRUE)
			break
		}
	}
	names(res) <- c("default", "menu", "caption", "smallcaption", "status")
	return(res) 
}
