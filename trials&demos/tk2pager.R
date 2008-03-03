# An enhanced tkPager
tk2pagerLink <- function(pagertabs, link) {
	# Display a new page in the 'win' pager, whatever the currently active one
	require("svMisc")
	currentpager <- getTemp(".pager")
	assignTemp(".pager", pagertabs)
	on.exit(assignTemp(".pager", currentpager))
	# 'link' can be:
	# 1) <<R Documentation>> => help(lib.loc = NULL)
	if (link == "<<R Documentation>>") {
		help(lib.loc = NULL, pager = function(file) print(file))
	# 2) <<XXX>> => help(package = XXX)
	} else if ((link2 <- sub("^<<(.+)>>$", "\\1", link)) != link) {
		help(package = link2, pager = mypager)
	# 3) ''YYY'' => help(YYY, try.all.package = TRUE) after possibly removing trailing ()
	} else {
		link2 <- sub("^''([^(]+)([(].*[)])?''$", link)
		help(link2, try.all.packages = TRUE, pager = function(file) mypager(file)) 
	}
}

mypager <- function(file, header, title, delete.file) cat(header, "\n")

tk2pagerLink(".1", "<<R Documentation>>")

tk2pager <- function (file, header = basename(file), title = "R help",
	delete.file = FALSE, type = c("Rhelp", "Rcode", "Ccode", "text"),
	pager = getTemp(".pager")) {
	
	### TODO: about R => points to all the pages given in HTML help
	### TODO: grayed background to show that it is not editable?
	require("svMisc")
	require("tcltk2")
	
	oldmode <- tclServiceMode(FALSE)
    on.exit({tclServiceMode(oldmode)
		# Under Windows, change the icon of the pager window
		Rico <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
     	try(tk2ico.set(pager, Rico), silent = TRUE)
     	tk2ico.destroy(Rico)})
	
	### TODO: do not add twice the same file!
	
	# Do we add one or several pages to an existing pager?
    if (!is.null(pager)) {
		if (!inherits(pager, "tkpager"))
			stop("'pager' must be a \"tkpager\" object, or NULL")
		if (is.null(pager$tabs))
			stop("'tabs' not found for this pager")
		if (!as.logical(tkwinfo( "exists", getTemp(".pager")))) {
			# The pager window was destroyed
			rmTemp(".pager")
			pager <- NULL
		}
	}	
	if (is.null(pager)) {	# Create a new page window
		pager <- tktoplevel()
		pager$tabs <-	tk2notebook(pager)
		tkpack(pager$tabs, fill = "both", expand = TRUE)
		class(pager) <- c("tkwin", "tkpager")
		assignTemp(".pager", pager)	# Save a link to it in tempEnv
		### TODO: hook to a callback that deletes the .pager variable!...
		# Currently, this is solved differently!
		
		# Add a menu to the pager
		### TODO: a context menu too
		### TODO: use gettext!!!
		save <- function() {
			file <- tclvalue(tkgetSaveFile(
				initialfile = tclvalue(tclfile.tail(getfile())),
				initialdir = tclvalue(tclfile.dir(getfile()))))
			if (length(file) == 0 || file == "") return()
			chn <- tclopen(file, "w")
			tclputs(chn, tclvalue(tkget(pager$tabs, "0.0", "end")))
			tclclose(chn)
		}
		print <- function() {
			### TODO: under Unix: 'exec lpr filename'
			# Under DOS (default printer): COPY /b d:\path\to\myfile.txt prn
			# =>
			# exec $env(COMSPEC) /c type [file attribute d:\path\to\myfile.txt -shortname] > prn
			# Another way (with utf-8 file)
#proc print w {
#    if {$::tcl_platform(platform) ne "windows"} {
#        error "printing is only supported under Windows"
#    }
#    set filename c:/temp/[file tail [wm title .]]
#    set f [open $filename w]
#    fconfigure $f -encoding utf-8
#    puts $f \ufeff[$w get 1.0 end]
#    close $f
#    exec cmd /c start /min notepad /p $filename
#    file delete $filename
# }
#						
		}
		exit <- function() tkdestroy(pager)
		topMenu <- tkmenu(pager)
		tkconfigure(pager, menu = topMenu)
		fileMenu <- tkmenu(topMenu, tearoff = FALSE)
		tkadd(fileMenu, "command", label = "Print...", command = print)
		tkadd(fileMenu, "command", label = "Save in the file...", command = save)
		tkadd(fileMenu, "separator")
		tkadd(fileMenu, "command", label = "Close", command = exit)
		tkadd(topMenu, "cascade", label= "File", menu = fileMenu)
	}
	tktitle(pager) <- title
	
	# This function adds a page to the pager
	add.page <- function(pager, zfile, zheader, type) {
		frm <- tkframe(pager$tabs)
		txt <- tk2ctext(frm, relief = "flat", linemapbg = "gray90", height = 40,
			wrap = "word", font = "TkFixedFont")
		scr <- tkscrollbar(frm, command = function(...) tkyview(txt, ...))
		tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
		tkpack(scr, side = "right", fill = "y")
		tkpack(txt, fill = "both", expand = TRUE)
		tkadd(pager$tabs, frm, text = zheader)
		tkselect(pager$tabs, frm)
		res <- switch(type,
			Rhelp = {tcl("ctext::HighlightRhelp", txt)
					 tkconfigure(txt, linemap = FALSE)},
			Rcode = tcl("ctext::HighlightR", txt),
			Ccode = tcl("ctext::HighlightC", txt),
			text  = tkconfigure(txt, linemap = FALSE),
			tkconfigure(txt, linemap = FALSE))
		# Read the file and insert in in the text
		if (!file.exists(zfile)) {
			tkinsert(txt, "end", paste("File '", zfile, "' not found!", sep = ""))
		} else {
			if (type == "RhelpDD") {
				# A special formatting for R help files
				chn <- tcl("open", zfile)
        		text <- tclvalue(tcl("read", chn))
        		tcl("close", chn)
        		text <- strsplit(text, "\n")[[1]]
				# Split into different sections
        		sectionstart <- grep("^_\\b", text)
        		text <- gsub("_\b", "", text)
				
				# Add tabulations in front of the title
				tstart <- sectionstart[1]
				tend <-  grep("^$", text[-2])[1]	# Before the second enpty line
				text[tstart:tend] <- paste("\t", text[tstart:tend], sep = "")
				
				# Place the text
				text <- paste(text, collapse = "\n")
				tkinsert(txt, "end", text)
		#		tcl(txt, "fastinsert", "end", paste(text, collapse = "\n"))
				
				# Switch to R code highlighting
		#		tkconfigure(txt, highlight = 0)
		#		tcl("ctext::clearHighlightClasses", txt)
				tcl("ctext::HighlightR", txt)
				
				# Determine the location of the usage and examples sections
				usagestart <- grep("^Usage:$", text)
				if (length(usagestart) > 0) {
					usagestart = usagestart + 8
					# Do a syntax highlighting of this section
					usageend <- grep("^Arguments:$", text)
					if (length(usageend) > 0 && usageend > usagestart)
						tcl(txt, "highlight", paste(usagestart, 0, sep = "."),
							paste(usageend - 1, "end", sep = "."))
				}
				
				# Determine if there are examples, and highlight them too
				#examplestart <- regexpr("\nExamples:\n", text)
				#if (examplestart > 0)
				#	tcl(txt, "highlight", paste(examplestart + 11, "end")
				examplestart <- grep("^Examples:$", text)
				if (length(examplestart))
					tcl(txt, "highlight", paste(examplestart + 1, 0, sep = "."), "end")
				
#				# Place header and title
#				tkinsert(txt, "end", paste(text[1:(tend + 1)], collapse = "\n"))
#				
#				# Split the rest into sections and work sections one by one
#				sections <- split(text[-(1:(tend+1))], cut((tend+2):length(text),
#					breaks = c(sectionstart[-1] - 1, length(text))))
#				for (s in seq(along = sections)) {
#					S <- sections[[s]]
#					# If the section header is Usage: or Examples: => switch to R code
#					if (S[1] %in% c("Usage:", "Examples:")) {
#						cat("yee\n")
#						tkinsert(txt, "end", paste("\n", S[1], sep = ""))
#						# Switch temporarily to the R code highlighter
#						tcl("ctext::clearHighlightClasses", txt)
#						tcl("ctext::HighlightR")
#						# Insert the rest of this section
#						tkinsert(txt, "end", paste(c("", S[-1]), collapse = "\n"))
#						# And switch back to R help mode
#						tcl("ctext::clearHighlightClasses", txt)
#						tcl("ctext::HighlightRhelp")	
#					} else if (S[1] == "See Also:") {	# Special treatment for links
#						### TODO: make links for see also items
#						tkinsert(txt, "end", paste(c("", S), collapse = "\n"))
#					} else {	# Just insert the section
#						tkinsert(txt, "end", paste(c("", S), collapse = "\n"))
#					}
#				}
#				# Insert the various parts in turn and format them nicely
        		#text <- paste(c(header, "", title, "", text[-(1:4)]), collapse = "\n")
        		#tkinsert(txt, "end", text)
			} else {
				chn <- tcl("open", zfile)
        		# We keep this, just in case it would be a R help-like page
				tkinsert(txt, "end", gsub("_\b", "", tclvalue(tcl("read", chn))))
        		tcl("close", chn)
        	}
        }
		tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        #tkfocus(txt)
        ### TODO: remember the pointer to the ctext element
	}
	# Make sure that header is recycled along files
	header <- rep(header, length.out = length(file))
	for (i in seq(along = file)) {
        zfile <- file[[i]]
		add.page(pager, zfile, header[[i]], type[1])
        if (delete.file) tcl("file", "delete", zfile)
    }
}
setwd("c:/temp")
tk2pager("nlgc")
#tk2pager(c("nonexistant", "codegen"))
