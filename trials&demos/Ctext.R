# R script editor using ctext
tkscript <- function() {
	library(tcltk2)
	tclRequire("ctext")
	
	tt <- tktoplevel()
	tktitle(tt) <- "R editor"
	frm <- tkframe(tt)
	txt <- tkwidget(frm, "ctext", height = 40, relief = "flat",
		linemapbg = "gray90", wrap = "word")
	scr <- tkscrollbar(frm, command = function(...) tkyview(txt, ...))
	tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
	tkpack(scr, side = "right", fill = "y")
	tkpack(txt, fill = "both", expand = TRUE)
	tkpack(frm, fill = "both", expand = TRUE)
	# Configure the ctext widget to highlight R code
	#.Tcl(paste("ctext::HighlightR ", txt$ID, " {lucida console} 9", sep = ""))	
	tcl("ctext::HighlightR", txt, "lucida console", 9)
	
	
	
	getfile <- function() {
		file <- tktitle(tt)
		file <- sub("R editor", "", file)
		file <- sub("^ - ", "", file)
		return(file)
	}
	save <- function() {
		file <- tclvalue(tkgetSaveFile(
			initialfile = tclvalue(tclfile.tail(getfile())),
			initialdir = tclvalue(tclfile.dir(getfile()))))
		if (length(file) == 0 || file == "") return()
		chn <- tclopen(file, "w")
		tclputs(chn, tclvalue(tkget(txt, "0.0", "end")))
		tclclose(chn)
		tktitle(tt) <- paste("R editor", file, sep = " - ")
	}
	load <- function() {
		file <- tclvalue(tkgetOpenFile(
			initialdir = tclvalue(tclfile.dir(getfile()))))
		if (length(file) == 0 || file == "") return()
		chn <- tclopen(file, "r")
		tkinsert(txt, "0.0", tclvalue(tclread(chn)))
		tclclose(chn)
		tktitle(tt) <- paste("R editor", file, sep = " - ")
	}
	changedir <- function() {
		dir <- tclvalue(tkchooseDirectory(initialdir = getwd()))
		if (length(dir) == 0 || dir == "") return()
		setwd(dir)
		cat("Working dir changed to:", dir, "\n")
	}
	exit <- function() tkdestroy(tt)
	run <- function() {
		code <- tclvalue(tkget(txt, "0.0", "end"))
		e <- try(parse(text = code))
		if (inherits(e, "try-error")) {
			tkmessageBox(message = "Syntax error", icon = "error")
			return()
		}
		cat("Executing from script window:", "-----", code, "result:", sep = "\n")
		print(eval(e))
	}
	topMenu <- tkmenu(tt)
	tkconfigure(tt, menu = topMenu)
	fileMenu <- tkmenu(topMenu, tearoff = FALSE)
	tkadd(fileMenu, "command", label = "Load", command = load)
	tkadd(fileMenu, "command", label = "Save", command = save)
	tkadd(fileMenu, "command", label = "Change dir", command = changedir)
	tkadd(fileMenu, "separator")
	tkadd(fileMenu, "command", label = "Exit", command = exit)
	tkadd(topMenu, "cascade", label= "File", menu = fileMenu)
	tkadd(topMenu, "command", label = "Run", command = run)
}
tkscript()
