\name{tk2widgets}
\alias{tk2widgets}
\alias{tk2button}
\alias{tk2canvas}
\alias{tk2checkbutton}
\alias{tk2combobox}
\alias{tk2entry}
\alias{tk2frame}
\alias{tk2label}
\alias{tk2labelframe}
\alias{tk2listbox}
\alias{tk2mclistbox}
\alias{tk2menu}
\alias{tk2menubutton}
\alias{tk2message}
\alias{tk2notebook}
\alias{tk2panedwindow}
\alias{tk2progress}
\alias{tk2radiobutton}
\alias{tk2scale}
\alias{tk2scrollbar}
\alias{tk2separator}
\alias{tk2spinbox}
\alias{tk2table}
\alias{tk2text}
\alias{tk2tree}
\alias{tk2treeview}

\title{ A themed series of tk widget }
\description{
  A series of widgets you can use in your tk windows/dialog boxes.
}
\usage{
  tk2button(parent, ...)
  tk2canvas(parent, ...)
  tk2checkbutton(parent, ...)
  tk2combobox(parent, ...)
  tk2entry(parent, ...)
  tk2frame(parent, ...)
  tk2label(parent, ...)
  tk2labelframe(parent, ...)
  tk2listbox(parent, ...)
  tk2mclistbox(parent, ...)
  tk2menu(parent, ...)
  tk2menubutton(parent, ...)
  tk2message(parent, ...)
  tk2notebook(parent, ...)
  tk2panedwindow(parent, ...)
  tk2progress(parent, ...)
  tk2radiobutton(parent, ...)
  tk2scale(parent, ...)
  tk2scrollbar(parent, ...)
  tk2separator(parent, ...)
  tk2spinbox(parent, ...)
  tk2table(parent, ...)
  tk2text(parent, ...)
  tk2tree(parent, ...)
  tk2treeview(parent, ...)
}

\arguments{
  \item{parent}{ The parent window }
  \item{\dots}{ Further arguments passed to the widget }
}

\value{
  The reference to the created widget.
}

\author{ Philippe Grosjean }
\note{ Most of these widgets comes from the tile (currently 0.4) library }

\seealso{ \code{\link{tk2theme}}, \code{\link{tk2tip}}, \code{\link{tk2commands}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	### A tk2notebook example
    tt2 <- tktoplevel()
    nb <- tk2notebook(tt2)
    tcli1 <- tk2label(nb, text = "Nothing here.")
    tkadd(nb, tcli1, text = "Test")
    tcli2 <- tk2frame(nb)
    but <- tk2button(tcli2, text = "Click me", command = function() tkdestroy(tt2))
    tkgrid(but)
    tkadd(nb, tcli2, text = "Button")
    tkselect(nb, tcli2)
    tkgrid(nb)

    ## A simple tk2panedwindow example
    tt2 <- tktoplevel()
    pw <- tk2panedwindow(tt2, orient = "vertical")
    lpw.1 <- tk2label(pw, text = "Panel 1")
    lpw.2 <- tk2label(pw, text = "Panel2")
    tkadd(pw, lpw.1, minsize = 100)
    tkadd(pw, lpw.2, minsize = 70)
    but <- tk2button(lpw.2, text = "OK", width = 10,
    	command = function() tkdestroy(tt2))
    tkgrid(but)
    tkpack(pw, fill = "both", expand = "yes")
    # Resize the window and move the panel separator with the mouse

    ## A tk2combobox example
    tt2 <- tktoplevel()
    cb <- tk2combobox(tt2)
    tkgrid(cb)
    # Fill the combobox list
    fruits <- c("Apple", "Orange", "Banana", "Pear")
	# TO DO: change this!!!
	#for (i in (1:4))
    #    tk2listinsert(cb, "end", fruits[i])
	tkinsert(cb, "end", "pear")
	#tkconfigure(cb, value = "Pear")	# Set the current value for the combobox
    # or better: link it to a variable
    Fruit <- tclVar("Pear")
    tkconfigure(cb, textvariable = Fruit)
    # Create a button to get the content of the combobox
    but <- tk2button(tt2, text = "OK", width = 10,
    	command = function() {tkdestroy(tt2); cat(tclvalue(Fruit), "\n")})
    tkgrid(but)

    ### An example of a tk2spinbox widget
    tt2 <- tktoplevel()
    tspin <- tk2spinbox(tt2, relief = "groove", from = 2, to = 20, increment = 2)
    tkgrid(tspin)
    #tdial <- tk2dial(tt2, from = 0, to = 20, resolution = 0.5, width = 70,
    #	tickinterval = 2)
    #tkgrid(tdial)
    tbut <- tk2button(tt2, text = "OK", width = 10,
    	command = function() tkdestroy(tt2))
    tkgrid(tbut)

    ## A tk2mclistbox example
    tt2 <- tktoplevel()
    mlb <- tk2mclistbox(tt2, width = 55, resizablecolumns = TRUE)
    # Define the columns
    tk2column(mlb, "add", "name", label = "First name", width = 20)
    tk2column(mlb, "add", "lastname", label = "Last name", width = 20)
    tk2column(mlb, "add", "org", label = "Organisation", width = 15)
    tkgrid(mlb)
    # Fill the multicolumn list (we can use a vector, or a matrix of character strings)
    item1 <- c("Bryan", "Oackley", "ChannelPoint")
    items <- matrix(c("John", "Ousterhout", "Scriptics",
                      "Steve", "Miller", "TclTk inc."), ncol = 3, byrow = TRUE)
    tk2insert.multi(mlb, "end", item1)
    tk2insert.multi(mlb, "end", items)
    #### TO DO: bind events
    # Ex: .listbox label bind date <ButtonPress-1> "sortByDate %W"
    # See the example.tcl in .\libs\mclistbox1.02 for a more complex example
    # Create a button to close the dialog box
    but <- tk2button(tt2, text = "OK", width = 10,
    	command = function() tkdestroy(tt2))
    tkgrid(but)

    ### A simple tk2table example
    myRarray <- c("Animal", "\"sphinx moth\"", "oyster",
                  "Type", "insect", "mollusk")
    dim(myRarray) <- c(3, 2)
    for (i in (0:2))
      for (j in (0:1))
         .Tcl(paste("set tclarray(", i, ",", j, ") ", myRarray[i+1, j+1], sep = ""))
    tt2 <- tktoplevel()
    table1 <- tk2table(tt2, variable = "tclarray", rows = "3",
    	cols = "2", titlerows = "1", selectmode = "extended", colwidth = "25",
    	background = "white")
    tkpack(table1)
  }
}

\keyword{ utilities }
