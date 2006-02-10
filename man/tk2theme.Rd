\name{tk2theme}
\alias{tk2theme}
\alias{tk2theme.list}
\alias{tk2theme.elements}

\title{ Set or get the theme in use for themed widgets (tile) }
\description{
  In tcltk2, we propose a replacement for standard tcltk widgets. These new widgets
  can be "themed", that is, adapted to take various visual appearances. It can get
  the visual appearance of various Unix/Linux systems, of Windows (including XP)
  and of Aqua under MacOS. Custom themes can also be defined, or imported from GTK.
}
\usage{
  tk2theme(theme = NULL)
  tk2theme.list()
  tk2theme.elements()
}

\arguments{
  \item{theme}{ The new theme to use }
}

\value{
  A list of available themes for \code{tk2therme.list()} or of theme elements
  for \code{tk2theme.elements()}
}

\author{ Philippe Grosjean }

\note{ Themes are provided by the tk package Tile (currently in version 0.6.2).
  It is automatically installed under Windows, but for other platforms, you must
  download and compile Tile yourself (http://tktable.sourceforge.net/tile/) }

\seealso{ \code{\link{tk2widgets}}, \code{\link{tk2tip}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	## Test all themes one after the other on the demo window
    tk2demo <- function() {
     	tt <- tktoplevel()
     	tktitle(tt) <- "tk2 demo"

     	## Change icon (under Windows)
     	Info <- tk2ico.load(res = "information")
    	tk2ico.set(tt, Info)
    	tk2ico.destroy(Info)

     	## Add a menu with quit and all themes
     	topMenu <- tk2menu(tt)
    	tkconfigure(tt, menu = topMenu)
    	fileMenu <- tk2menu(topMenu, tearoff = FALSE)
    	tkadd(fileMenu, "command", label = "Quit",
    		command = function() tkdestroy(tt))
    	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
    	Themes <- sort(tk2theme.list())
    	themeMenu <- tk2menu(topMenu, tearoff = FALSE)
    	for (i in 1:length(Themes)) {
    		tkadd(themeMenu, "command", label = Themes[i],
    			command = eval(parse(text = paste("function() tk2theme(\"", Themes[i], "\")",
                    sep = ""))))
    	}
    	tkadd(topMenu, "cascade", label = "Theme", menu = themeMenu)

    	#### TO DO: Add a toolbar

    	## Add a notebook with three tabs
    	nb <- tk2notebook(tt)
    	tcli1 <- tk2frame(nb)
    	tkadd(nb, tcli1, text = "Tab 1")
    	tcli2 <- tk2label(nb, text = "Nothing here.")
    	tkadd(nb, tcli2, text = "Tab 2")
    	tcli3 <- tk2label(nb, text = "Nothing here either.")
    	tkadd(nb, tcli3, text = "Tab 3")
    	tkselect(nb, tcli1)
    	tkgrid(nb)

    	## Add a labelframe with new tile widgets
    	tfrm1 <- tk2labelframe(tcli1, text = "tile widgets")
    	tkgrid(tfrm1)

    	# Add a label
    	tlab <- tk2label(tfrm1, text = "This is a label... hover me to see a tooltip")
    	tkgrid(tlab)
    	# Define a tooltip for the label
    	tk2tip(tlab, "A tooltip for the label \ndisplayed on two lines")

    	# Add a button
    	but <- tk2button(tfrm1, text = "Click me", width = 12,
    		command = function() tkmessageBox(title = "tk2 demo",
    			message = "Hello, world!", icon = "info", type = "ok"))
    	tkgrid(but)

    	# Add a checkbutton
    	tcb <- tk2checkbutton(tfrm1, text = " A check button")
    	tkgrid(tcb)

    	# Add radio buttons
    	rbValue <- tclVar("oranges")
    	rb1 <- tk2radiobutton(tfrm1, text = " Apples", variable = rbValue,
    		value = "apples")
    	rb2 <- tk2radiobutton(tfrm1, text = " Oranges", variable = rbValue,
    		value = "oranges")
    	tkgrid(rb1, sticky = "w")
    	tkgrid(rb2, sticky = "w")

    	# Add a progressbar
    	tprog <- tk2progress(tfrm1, orient = "horizontal", from = 0,  to = 100)
    	tkgrid(tprog)
    	tkset(tprog, 0.5)

    	# Add a slider #### TO DO: fix command!
    	tscal <- tk2scale(tfrm1, orient = "horizontal", from = 0, to = 100,
    		command = tkset(tprog, 0.2))
    	tkgrid(tscal)

    	## Add a labelframe with additional widgets
    	tfrm2 <- tk2labelframe(tcli1, text = "additional widgets", width = 50)
    	tkgrid(tfrm2)

    	# Add a combobox
    	cb <- tk2combobox(tfrm2, value = "Banana")
    	tkgrid(cb)
    	# Fill the combobox list
    	fruits <- c("Apple", "Orange", "Banana", "Pear")
        # TO DO: change this!!!
	    #for (i in (1:4))
        #    tk2listinsert(cb, "end", fruits[i])
	    tkinsert(cb, "end", "pear")

    	tkfocus(tt)
     	return(TRUE)
    }

    tk2demo()
    themes <- tk2theme.list()
    for (i in 1:length(themes)) {
    	tk2theme(themes[i])
    	Sys.sleep(2)
    }
  }
}
\keyword{ utilities }
