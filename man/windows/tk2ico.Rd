\name{tk2ico}
\alias{tk2ico}
\alias{tk2ico.create}
\alias{tk2ico.destroy}
\alias{tk2ico.hicon}
\alias{tk2ico.info}
\alias{tk2ico.load}
\alias{tk2ico.pos}
\alias{tk2ico.set}
\alias{tk2ico.taskbar.add}
\alias{tk2ico.taskbar.delete}
\alias{tk2ico.taskbar.modify}
\alias{tk2ico.text}

\title{ Manipulate icons under Windows }
\description{
  Create, load and work with Windows icons. Change icons fo Windows, use icons in
  the taskbar under Windows 9X/2000/XP, ... These function are only useful for Windows.
}

\usage{
  tk2ico.create(icofile)
  tk2ico.destroy(icon)
  tk2ico.hicon(icon)
  tk2ico.info(icon, convert = TRUE)
  tk2ico.load(file = "shell32.dll", res = "application")
  tk2ico.pos(icon, pos)
  tk2ico.set(win, icon, pos = NULL, type = c("all", "small", "big"))
  tk2ico.text(icon, text = NULL)
  tk2ico.taskbar.add(icon, pos = 0, text = tk2ico.text(icon), callback = NULL)
  tk2ico.taskbar.delete(icon)
  tk2ico.taskbar.modify(icon, pos = NULL, text = NULL)

}

\arguments{
  \item{icofile}{ A file with a .ico extension, containing one or more Windows icons }
  \item{icon}{ An icon object }
  \item{convert}{ Do we convert the result into a data.frame? }
  \item{file}{ A file having icon resources (.exe, or .dll) }
  \item{res}{ The name of the resource from where the icon should be extracted }
  \item{pos}{ A position (starting from 0) pointing to an icon in a multi-icon object }
  \item{win}{ A tk window whose icon is changed }
  \item{type}{ Do we change only the small, the large, or both icons? }
  \item{text}{ Change a text for an icon }
  \item{callback}{ Give a callback function called when the user clicks in the taskbar icon }
}

\value{
  An icon object, which is a reference to an image resource in tcl. Do not forget to
  destroy it using \code{tk2ico.destroy()} when you do not need it any more!
}

\author{ Philippe Grosjean }
\note{ This is Windows-specific. It uses the winico 0.5 Tcl library. Under other platforms
  these functions just return \code{NULL} without error or warning. So, code that must
  run on different platforms could use these functions all the time, as soon as they
  correctly deal with possible \code{NULL} return. }

\seealso{ \code{\link{tk2dde}}, \code{\link{tk2reg}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	## Examples of tk2ico - icon manipulation under Windows
    tt2 <- tktoplevel()
    # Load a system icon (there are: "application", "asterisk", "error",
    # "exclamation", "hand", "question", "information", "warning", and "winlogo".
    Warn <- tk2ico.load(res = "warning")
    tk2ico.info(Warn)
    # Change the text
    tk2ico.text(Warn)
    tk2ico.text(Warn, "Warning icon")
    tk2ico.text(Warn)
    # Get hicon
    tk2ico.hicon(Warn)
    # Change the icon of my window tt2
    tk2ico.set(tt2, Warn)
    # Do not forget to destroy icon to free resource when not needed any more
    tk2ico.destroy(Warn)
    rm(Warn)
    # Load an icon from the resource section of an exe, or dll
    Rico <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.info(Rico)
    tk2ico.set(tt2, Rico)
    tk2ico.destroy(Rico)
    rm(Rico)

    # Load one or several icons from an .ico file
    libdir <- file.path(.path.package(package="tcltk2")[1], "etc")
    SVico <- tk2ico.create(file.path(libdir, "SciViews.ico"))
    # The various resolution icons created from this file
    tk2ico.info(SVico)
    # Change the default position to 1
    tk2ico.pos(SVico, 1)
    # Change only the small icon (use Alt-Tab to see the large one)
    tk2ico.set(tt2, SVico, pos = 0, type = "small")

    ### This does not work!
    ## Set a taskbar icon (rem: should use callback usually)
    #tk2ico.taskbar.add(SVico, pos = 0, text = "{my tcltk taskbar icon}")
    ## Change its text...
    #tk2ico.tackbar.modify(SVico, text = "{text changed!}")
    ##... and delete it
    #tk2ico.taskbar.delete(SVico)

    # Destroy the icons
    tk2ico.destroy(SVico)
    rm(SVico)

    # When done, dispose of the window
    tkdestroy(tt2)
  }
}
\keyword{ utilities }
