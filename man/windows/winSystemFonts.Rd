\name{winSystemFonts}
\alias{winSystemFonts}

\title{ Get the list of main Windows system fonts }
\description{
  The function returns a list of system fonts in string. They can be used
  directly to defines Tk fonts that match system fonts.
}
\usage{
winSystemFonts()
}

\value{
	A string with 5 components describing font used (1) for GUI elements,
	(2) for menus, (3) for the title of windows, (4) for the title of toolbar
	windows or column headers and (5) for status bar and tooltips.
}

\author{ Philippe Grosjean }

\note{ This is only available under Windows. }

\seealso{ \code{\link{tk2reg}} }

\examples{
winSystemFonts()
}

\keyword{ utilities }
