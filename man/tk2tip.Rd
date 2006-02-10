\name{tk2tip}
\alias{tk2tip}
\alias{tk2killtip}

\title{ Display and manage tooltips in tk widgets }
\description{
  \code{tk2tip} provides a simple mechanism to display tooltips on tk widgets when
  the mouse hoover on top of them.
}

\usage{
  tk2tip(widget, message)
  tk2killtip()
}

\arguments{
  \item{widget}{ The widget to which a tooltip is attached }
  \item{message}{ The message of the tooltip ("" to remove the tooltip) }
}

\author{ Philippe Grosjean }

\note{ This implementation is done in pure tcl code }

\seealso{ \code{\link{tk2widgets}}, \code{\link{tk2theme}} }
\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	tt2 <- tktoplevel()
    lb <- tk2label(tt2, text = "Move mouse over me, or over the button to see tooltip")
    tkgrid(lb)
    tk2tip(lb, "A tooltip for the label \ndisplayed on two lines")
    but <- tk2button(tt2, text = "OK", width = 10,
    	command = function() tkdestroy(tt2))
    tkgrid(but)
    tk2tip(but, "Another tooltip for the button")
    #### TO DO: show the use of tk2killtip()!
  }
}
\keyword{ utilities }
