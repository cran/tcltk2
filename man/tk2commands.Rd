\name{tk2commands}
\alias{tk2commands}
\alias{tk2column}
\alias{tk2insert.multi}
\alias{tk2listinsert}
\alias{tk2notetraverse}

\title{ Tk commands associated with the tk2XXX widgets }
\description{
  These commands supplement those available in the tcltk package to ease manipulation
  of tk2XXX widgets.
}
\usage{
  tk2column(widget, action = c("add", "configure", "delete", "names", "cget", "nearest"), ...)
  tk2insert.multi(widget, where = "end", items)
  tk2listinsert(widget, ...)
  tk2notetraverse(nb)
}

\arguments{
  \item{widget}{ The widget to which these actions apply }
  \item{action}{ Which kind of action? }
  \item{where}{ Where are these item added in the list (by default, at the end) }
  \item{items}{ The items to add (either a vector for a single line, or a matrix for more items) }
  \item{nb}{ A tk2notebook widget }
  \item{\dots}{ Further arguments to the action }
}
\details{
  \code{tk2column} manipulate columns of a tk2mclistbox widget,
  \code{tk2insert.multi} is used to insert multiple field entries in a tk2mclistbox widget,
  \code{tk2listinsert} eases the insertion of items in a tk2combobox widget,
  \code{tk2notetraverse} helps manage the tabs in a tk2notebook.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{tk2widgets}}, \code{\link{tk2theme}}, \code{\link{tk2tip}} }

\keyword{ utilities }
