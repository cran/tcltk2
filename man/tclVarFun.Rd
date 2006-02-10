\name{tclVarFun}
\alias{makeTclNames}
\alias{tclFun}
\alias{tclGetValue}
\alias{tclSetValue}
\alias{tclVarExists}
\alias{tclVarFind}
\alias{tclVarname}

\title{ Manipulate R variables and functions from tcl and back }
\description{
  These functions are intended to provide a better "duality" between the name of
  variables in both R and tcl, including for function calls. It is possible to
  define a variable with the same name in R and tcl (the content is identical,
  but copied and coerced in the two respective environments). It is also possible
  to get the value of a tcl variable from R, and to call a R function from within
  tcl. These functionnalities are provided in the tcltk package, but Tcl variable
  usually have different internal names as R equivalents.
}
\usage{
  makeTclNames(names, unique = FALSE)
  tclFun(f, name = deparse(substitute(f)))
  tclGetValue(name)
  tclSetValue(name, value)
  tclVarExists(name)
  tclVarFind(pattern)
  tclVarname(name, init = "", keep.existing = TRUE)
}

\arguments{
  \item{names}{ Transform names so that they are valid for variables in tcl }
  \item{unique}{ Should these names be unique in the vector? }
  \item{f}{ An R function. currently, do no support functions with arguments. }
  \item{name}{ The name of a variable }
  \item{value}{ The value to place in a variable }
  \item{pattern}{ A pattern to search for }
  \item{init}{ Initial value to use when creating the variable }
  \item{keep.existing}{ If the tcl variable already exist, should we keep its content?  }
}

\details{
  These functions are similar to \code{tclVar()} from package tcltk, except for
  the following change: here, it is possible to propose a name for the created
  tcl variable, or to set or retrieve the content of a tcl variable that is not
  mirrored in R.
}

\value{
  Most of these functions return a \code{tclVar} object.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{tk2theme}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	## Tcl functions and variables manipulation
    tclVarExists("tcl_version")
    tclVarExists("probably_non_existant")
    tclVarFind("tcl*")

    # Using tclVarname() and tclGetValue()... intented for better match between R and Tcl variables
    Test <- tclVarname("Test", "this is a test!")
    # Now 'Test' exist both in R and in Tcl... In R, you need to use tclvalue(Test) to retrieve its content
    # If a variable already exists in Tcl, its content is preserved using keep.existing = TRUE
    .Tcl("set A_Variable {just to test...}") # Create a variable in Tcl and assign "just a test..." to it
    A_Variable <- tclVarname("A_Variable", "something else?") # Create the dual variable with same name
    tclvalue(A_Variable) # Content of the variable is not changed!

    # If you want to retrieve the content of a Tcl variable, but do not want to create a reference to it in R, use:
    .Tcl("set Another_Variable {something in it}")  # Create a Tcl variable, not visible from R
    tclGetValue("Another_Variable") # Get its content in R
  }
}
\keyword{ utilities }
