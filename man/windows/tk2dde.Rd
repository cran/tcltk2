\name{tk2dde}
\alias{tk2dde}
\alias{tk2dde.exec}
\alias{tk2dde.poke}
\alias{tk2dde.request}
\alias{tk2dde.services}
\alias{.tk2dde.require}

\title{ Use DDE (Dynamic Data Exchange) under Windows }
\description{
  DDE is the first Microsoft's attempt to make an inter-application mechanism.
  It is now superseeded by (D)Com, but it is still available. Being simpler than
  Com, DDE is interesting for simple tasks. Applications like Word, Excel, Windows
  Explorer, Internet Explorer, etc... provide services one can access through DDE
  (see examples).
}
\usage{
    tk2dde(topic = NULL)
    tk2dde.services(service = "", topic = "")
    tk2dde.exec(service, topic, command, async = FALSE)
    tk2dde.poke(service, topic, item, data)
    tk2dde.request(service, topic, item, binary = FALSE)
    .tk2dde.require()
}

\arguments{
  \item{topic}{ The 'topic' to reach or expose. A DDE server is accessed as
    'service'|'topic'. In the case of \code{tk2DDE()}, a non null topic activates
    the DDE server, and a null topic deactivate it }
  \item{service}{ The name of the service to reach. In \code{tk2DDEservices}, if
    both service and topic are empty, the list of all available DDE service is
    returned, otherwise, only available topics for a given service are listed }
  \item{command}{ A string with the command to run in the external application
    (syntax depends on the server) }
  \item{async}{ Is a command run asynchroneously (returns immediatelly, before the
    command is processed), or not? }
  \item{item}{ The concerned item (usually a variable name, a range in a worksheet,
    etc...) }
  \item{data}{ The new value for the item }
  \item{binary}{ Should the return bez treated as binary data or not? }
}

\author{ Philippe Grosjean }
\note{ This is only available under Windows. Trying to use these functions under
  other platforms raises an error. Under Windows, R is automatically configured
  as a DDE server with name TclEval|SciViewsR when this package is loaded }

\seealso{ \code{\link{tk2reg}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	## Examples of DDE - Windows only
    ###### Examples using wish
    # Start a Wish84 console side-by-side with R.
    # (to get wish, you need to install ActiveTcl from
    # http://www.activestate.com/Products/ActiveTcl/)
    # Once it is done, start 'Wish84' from the start menu)
    # Register the Wish console as a DDE server, that is, type in it:
    # \% package require dde
    # \% dde servername wish

    # In R:
    tk2dde("R") # Return 0 if succeed
    tk2dde.services()
    # Evaluate some string in wish
    tk2dde.exec("TclEval", "wish", "{puts {Hello World!}}")
    # Give a value to a variable in wish
    tk2dde.poke("TclEval", "wish", "myvar", "This is a string!")
    tk2dde.poke("TclEval", "wish", "mynumvar", 34.56)

    # In wish, check that vars exist and have correct value
    # \% puts $myvar
    # \% puts $mynumvar

    # Get the value of one variable from wish into R
    tk2dde.request("TclEval", "wish", "myvar")
    tk2dde.request("TclEval", "wish", "mynumvar")
    # Note that you don't know here if it is a string, a numerical data, or so...
    # You have to know and convert yourself!

    # Now, the other way: execute a R function from wish
    # You first need to register a R function for callback
    # (I don't how yet to deal with arguments here, so, use functions without args!)
    doDDE <- function() cat("DDE execute!\\n")	# A simple function without arguments
    tclFun(doDDE)
    # And in wish
    # \% dde execute TclEval R doDDE

    # Once you have defined a variable using tclVar, you can get or change its value
    #from the dde server
    # However, tclVar gives cryptic names like ::RTcl1. so we prefer to use tclVarname
    myvar <- tclVarname("myvar", "this is a test...")
    tclvalue(myvar) # This is the way we access to this variable in R

    # In wish you get the value and change it:
    # \% dde request TclEval R myvar
    # Again, dde poke does not work here and must be replaced by an execute command
    # This doesn't work (???)
    # \% dde poke TclEval R myvar {yes! and it works...}
    # ... but this is fine
    # \% dde execute TclEval R {set myvar {yes! and it works...}}

    # And in R...
    tclvalue(myvar)

    # You can also change the value of a variable, or run a command in R from
    #the command line using execdde.exe:
    # Copy \%R_Home\%\\library\\tcltk2\\libs\\execdde.exe somewhere in your path,
    # start a DOS window
    # and enter the following commands:
    # > execdde -s TclEval -t R -c doDDE > NUL
    # > if errorlevel 1 echo An error occurred... branch accordingly in your batch file!
    # > execdde -s TclEval -t R -c "set myvar {ok from execdde!}" > NUL

    # And in R:
    tclvalue(myvar)
    # Note that, thanks to separate event loops, this work also when R is calculating...

    # Other examples of DDE use:
    #### Manipulating Microsoft Excel ####
    # Start Excel with a blank workbook, then...

    # Change values in Excel from R:
    tk2dde.poke("Excel", "Sheet1", "R1C1:R2C1", "{5.7\\n6.34}")   # Some data
    tk2dde.poke("Excel", "Sheet1", "R3C1", "{= A1 + A2}")        #... and a formula

    # Read values in Excel (note that results of formulas are returned)
    Res <- tk2dde.request("Excel", "Sheet1", "R1C1:R3C1")
    Res
    as.numeric(strsplit(Res, "\\r\\n", fixed = TRUE)[[1]])        # Convertion

    # Execute a command in Excel
    tk2dde.exec("Excel", "Sheet1", "{\[\Select(\"R1C1:R3C1\")]\[New(2,2)\]}")
    # New(2,2) create a bar graph in a separate sheet
    # Note: close this graph now, in order to run the remaining commands properly!

    # R as server and Excel as client
    # Prepare R as server
    tk2dde("SciViewsR") # Now, my server name is TclEval|SciViewsR !
    # Create a variable that will be shared with Excel
    XlVar <- tclVarname("XlVar", "this is a test!")
    # Enter a formula somewhere in Excel to link to this variable
    tk2dde.poke("Excel", "Sheet1", "R5C1", "=TclEval|SciViewsR!'::XlVar'")

    # Now, change the content in XlVar
    tclvalue(XlVar) <- "Another text..."
    # Rem: in my Excel, the content of the cell is not changed automatically...
    # I must reenter the formula for the changes to be considered!?


    #### Manipulating Microsoft Word ####
    # Start Word and then...
    tk2dde.exec("Winword", "System", "{\[EndOfDocument\]}")
    tk2dde.exec("Winword", "System", "{\[Insert \"Text from Tcl.\"\]}", async = TRUE)

    # Can also use:
    #  AppMaximize 1
    #  StatOfDocument
    #  EditFind .Find="hello" .PatternMatch=1
    #  FileExit 2
    #  FileClose 2 # 2 = close without saving, 1 = save first, 0 = prompt

    # If you need to start the application (Word) before using DDE, you must
    # wait that Word becomes ready!
    # This is a Tcl proc that shows how to do that
    #### TO DO: translate this in R...
    # proc doc2txt fn {
    #     package require dde
    #     package require Tk; # because we need [selection]
    #     eval exec [auto_execok start] [list $fn] &
    #
    #     #Loop to wait until Word is really there, and ready to talk
    #     set word ""
    #     while {$word==""} {
    # 	set word [dde services Winword System]
    # 	after 200
    #     }
    #     after 1000 ;# wait for the window to load...
    #     dde execute Winword System {[EditSelectAll]}
    #     dde execute Winword System {[EditCopy]}
    #     set res [selection get -selection CLIPBOARD]
    #     dde execute Winword System {[FileExit 2]}
    #     set res
    # }
    # doc2txt {C:\Temp\Test.doc}

    #### Control Matlab (don't work with Rel 12?!)
    # Start Matlab and then...
    tk2dde.exec("Matlab", "Engine", "{s = ones(5);}")
    # Then in Matlab, type s


    #### Manipulating Progman to create and delete shortcuts ####
    tk2dde.exec("progman", "progman", "{\[CreateGroup(Bogus)\]}")
    tk2dde.exec("progman", "progman", "{\[AddItem(notepad.exe,BogusPadLink)\]}")
    tk2dde.exec("progman", "progman", "{\[ShowGroup(Bogus,0)\]}")
    tk2dde.exec("progman", "progman", "{\[ShowGroup(Bogus,1)\]}")
    # And delete it...
    tk2dde.exec("progman", "progman", "{\[DeleteItem(BogusPadLink)\]}")
    tk2dde.exec("progman", "progman", "{\[DeleteGroup(Bogus)\]}")
    ## Rem: the same can be done with tcom, if installed:
    #.Tcl("package require tcom")
    #.Tcl("set sh [::tcom::ref createobject {WScript.Shell}]")
    #.Tcl("set lnk [$sh CreateShortcut {c:\\\\temp\\\\boguspad.lnk}]")
    #.Tcl("$lnk TargetPath {\"notepad.exe\"}")
    #.Tcl("$lnk WorkingDirectory {c:\\\\temp}")
    #.Tcl("$lnk Arguments Tutorial.txt")
    #.Tcl("$lnk Save")


    ## Controlling Internet Explorer through DDE
    # Start Internet Explorer, then...
    tk2dde.exec("iexplore", "WWW_OpenURL", "{http://www.sciviews.org/}")
    # It works, but return an error in R???
    tk2dde.exec("iexplore", "WWW_ShowFile", "{C:/progra~1/R/rw2001/doc/html/index.html}")
    # Idem
    # Info on the current opened window
    tk2dde.request("iexplore", "WWW_GetWindowInfo", "1")
    # and this does not work!)
    # tk2dde.request("iexplore", "WWW_ListWindows", "0")


    ## Controlling Windows explorer
    tk2dde.exec("Folders", "AppProperties", "{\[ViewFolder(\"C:\\Temp\",\"C:\\Temp\",5)\]}")
    #... or you can also try 'ExplodeFolder'
    
    # Search in folder
    tk2dde.exec("Folders", "AppProperties", "{\[FindFolder(\"\",\"C:\\Temp\")\]}")
  }
}
\keyword{ utilities }
\keyword{ IO }
