# Tcl/Tk 8.4.13 (patchlevel)
## I need a diagnostic function, which tells what Tcl/Tk version is installed,
## what is available and what is missing!

# First, let's try to understand tcltk

# The objects are: tclVar, c(tclVar, tclArray), tclObj and tkwin

# When loading Tcl/Tk:
# - Under Windows, it looks for an environment variable MY_TCLTK to load
#   custom Tcl/Tk implementations, otherwise, it uses the one under R_Home/Tcl
# - It uses Tcl/Tk 8.4.13 in R 2.4.1
# - It defines the envir variable TCL_LIBRARY pointing to the tcl8.4 lib used
#   under Windows only (this is equivalent to .Tcl("info library")) 
# - It adds tcltk/exec dir to the Tcl path => TODO: look what's there!
# - it hides the root Tk window named '.' by using 'wm withdraw .'
# - Under Windows, the c code resets focus to the R console after loading tcltk


# Important: Tcl uses a separate event loop than the R command line. That way,
# Tcl/Tk still responds, even when R is calculating something from the command
# line...
### TODO: make a couple of nice demonstrations for this!

# Under Windows, Tcl is never unloaded + .Tcl("exit") crashes and .Tcl("destroy .")
# makes Tk unaccessible! => How do we unload Tcl when tcltk is unloaded?

# The basic function is .Tcl(): it evalues a string containing Tcl code
# It returns a tclObj object.
# Methods for tclObj:
# - Print(): prepend <Tcl> and print the content as text
# - tclvalue() extract the content as text
# - as.character(), as.integer(), as.double() to coerce Tcl object to R object
as.character(.Tcl('info loaded'))

# as.tclObj(x, drop=FALSE) converts from character, double, integer or logical to tclObj
as.tclObj(c("some string", "some other string"))
# is.tclObj(x) Tests if it is a tclObj
foo <- tclVar()
# Warning! It is only possible to modify a "tclObj" object when there is an
# underlying Tcl variable! It is completely extracted, converted, modified and
# converted back to Tcl!
tclObj(foo) <- c(pi,exp(1))
tclvalue(foo)
as.character(tclObj(foo))
as.double(tclObj(foo))
as.integer(tclObj(foo))	# Not truncated: gives NA NA!
as.logical(tclObj(foo))	# Gives NA NA too!


# .Tcl.args converts a named list of arguments (arg1=1, arg2="tata", arg3=NULL)
# into a Tcl argument syntax: -arg1 1 -arg2 tata, -arg3. Types are simply
# converted, but with tkwin, we get their ID and clallbacks are passed via the
# result of .Tcl.callback (tag=NULL is converted into -tag {})
# ".Tcl.callback() generates a script which invokes a dedicated R_call command taking
# the hex-encoded address of the function as the first argument. Simultaneously, 
# the function is analyzed and if it has formal arguments apart from ...; such 
# arguments are converted to %-substitutions" (Dalgaard, 2001 -DSC2001-)
#> .Tcl.callback(function(x,y)cat(x,y,"\n"))
#[1] "{ R_call 0x8369fdc %x %y }"

# Hence binding a scrollbar:
#txt <- tktext(tt)
#scr <- tkscrollbar(tt, command = function(...) tkyview(txt, ...))
#tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))

# And a different binding on mouse action:
#tt <- tktoplevel()
#tkbind(tt, "<Button-1>", function(x, y) cat(x, y, "\n"))

f <- function(x, y, all = TRUE) cat(x, "Hi!\n")
.Tcl.callback(f)
tt <- tktoplevel()
tt
.Tcl.args(text = "Push!", window = tt, command = f)

# Other forms of callbacks using expression(), allowing to use break
# If you want to bind to <Control-Return> and do not trigger the <Return> binding
# you have to break!!!
###??? But this is not TRUE? Return is not triggered when Control-Return is run!?
tkpack(txt <- tktext(tt <- tktoplevel()))
# The old style...
tkbind(txt, "<Return>", function() cat("Return!\n"))
tkbind(txt, "<Control-Return>", function() cat("Control-Return!\n"))
# Hit Control-Return, and see what happens... then:
tkbind(txt, "<Control-Return>", expression(cat("Control-Return!\n"), break))
# Hit Control-Return again
# When it is needed to pass arguments to the callback, you can use something like:
tkbind(txt, "<Control-Button-1>", expression(function(x,y) cat(x, y, "\n"),
	break))
tkdestroy(tt)

# tcl() is the most used command. It runs .Tcl.args.objv() and passes the result
# to .Tcl.objv()

### TODO: a word about event loops and show with sockets why it is so nice
# + an automatic analysis system based on a log file + after Tcl command
# (the later one must work better than currently!)

# Accessing Tcl variables from within R
# The object is "tclVar" created using tclVar() and value is accessed using tclvalue()
# and tclvalue<-(). On the Tcl side, the variables are called ::RTcl1, ::RTcl2, etc.
# The :: prefix means that they are global in Tcl. So:
foo <- tclVar("initial value")
tclvalue(foo)
tclvalue(foo) <- "Hello, World"
# "The "tclVar" objects are subject to finalization, so that when they go out of
# scope, the garbage collector will eventually remove their tcl counterparts.
# The as.character() method applied to a tclVar object gives its Tcl name
(Tclname <- as.character(foo))
.Tcl(paste("info exists", Tclname))
# This is better handled using tcl()
tcl("info", "exists", foo) 
# Now, delete 'foo'
rm(foo)
.Tcl(paste("info exists", Tclname)) # Still there
invisible(gc())
.Tcl(paste("info exists", Tclname)) # Gone!
# Now, what to do if we would like to have the same name in both R and Tcl?
### TODO: speak about functions in tcltk2


### TODO: Tkrplot and embedding R graphics

# Windows
# The root window, which is created when tcltk is loaded, but hidden immediately
# by .Tcl("wm . withdraw") under Windows, to avoid closing it
# The toplevel window is called .TkRoot and it is created when the tcltk code loads
# It is a tkwin object that one can test using is.tkwin()
# A tkwin object is a list with two components: ID, the window id in Tcl, and env,
# an environment holding information about subwindows, the parent window and any
# callback functions associated with the window
# The reason for the 'env' is that copies refer to the same environment (for instance
# when passing the tkwin to a function, we got a copy)
# Also, each window object is stored in the environment of its parent and the
# window itself keeps a variable containing the parent window. This allows for
# keeping these objects alive and not being reclaimed by the R garbage
# collector... unless one uses tkdestroy() which deletes the window entry in its
# parent env, and allows thus garbage collection! 
.TkRoot
is.tkwin(.TkRoot)
# tktoplevel() creates a top level window; tkdestroy() destroys it
# .Tk.ID() returns the ID contained in a tkwin object
tt <- tktoplevel()
tt
is.tkwin(tt)
.Tk.ID(tt)
tkdestroy(tt)
### TODO: define tk2toplevel() and tk2destroy() that do more (bindings, etc.)
# .Tk.newwin() and .Tk.subwin() are internally used to create windows in some
# circumstances (for instance, new widgets). Note that .Tk.newwin()
# tkwidget(parent, type, ...) creates a widget using .Tk.newwin()
# There are shortcuts like:
# tkbutton      <- function(parent, ...) tkwidget(parent, "button", ...)
# for: tkbutton(), tkcanvas(), tkcheckbutton(), tkentry(), tkframe(), tklabel(),
#      tklistbox(), tkmenu(), tkmenubutton(), tkmessage(), tkradiobutton(),
#      tkscale(), tkscrollbar(), tktext()
### TODO: add more and make them styled = tk2XXX() widgets


# There are a series of commands to manipulate widgets, e.g., tkadd() for instance
# Two ways to interact with widgets: 'control variables' and 'callbacks'
# A callback where we use '...' (we don't care of which arguments are passed...
# and these arguments are different depending which part of the scrollbar is clicked
t3 <- tktoplevel()
txt <- tktext(t3)
scr <- tkscrollbar(t3, command = function(...) tkyview(txt, ...))
tkconfigure(txt, yscrollcommand = function(...) tkset(scr, ...))
tkpack(scr, side = "right", fill = "y")
tkpack(txt, fill = "both", expand = TRUE)

tkdestroy(t3)

# Another example where we use the myproc %x %y Tcl callback and translate it to R

plotMove <- function(x, y) {
	x <- as.numeric(x)
	y <- as.numeric(y)
	tkmove(canvas, "selected", x - lastX, y - lastY)
	lastX <<- x	# This is a bad idea to save temp variables in .GlobalEnv!
	lastY <<- y # Idem
}
t4 <- tktoplevel()
canvas <- tkcanvas(t4)
tkpack(canvas, fill = "both", expand = TRUE)
lastX <<- 0
lastY <<- 0
tkbind(canvas, "<B1-Motion>", plotMove)

tkdestroy(t4)
# This was a binding to an 'event'. An event has three parts and is between <>:
# modifier, type and details, like in <Control-Alt-Key-c>, "Control & Alt = modif
# Key = type and 'c' = details.
# Binding events is done with tkbind(), sometimes tkbindtags()
# in binding, one can append an action by prefixing with a '+', or delete all if
# nothing is provided (=> add this feature?).
# One also can bind to a window, a class 'Button', or 'all', or any name
# Then, bintags() can map names to a particular window (and the same fct allow
# to read them)
# Events are managed using tkevent.add(), tkevent.delete(), tkevent.generate()
# and tkevent.info()
tkbindtags(t4)
tkbindtags(canvas)

# The placement of widgets inside windows is done using tkgrid(), tkplace() or
# tkpack()

# Example of packer:
tt <- tktoplevel()
edge <- c("top", "right", "bottom", "left")
buttons <- lapply(1:4, function(i) tkbutton(tt, text = edge[i]))
for ( i in 1:4 )
	tkpack(buttons[[i]], side = edge[i], fill="both")
# fill => use all space, expand => accopies all the space, or anchor
tkdestroy(tt)

# Example of grid:
t2 <- tktoplevel()
heading <- tklabel(t2, text = "Registration form")
l.name <- tklabel(t2, text = "Name")
l.age <- tklabel(t2, text = "Age")
e.name <- tkentry(t2, width = 30)
e.age <- tkentry(t2, width = 3)
tkgrid(heading, columnspan = 2)
tkgrid(l.name, e.name)
tkgrid(l.age, e.age)
tkgrid.configure(e.name, e.age, sticky = "w")
tkgrid.configure(l.name, l.age, sticky = "e")

tkdestroy(t2)

# Manual resizing of windows disable the autoresizing feature, but it can be reenabled
# using tkwm.geometry(tt, "")
### TODO: a demonstration of those three implementations
### TODO: a GUI builder
 
# A list widget
### TODO: a tk2listbox with automatically assigned two variables!
mylist <- tclVar()
# We show here how to retrieve the selection of the list in a Tcl variable
mysel <- tclVar()
tt <- tktoplevel()
lb <- tklistbox(tt, listvariable = mylist, selectmode = "single")
#lb <- tklistbox(tt, listvariable = mylist, selectmode = "extended")
scr <- tkscrollbar(tt, command = function(...) tkyview(lb, ...))
tkconfigure(lb, yscrollcommand = function(...) tkset(scr, ...))
tkpack(scr, side = "right", fill = "y")
tkpack(lb, fill = "both", expand = TRUE)
tkconfigure(lb, borderwidth = 0, exportselection = 0) #, exportselection = 1)
# This ensures to collect back the list selection into mysel
tkbind(lb, "<<ListboxSelect>>", function() tclvalue(mysel) <- tkcurselection(lb)) 
### TODO: the <<ListboxSelect>> event is not trigered when the selection is changed
tkpack(tkentry(tt, text = "rrrt"))
# programmatically! => what should we do here?
# Fill the list
tclObj(mylist) <- month.name
as.character(tclObj(mylist))
# Select 3rd to 6th months
tkselection.set(lb, 3-1, 6-1)	# Take care: 0-based indices!
# Scroll to last month
tksee(lb, 12-1)
# What is currently selected?
as.integer(tclObj(mysel))

# Is the 3rd month selected?
as.logical(tkselection.includes(lb, 3-1))
# What are the currently selected items?
as.integer(tkcurselection(lb))
as.integer(tclObj(mysel))
# To clear all selections
tkselection.clear(lb, 0, tksize(lb))
as.integer(tkcurselection(lb))
as.integer(tclObj(mysel))
# Disable the list
tkconfigure(lb, state = "disabled") 
# and reenable it again
tkconfigure(lb, state = "normal") 
tclObj(mylist) <- month.name[1:8]

tkdestroy(tt)
# Get the selected items
as.character(tclObj(mylist))[as.integer(tclObj(mysel))+1]	# Do not forget +1!


# The text widget

# Tags allow to reference parts of the text (annotations)
# tktag.add(), tktag.bind(), tktag.cget(), tktag.configure(), tktag.delete(),
# tktag.lower(), tktag.names(), tktag.nextrange(), tktag.prevrange(),
# tktag.raise(), tktag.ranges(), tktag.remove()

# To get all text:
X <- tkget(txt, "0.0", "end")
strsplit(X, "\n")

# To get the selected text:
if (tktag.ranges(txt, "sel") == "") X <- "" else
	X <- tkget(txt, "sel.first", "sel.last")

# To insert text at the end:
tkinsert(txt, "end", paste(string, collapse = "\n")

# To change the insertion mark
tkmark.set(txt, "insert", "0.0")
# There is: tkmark.names(), tkmark.next(), tkmark.previous(), tkmark.set() and tkmark.unset()
tksee(txt, "insert")
# + tkmark.gravity() that controls where themark moves when text is inserted at
# its place (insertion does not move the mark, and by default it is placed at right)
tkmark.gravity(txt, "insert", "left")	# Gravity set to the left


# Menus are independent widgets that can attach to menubutton, toplevel, cascade entry
# or be used as popup
# Step 1) create the menu
# Step 2) add to it with tkadd (command, checkbutton, radiobutton, cascade entry,
# tear-off or separator)
# tear-off is on by default for tkmenu(), but put it off for tk2menu()
# Step 3) bind the menu to a particular action, or to a particular widget
# A menubutton with three radiobuttons menu entries
### TODO: a better tk2add() for menus (tk2menu.add??? with "command", "separator", ...)

color <- tclVar("blue")
tt <- tktoplevel()
tkpack(mb <- tkmenubutton(tt, text = "Color"))
m <- tkmenu(mb, tearoff = FALSE)
for ( item in c("red", "blue", "green"))
	tkadd(m, "radio", label = item, variable = color, value = item)
tkconfigure(mb, menu = m)
butOK <- tkbutton(tt, text = "OK", command = function(){
	cat(tclvalue(color), "\n"); tkdestroy(tt)})
tkpack(butOK)

# File handling function:
# tclfile.tail(), tclfile.dir(), tclopen(), tclclose(), tclputs(), tclread()
# There are standard dialog: tkgetOpenFile(), tkgetSaveFile(), tkchooseDirectory()
# tkmessageBox(), tkdialog(), tkpopup()

# A simple application scripting widgets as an example of using these functions
## Add wheelmouse binding:
#bind .lb <Button-4> {.lb yview scroll -1 units }
#bind .lb <Button-5> {.lb yview scroll +1 units }

tkscript <- function() {
	tt <- tktoplevel()
	tktitle(tt) <- "R editor"
	txt <- tktext(tt, height = 10, font = "lucida console 9")
	tkpack(txt, fill = "both", expand = TRUE) ### TODO: add scrollbars
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
		code <- as.character(tkget(txt, "0.0", "end"))
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

# Clipboard access


# I need a way to test Tk dialog boxes: show them, capture screen and simulate
# clicks!


# While creating windows, it could be useful to disable update temporarily
oldmode <- tclServiceMode(FALSE)
# Do some work to create a nice picture.  Nothing will be displayed until...
tclServiceMode(oldmode)

# Load of a package:
# First, make sure to append the path where the Tcl package resides
res <- addTclPath(libdir)	# Returns the list of Tcl paths invisibly
# The get the list of Tcl paths:
as.character(tcl("set", "auto_path"))
# One could define TclPath() which gives this
# Then use:
tclRequire("Tktable", warn = TRUE)
# Use package forget to unset it!?
# Should be also useful to interface this:
# package present ?-exact? package ?version? (to test if a package is present!)

# Source Tcl code:
tcl("source", "tclscript")
# Could define:
tclsource("tclscript")

# Load code in shared libs:
tcl("load", "mysharedlib")
# Could define:
tclload("mysharedlib")
# Note: on the doc, they say it is NOT possible to unload or relaod shared libs!
# => how to quit completely from tcltk/tcltk2???
# Related commands: info sharedlibextension (.so or .dll)

# 1) 


# tkpager and tk_select.list() respectively create a pager and a selection dialog box
tkpager(file.path(R.home(), "CHANGES"), "Changes in R", "tkPager - ", FALSE)
### TODO: make tk2pager and allow for changing options(pager=....)
tk_select.list(c("apples", "oranges", "bananas"), title = "Select fruits")
### TODO: make a tk2select.list()
# Theres is also a tkStartGUI() which provides a tk R console, but not under Windows


#tcltk functions and examples from tcltk online help:
xyzzy <- tclVar(7913)
tclvalue(xyzzy)
tclvalue(xyzzy) <- "foo"
as.character(xyzzy)
tcl("set", as.character(xyzzy))

top <- tktoplevel() # a Tk widget, see Tk-widgets
ls(envir=top$env, all=TRUE)
ls(envir=.TkRoot$env, all=TRUE)# .Tcl.args put a callback ref in here


tt <- tktoplevel()
tkpack(l1<-tklabel(tt,text="Heave"), l2<-tklabel(tt,text="Ho"))
tkpack.configure(l1, side="left")


tt <- tktoplevel()
label.widget <- tklabel(tt, text="Hello, World!")
button.widget <- tkbutton(tt, text="Push",
                          command=function()cat("OW!\n"))
tkpack(label.widget, button.widget) # geometry manager
                                    # see Tk-commands

## Push the button and then...

tkdestroy(tt)







## Try stretching the window and then

tkdestroy(tt)


tt <- tktoplevel()
tkpack(txt.w <- tktext(tt))
tkinsert(txt.w, "0.0", "plot(1:10)")

# callback function 
eval.txt <- function()
   eval(parse(text=tclvalue(tkget(txt.w, "0.0", "end"))))
tkpack(but.w <- tkbutton(tt,text="Submit", command=eval.txt))

## Try pressing the button, edit the text and when finished:

tkdestroy(tt)


# Internationalisation
# Retrieve a string translated from tk msgs
::msgcat::mclocale fr
[namespace eval ::tk mc &Abort]
    

    


######################################
capabilities("tcltk")

# tclArray() defines a new empty array
A <- tclArray()


array exists
array size
array unset
array statistics
[[
[[<-
$
$<-
names
#names<- No!
length
#length<- No!



# This makes problems!!
require(tcltk)
tclRequire("Tktable")

myRarray <- c("Name","\"James Wettenhall\"","R-Help",
              "Email","wettenhall@wehi.edu.au","R-Help@stat.math.ethz.ch")

dim(myRarray) <- c(3,2)

for (i in (0:2))
  for (j in (0:1))
     .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

tt<-tktoplevel()
table1 <- tkwidget(tt,"table",variable="tclarray",rows="3",cols="2",titlerows="1",selectmode="extended",colwidth="25",background="white")
tkpack(table1)

# A more sophisticated example
# Define a matrix :
matrix1 <- matrix(1:2000,nrow=50,ncol=40,dimnames=list(paste("Row",1:50),paste("Col",1:40)))

# Define a Tcl array and initialize it to that matrix :
tclArray1 <- tclArrayVar(matrix1)

# Display the Tcl array in a Tk table widget (using print.tclArrayVar).
# The Tcl name of the array variable is displayed in the title bar.
tclArray1

# Display the Tcl array, showing only 10 rows and 10 columns :
print.tclArrayVar(tclArray1,height=10,width=10)

# Change the value of one of the elements in the Tcl array :
tclArray1[2,2] <- 999999

# Check the value of one of the elements in the Tcl array :
tclArray1[2,2]
[1] "999999"

tclArray1[5]
Error in "[.tclArrayVar"(tclArray1, 5) : Object is not a one-dimensional Tcl array


# One dimensional array
# Define a vector
vector1 <- 1:100

# Define a Tcl array and initialize it to that vector :
tclArray2 <- tclArrayVar(vector1)

# Display the Tcl array (in a Tk table widget) :
tclArray2

# Display the Tcl array, showing only 10 rows :
print.tclArrayVar(tclArray2,height=10)

# Check the value of one of the elements in the Tcl array :
tclArray2[5]
[1] "5"
tclArray2[2,3]
Error in "[.tclArrayVar"(tclArray2, 2, 3) :
        Object is not a two-dimensional Tcl array
        
## Arrays
# Define a data frame
dataFrame1 <- data.frame(names=c("foo","bar"),ages=c(20,30))
tclarray3  <- tclArrayVar(dataFrame1)
tclarray3

## Wrapper function
tclArrayVar <- function(Rarray=NULL)
{
    if (!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray))!=2)
      stop("Array must be one-dimensional or two-dimensional.")
    require(tcltk)
    n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env)
    name <- paste("::RTcl", n,sep = "")
    l <- list(env = new.env(),nrow=0,ncol=0,ndim=0)
    assign(name, NULL, envir = l$env)
    reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
    class(l) <- "tclArrayVar"
    if (is.null(Rarray))
    {
      ndim <- 2
      .Tcl(paste("set ",name,"(0,0) \"\"",sep=""))
    }
    else
    {
      if (is.vector(Rarray))
      {
        ndim <- 1
        Rarray <- as.data.frame(Rarray)
      }
      else
        ndim <- 2
      for (i in (1:nrow(Rarray)))
        if (ndim==2)
          for (j in (1:ncol(Rarray)))
            .Tcl(paste("set ",name,"(",i,",",j,") \"",paste(Rarray[i,j]),"\"",sep=""))
        else
          .Tcl(paste("set ",name,"(",i,",",1,") \"",paste(Rarray[i,1]),"\"",sep=""))
      if (!is.null(rownames(Rarray)))
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"",rownames(Rarray)[i],"\"",sep=""))
      else
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"\"",sep=""))
      if (!is.null(colnames(Rarray)))
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"",colnames(Rarray)[j],"\"",sep=""))
      else
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"\"",sep=""))
      l$nrow <- nrow(Rarray)
      l$ncol <- ncol(Rarray)
    }
    l$ndim <- ndim
    l
}

print.tclArrayVar <- function(tclArray,height=-1,width=-1)
{
  require(tcltk)
  tt <- tktoplevel()
  tclRequire("Tktable")
  tclArrayName <- ls(tclArray$env)
  tkwm.title(tt,tclArrayName)
  table1 <- tkwidget(tt,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),titlerows="1",titlecols="1",
                     height=paste(height+1),width=paste(width+1),
                     xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclArrayName,background="white",selectmode="extended")
}


assign("[.tclArrayVar",
function(object, i, j=NULL) {
  require(tcltk)
  if (is.null(j) && object$ndim!=1)
    stop("Object is not a one-dimensional Tcl array")
  if (!is.null(j) && object$ndim!=2)
    stop("Object is not a two-dimensional Tcl array")
  if (object$ndim==1)
    j <- 1
  tclArrayName <- ls(object$env)
  tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))
})

assign("[<-.tclArrayVar",
function(object, i, j=NULL,value) {
  require(tcltk)
  if (is.null(j) && object$ndim!=1)
    stop("Object is not a one-dimensional Tcl array")
  if (!is.null(j) && object$ndim!=2)
    stop("Object is not a two-dimensional Tcl array")
  if (object$ndim==1)
    j <- 1
  tclArrayName <- ls(object$env)
  .Tcl(paste("set ",tclArrayName,"(",i,",",j,") ",value,sep=""))
  if (i>object$nrow) object$nrow <- i
  return(object)
})


### Copy from tktable to Excel
tkconfigure(table1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
# Allow resizing rows and/or columns
tkconfigure(table1,resizeborders="none")    # OR
tkconfigure(table1,resizeborders="both")    # OR
tkconfigure(table1,resizeborders="row")     # OR
tkconfigure(table1,resizeborders="col")
# No line wrapping
tkconfigure(table1,multiline="0")
# Adding, inserting rows/columns
tkinsert(table1,"rows","end","1")
tkinsert(table1,"cols","end","1")
# Insert before:
tkinsert(table1,"rows",tclvalue(tkindex(table1,"active","row")),"-1")
tkinsert(table1,"cols",tclvalue(tkindex(table1,"active","col")),"-1")

# Deleting rows and columns
tkdelete(table1,"rows","end","1")
tkdelete(table1,"cols","end","1")
tkdelete(table1,"rows",tclvalue(tkindex(table1,"active","row")),"1")
tkdelete(table1,"cols",tclvalue(tkindex(table1,"active","col")),"1")




