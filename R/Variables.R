"makeTclNames" <-
function(names, unique = FALSE) {
    # Make valid Tcl variable names
    names <- make.names(names, unique = unique) # allow_ = TRUE by default in R >= 2.0.0
    # There is a problem if the variable starts with a dot => prepend it with 'X'
    .names <- grep("^\\.", names, useBytes = TRUE)
    names[.names] <- paste("X", names[.names], sep = "")
    # Although it is accepted, there could be problems with variable names containing dots
    # so, replace them with '_'
    names <- gsub("\\.", "_", names)
    return(names)
}

"tclFun" <-
function(f, name = deparse(substitute(f))) {
    # Register a simple R function (without arguments) as a callback in Tcl, and give it the same name)
    # Indeed, .Tcl.callback(f) does the job... but it gives criptic names like R_call 0x13c7168
    # Done in NAMESPACE
    #require(svMisc)
    #Require(tcltk) || stop("Package 'tcltk' is needed!")

    # Check that 'f' is a function with no arguments (cannot handle them, currently)
    is.function(f) || stop("'f' must be a function!")
    is.null(formals(f)) || stop("The function used cannot (yet) have arguments!")
    # Make sure the name of the function is valid
    if (!is.character(name)) stop("'name' must be a character string!") else name <- make.names(name[1])

    res <- .Tcl.callback(f)
    # Make sure this is correct (R_call XXXXXXXX)
    if (length(grep("R_call ", res, useBytes = TRUE) > 0)) # Create a proc with the same name in Tcl
    .Tcl(paste("proc ", name, " {} {", res, "}", sep = ""))
    # Return the R_call XXXXXXXX string, as .Tcl.callback() does
    return(res)
    # Rem: if you delete the R 'f' function, the Tcl 'f' function still works (?!)
}

"tclGetValue" <- function(name) {
    # Get the value stored in a plain Tcl variable
    if (!is.character(name)) stop("'name' must be a character!")
    name <- makeTclNames(name[1]) # The usual name conversion

    # Create a temporary dual variable with tclVar() (name does not mather)
    Temp <- tclVar(init = "")

    # Copy the content of the var of interest to it
    .Tcl(paste("catch {set ", as.character(Temp), " $", name, "}", sep = ""))

    # Get the content of the temporary variable
    Res <- tclvalue(Temp) # (Temp will be destroyed when the function exists)
    return(Res)
}

"tclSetValue" <- function(name, value) {
    # This is the opposite of tclGetValue() and it is a wrapper for 'set name value' command
    #### TO DO...
    cat("Not yet implemented!\n")
}

"tclVarExists" <- function(name) {
    as.integer(tcl("info", "exists", name)) == 1
}

"tclVarFind" <- function(pattern) {
    as.character(tcl("info", "vars", pattern))
}

"tclVarname" <- function(name, init = "", keep.existing = TRUE) {
    # tclVar gives names like ::RtclX automatically...
    # We need to define names ourselve. This is what tclVarname does
    # If keep existing == TRUE and the variable is already defined, then
    # we keep its content, instead of initializing it with "init"
    if (!is.character(name)) stop("'name' must be a character!")
    name <- makeTclNames(name[1]) # Make sure the name is correct
    
    # Temporary save potential content of the Tcl variable elsewhere (catch in case the variable does not exist)
    if (keep.existing) .Tcl(paste("catch {set ZZZTempRvariable $", name, "}", sep = ""))

    # Create the new dual Tcl-R variable
    l <- list(env = new.env())
    assign(name, NULL, envir = l$env)
    reg.finalizer(l$env, function(env) tcl("unset", ls(env)))
    class(l) <- "tclVar"
    tclvalue(l) <- init

    # Possibly restore the content of the variable, if keep.existing == TRUE
    if (keep.existing) {
        .Tcl(paste("catch {set", name, "$ZZZTempRvariable}"))
        .Tcl("unset -nocomplain ZZZTempRvariable")# Remove the temporary variable
    }
    return(l)
}
