".tk2dde.require" <-
function() {
	if (.Platform$OS.type != "windows") stop("This is a Windows-specific function!")
    # Make sure tcl/tk dde is operational
	if (!capabilities("tcltk")) stop("This version of R cannot use Tcl/Tk!")
	tclRequire("dde", warn = TRUE)	# Should be installed by default with the tcltk package under Windows
	return(TRUE)	# Everything is fine
}

"tk2dde" <-
function(topic = NULL) {
    # Initialize a tcltk dde server with name 'TclEval|topic'
    .tk2dde.require()

    # If topic is NULL, just get my server name
    if (is.null(topic)) return(tclvalue(.Tcl("dde servername {}")))

    # Otherwise topic must be character
    topic <- topic[1]
    if (!is.character(topic) || topic == "") stop("'topic' must be a non null character string!")

    # Verify if I am not already registered under this topic
    if (tclvalue(.Tcl("dde servername {}")) == topic) return(0)	# OK

    # Check that this server name does not exist yet
    if (length(grep(paste("[{]TclEval ", topic, "[}]", sep = ""), as.character(.Tcl("dde services TclEval {}")), useBytes = TRUE)) > 0)
    return(1)# This server name already exists => return 1 and don't set it!

    # Register me as a dde server with this topic name
    .Tcl(paste("dde servername", topic))
    # Check that the server is set correctly (if not, return 2 to warn that a problem occurred)
    if (tclvalue(.Tcl("dde servername {}")) == topic) return(0) else return(2)
}

"tk2dde.exec" <-
function(service, topic, command, async = FALSE) {
    # Execute a command in the 'service|topic' dde server
    .tk2dde.require()

    if (!is.character(service) || !is.character(topic)) stop("'service' and 'topic' must be character strings!")
    if (async[1] == TRUE) async <- "-async" else async <- ""

    # Execute the command in a try(), to nicely catch the error
    # class is "try-error" if an error occurs, otherwise, returns ""
    res <- (try(tclvalue(.Tcl(paste("dde execute ", async, " ", service[1], " ", topic[1], " ", command[1], sep = "")))))
    return(res)
}

"tk2dde.poke" <-
function(service, topic, item, data) {
    # Set a value (data) to 'item' in the 'service|topic' dde server's application
    .tk2dde.require()

    if (!is.character(service) || !is.character(topic)) stop("'service' and 'topic' must be character strings!")
    if (!is.character(item)) stop("'item' must be character strings!")

    # For some reasons, dde poke does not seem to work with a TclEval serve... use dde execute instead
    if (service == "TclEval") {
        # In Tcl, if 'data' is a character string, we must enclose it in curly braces
        if (is.character(data)) data <- paste("{", data, "}", sep = "")
        Cmd <- paste("{set ", item[1], " ", data[1], "}", sep = "")# This would not work with all kind of data!!!
        # Also, if this is a vector, matrix, or array, it does not work properly!
        return(tk2dde.exec(service, topic, Cmd, async = TRUE))
    }

    # Poke the data within a try(), to nicely catch the error
    # class is "try-error" if an error occurs, otherwise, returns ""
    res <- (try(as.character(.Tcl(paste("dde poke", service[1], topic[1], item[1], as.character(data[1]))))))
    return(res)
}

"tk2dde.request" <-
function(service, topic, item, binary = FALSE) {
    # Get the value for 'item' in 'service|topic' dde server
    .tk2dde.require()

    if (!is.character(service) || !is.character(topic)) stop("'service' and 'topic' must be character strings!")
    if (!is.character(item)) stop("'item' must be character strings!")
    if (binary[1] == TRUE) binary <- "-binary" else binary <- ""

    # Request the value in a try(), to nicely catch the error
    # class is "try-error" if an error occurs, otherwise, returns ""
    res <- (try(as.character(.Tcl(paste("dde request ", binary, " ", service[1], " ", topic[1], " ", item[1], sep = "")))))
    return(res)
}

"tk2dde.services" <-
function(service = "", topic = "") {
    # List the 'service|topic' dde currently available
    .tk2dde.require()

    # Check arguments
    if (!is.character(service) || !is.character(topic)) stop("'service' and 'topic' must be character strings!")
    service <- service[1]
    if (service == "") service <- "{}"# This is an empty string in Tcl
    topic <- topic[1]
    if (topic == "") topic <- "{}"# This is an empty string in Tcl

    # Otherwise topic must be character
    if (!is.character(topic)) stop("'topic' must be a non null character string!")

    # Get the list of all 'service|topic' dde servers currently running
    res <- as.character(.Tcl(paste("dde services", service, topic)))

    # Return the result
    return(res)
}

"tk2ico.create" <-
function(icofile)
    if (.Platform$OS.type != "windows") NULL else .Tcl(paste("winico createfrom", icofile))

"tk2ico.destroy" <-
function(icon)
	if (.Platform$OS.type != "windows") NULL else tcl("winico", "delete", icon)

"tk2ico.hicon" <-
function(icon)
	if (.Platform$OS.type != "windows") NULL else tcl("winico", "hicon", icon)

"tk2ico.info" <-
function(icon, convert = TRUE) {
    if (.Platform$OS.type != "windows") {
		return(NULL)
	} else {
        inf <- as.character(tcl("winico", "info", icon))
        if (convert) { # Transform into a data frame
            inf <- strsplit(inf, "-")
            inf <- matrix(unlist(inf), ncol = 8, byrow = TRUE)
            inf <- inf[, -1]
            inf <- unlist(strsplit(inf, " "))
            inf <- inf[inf != ""]
            inf <- inf[(1:(length(inf)/2)) * 2]
            inf <- matrix(inf, ncol = 7)
            inf <- data.frame(pos = as.integer(inf[, 1]),
            width = as.integer(inf[, 2]),
            height = as.integer(inf[, 3]),
            geometry = inf[, 4],
            bpp = as.integer(inf[, 5]),
            hicon = inf[, 6],
            ptr = inf[, 7])
            inf$geometry <- as.character(inf$geometry)
            inf$hicon <- as.character(inf$hicon)
            inf$ptr <- as.character(inf$ptr)
            }
        return(inf)
    }
}

"tk2ico.load" <-
function(file = "shell32.dll", res = "application")
    if (.Platform$OS.type != "windows") NULL else tcl("winico", "load", res, file)

"tk2ico.pos" <-
function(icon, pos)
    if (.Platform$OS.type != "windows") NULL else tcl("winico", "pos", icon, pos)

"tk2ico.set" <-
function(win, icon, pos = NULL, type = c("all", "small", "big")) {
    type <- type[1]
    if (type != "small" && type != "big" && type != "all")
        stop("'type' must be 'all', 'small' or 'big'!")
    if (.Platform$OS.type != "windows") {
        return(NULL)
	} else {
        if (type == "all") {# We search for highest quality icons
        inf <- tk2ico.info(icon, convert = TRUE)
        if (nrow(inf) == 1) {# Only one icon in the ressource
            res <- tcl("winico", "setwindow", win, icon, "small")
            res <- tcl("winico", "setwindow", win, icon, "big")
        } else { # There are several icons in the resource
            # Are there 16x16 icons?
            pos16 <- inf[inf$geometry == "16x16", ]$pos
            if (length(pos16) > 0) pos16 <- max(pos16) else pos16 <- 0
            res <- tcl("winico", "setwindow", win, icon, "small", pos16)
            # Are there 32x32 icons?
            pos32 <- inf[inf$geometry == "32x32", ]$pos
            if (length(pos32) > 0) pos32 <- max(pos32) else
                pos32 <- max(inf$pos)
                res <- tcl("winico", "setwindow", win, icon, "big", pos32)
            }
        } else {# Other type than 'all'
            if (is.null(pos)) {
                res <- tcl("winico", "setwindow", win, icon, type)
            } else {
                res <- tcl("winico", "setwindow", win, icon, type, pos)
            }
        }
        return(res)
    }
}

"tk2ico.taskbar.add" <-
function(icon, pos = 0, text = tk2ico.text(icon), callback = NULL)
	if (.Platform$OS.type != "windows") {
	    return(NULL)
	} else {
    	if (is.null(callback)) {
        	res <- tcl("winico", "taskbar", "add", pos = pos, text = text)
    	} else {
        	res <- tcl("winico", "taskbar", "add", pos = pos, text = text,
            	callback = callback)
    	}
    	return(res)
	}

"tk2ico.taskbar.delete" <-
function(icon)
	if (.Platform$OS.type != "windows") NULL else
		tcl("winico", "taskbar", "delete", icon)

"tk2ico.taskbar.modify" <-
function(icon, pos = NULL, text = NULL)
	if (.Platform$OS.type != "windows") {
	    return(NULL)
	} else {
    	if (!is.null(pos)) {
        	if (is.null(text)) {
            	tcl("winico", "taskbar", "modify", icon, pos = pos)
        	} else {
            	tcl("winico", "taskbar", "modify", icon, pos = pos, text = text)
        	}
    	} else if (!is.null(text))
    		tcl("winico", "taskbar", "modify", icon, text = text)
	}

"tk2ico.text" <-
function(icon, text = NULL)
	if (.Platform$OS.type != "windows") {
	    return(NULL)
	} else {
    	if (is.null(text)) res <- tcl("winico", "text", icon) else
    	res <- tcl("winico", "text", icon, text)
    	return(paste(as.character(res), collapse = " "))
	}

".tk2reg.require" <-
function() {
	# Make sure tcl/tk registry is operational
	if (.Platform$OS.type != "windows") stop("This is a Windows-specific function!")
	if (!capabilities("tcltk")) stop("This version of R cannot use Tcl/Tk!")
	tclRequire("registry", warn = TRUE)	# Should be installed by default with the tcltk package under Windows
	return(TRUE)	# Everything is fine
}

"tk2reg.broadcast" <-
function() {
    # Used to warn running apps that something changes in the registry
    # Use this when you change an environment variable
    .tk2reg.require()
    .Tcl("registry broadcast \"Environment\"")
}

"tk2reg.delete" <-
function(keyname, valuename) {
    # Delete a registry value in a key (take care when using this!)
    .tk2reg.require()
    .Tcl(paste("registry delete {", keyname, "} {", valuename, "}", sep = ""))
}

"tk2reg.deletekey" <-
function(keyname) {
    # Completelly delete a registry key (take care when using this!)
    .tk2reg.require()
    .Tcl(paste("registry delete {", keyname, "}", sep = ""))
}

"tk2reg.get" <-
function(keyname, valuename) {
    # Get the content of a key... do not manage types yet!
    .tk2reg.require()
    tclvalue(.Tcl(paste("registry get {", keyname, "} {", valuename, "}", sep = "")))
}

"tk2reg.keys" <-
function(keyname) {
    # Get a list of all subkeys in a key
    .tk2reg.require()
    as.character(.Tcl(paste("registry keys {", keyname, "}", sep = "")))
}

"tk2reg.set" <-
function(keyname, valuename, data,
    type = c("sz", "expand_sz", "multi_sz", "dword", "dword_big_endian",
    "binary", "link", "resource_list", "none")) {
    # Set a registry key value
    .tk2reg.require()
    # check type
    type <- type[1]
    if (!(type %in% c("sz", "expand_sz", "multi_sz", "dword", "dword_big_endian",
    "binary", "link", "resource_list", "none"))) stop("Unrecognized 'type'!")
    # Convert data into a string... Can only use a single item currently
    data <- as.character(data[1])
    .Tcl(paste("registry set {", keyname, "} {", valuename, "} {", data, "} {", type, "}" , sep = ""))
}

"tk2reg.setkey" <-
function(keyname) {
    # Set a registry key
    .tk2reg.require()
    .Tcl(paste("registry set {", keyname, "}", sep = ""))
}

"tk2reg.type" <-
function(keyname, valuename) {
    # Get the type of a key...
    .tk2reg.require()
    tclvalue(.Tcl(paste("registry type {", keyname, "} {", valuename, "}", sep = "")))
}

"tk2reg.values" <-
function(keyname) {
    # Get a list of all values in a key
    .tk2reg.require()
    as.character(.Tcl(paste("registry values {", keyname, "}", sep = "")))
}

