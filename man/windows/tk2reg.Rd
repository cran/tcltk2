\name{tk2reg}
\alias{tk2reg}
\alias{tk2reg.broadcast}
\alias{tk2reg.delete}
\alias{tk2reg.deletekey}
\alias{tk2reg.get}
\alias{tk2reg.keys}
\alias{tk2reg.set}
\alias{tk2reg.setkey}
\alias{tk2reg.type}
\alias{tk2reg.values}

\title{ Manipulate the registry under Windows }
\description{
  These functions access the Windows registry.
}

\usage{
  tk2reg.broadcast()
  tk2reg.delete(keyname, valuename)
  tk2reg.deletekey(keyname)
  tk2reg.get(keyname, valuename)
  tk2reg.keys(keyname)
  tk2reg.set(keyname, valuename, data, type = c("sz", "expand_sz", "multi_sz",
      "dword", "dword_big_endian", "binary", "link", "resource_list", "none"))
  tk2reg.setkey(keyname)
  tk2reg.type(keyname, valuename)
  tk2reg.values(keyname)
}

\arguments{
  \item{keyname}{ The name of the key. }
  \item{valuename}{ A value in this key. }
  \item{data}{ The data to place in the value. }
  \item{type}{ The type of value in the registry. By default, it is "sz", that is, a string. }
}

\value{
  Functions that should return a registry value return it in a character string
  no mather the type of value in the registry. You are responsible to coerce it
  to the correct type.
}

\author{ Philippe Grosjean }

\note{ For Windows only. These functions issue an error when they are called under
  other platforms. Take care while manipulating the Windows registry! You can
  easily lock the system completelly, if you delete important items, especially if
  you are logged as administrator on your computer. Make a backup of your registry
  first before experimenting with these function!!!
}

\seealso{ \code{\link{tk2dde}}, \code{\link{tk2ico}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	## Examples of tk2reg - registry manipulation under Windows
    # Rem: HKEY_LOCAL_MACHINE, HKEY_USERS, HKEY_CLASSES_ROOT, HKEY_CURRENT_USER,
    #      HKEY_CURRENT_CONFIG, HKEY_PERFORMANCE_DATA, HKEY_DYN_DATA
    Rkey <- "HKEY_LOCAL_MACHINE\\\\Software\\\\R-core\\\\R"   # The R key
    Rsubkey <- paste(Rkey, "subkey", sep = "\\\\")        # A subkey

    # Get all subkeys for Software in the local machine
    tk2reg.keys("HKEY_LOCAL_MACHINE\\\\Software")

    # Get all names in the R key
    tk2reg.values(Rkey)
    # Get info about registered R
    tk2reg.get(Rkey, "Current Version")
    tk2reg.get(Rkey, "InstallPath")

    # Create a subkey (explore the registry with regedit.exe to see it)
    tk2reg.setkey(Rsubkey)
    # Add something in it
    tk2reg.set(Rsubkey, "test", "a key added with tcltk2 registry!", type = "sz")

    # Get the type of a value
    tk2reg.type(Rsubkey, "test")
    # Get a value in a key
    tk2reg.get(Rsubkey, "test")

    # Delete a name in a key (take care: dangerous!)
    tk2reg.delete(Rsubkey, "test")
    # Delete a whole key (take care: very dangerous!)
    tk2reg.deletekey(Rsubkey)

    # An alternate way to get the path
    tk2reg.get("HKEY_LOCAL_MACHINE\\\\SYSTEM\\\\CurrentControlSet\\\\Control\\\\Session Manager\\\\Environment", "path")

    # Make sure that currently running apps are warned of your changes in the registry
    tk2reg.broadcast()

    # Delete temporary variables
    rm(list = c("Rkey", "Rsubkey"))
  }
}
\keyword{ utilities }
