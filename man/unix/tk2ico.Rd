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
  These function are only useful for Windows, but they can be used without error
  on other platform for making platform-independent code that has an additional
  behaviour under Windows. On the other platforms, these function just return NULL.
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
  These function do nothing and return \code{NULL} on other platforms than Windows.
}

\author{ Philippe Grosjean }

\keyword{ utilities }
