\name{trellis.par.get}
\alias{trellis.par.get}
\alias{trellis.par.set}
\alias{trellis.settings}
\alias{show.settings}
\title{ Manipulating Graphical Parameters for Trellis Displays }
\description{
  These functions are used to query and modify graphical parameters for
  fine control of Trellis displays.
}
\usage{
trellis.par.get(name)
trellis.par.set(name, value)
show.settings()
trellis.settings
}
\arguments{
  \item{name}{name of a component in quotes. For possible values type
    \code{names(trellis.settings)}.
  }
  \item{value}{ desired value of the component. 
    }
}
\details{
  Lattice functions get their default graphical parameters from a global
  variable named \code{trellis.settings}. There are a couple of settings
  of this variable defined in the package, which can be set by calling
  \code{trellis.device} with appropriate arguments.

  The functions \code{trellis.par.*} are meant to be interfaces to this
  variable. \code{trellis.par.get} returns the current value of a
  particular component and \code{trellis.par.get} sets its value.

  \code{trellis.par.get} is usually used inside trellis functions to get
  graphical parameters before plotting. This enables device
  customization as well as modifications by users via
  \code{trellis.par.set}. Typical usage is: 

  \code{add.line <- trellis.par.get("add.line")}

  \code{add.line$col <- "red"}

  \code{add.line <- trellis.par.set("add.line", add.line)}

  The actual list of the components in \code{trellis.settings} has not
  been finalized, so I'm not attempting to list them here. The current
  value can be obtained by \code{print(trellis.settings)}. Most names
  should be self-explanatory.

  \code{show.settings} provides a graphical display summarizing some of
  the values in the current setting.

  There are a few more (non S compatible) interface functions which
  might be simpler to use. Details can be found in the documentation for
  \code{lset}.
}
\note{
  S-Plus keeps a different value of the settings attached to each
  device. Lattice does not work that way, and consequently sometimes the
  process does not work as it should --- e.g., if an \code{x11} device is
  already open with a specific (possibly modified from the default)
  setting, and a call to \code{trellis.device(postscript)} is made, the
  old setting will be overridden and will not be restored by
  \code{dev.off()}.
}
\value{
  \code{trellis.par.get} returns a list giving parameters for that
  component .
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{trellis.device}}, \code{\link{lset}},
  \code{\link{Lattice}}
}
\examples{
show.settings()
}
\keyword{dplot}


