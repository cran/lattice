\name{trellis.par.get}
\alias{trellis.par.get}
\alias{trellis.par.set}
\alias{trellis.settings}
\title{ Manipulating Graphical Parameters for Trellis Displays }
\description{
  These functions are used to query and modify graphical parameters for
  fine control of Trellis displays.
}
\usage{
trellis.par.get(name)
trellis.par.set(name, value)
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
  variable named \code{trellis.settings} (type
  \code{print(trellis.settings)} to see the current value). There are a
  couple of settings defined in the package, which are set when
  \code{trellis.device} is called. The functions \code{trellis.par.*}
  are meant to be interfaces to this variable. \code{trellis.par.get} is
  usually used inside trellis functions to get graphical parameters
  before plotting. This enables device customization as well as
  modifications by users through the \code{trellis.par.set}
  interface. Typical usage is: 

  \code{add.line <- trellis.par.get("add.line")}

  \code{add.line$col <- "red"}

  \code{add.line <- trellis.par.set("add.line", add.line)}
}
\value{
  \code{trellis.par.get} returns a list giving parameters for that
  component .
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{trellis.device}}, \code{\link{trellis.settings}},
  \code{\link{Lattice}}
}
\keyword{dplot}


