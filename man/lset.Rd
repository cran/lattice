\name{lset}
\alias{lset}
\alias{col.whitebg}
\title{Simple Interfaces to modify Trellis Settings}
\description{
  A (hopefully) simpler alternative interface to
  \code{trellis.par.get/set}. NOT S-compatible
}
\usage{
lset(theme, warn = TRUE)
col.whitebg()
}
\arguments{
  \item{theme}{ a list decribing how to change the settings of the
    current active device. Valid components are those in the list
    returned by \code{trellis.par.get()}. Each component must itself be
    a list, with one or more of the appropriate components (need not
    have all components). Changes are made to the settings for the
    currently active device only.
  }
  \item{warn}{ logical, whether a warning should be issued when
    \code{trellis.par.get} is called when no graphics device is open }
}
\details{
  \code{lset} essentially does the same thing as \code{trellis.par.set},
  but is different in that it allows setting more than one parameter at
  a time, and also that each component need have only the components
  that need to be changed. 
  
  This function is expected to be used mostly in conjunction with
  functions that return `themes', i.e., lists which when supplied to
  \code{lset} change the overall look and feel of the display.

  \code{col.whitebg} is one such function, which returns a list which
  when supplied to \code{lset} changes the background to transparent and
  the other colors to something suitable. Contributions of more such
  `themes' are welcome. 
}
\seealso{\code{\link{lattice.theme}}, \code{\link{show.settings}},
  \code{\link{Lattice}}} 
\examples{
show.settings(col.whitebg())
# The following reproduces Splus Unix black and white pch:
#lset(list(superpose.symbol=list(pch=c("o","+",">","s","w","#","\{"))))
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
