\name{Lattice}
\alias{Lattice}
\title{Lattice Graphics}
\description{
  Implementation of Trellis Graphics
}
\details{
  Lattice is an implementation of Trellis Graphics (see the references
  for details). It is intended to emulate the
  implementation of Trellis in S, and perhaps to add some functionality
  of its own. Existing Trellis code written for S-Plus should
  \emph{ideally} work unchanged in Lattice, one unavoidable
  exception being custom panel functions (see the entry for \code{panel}
  in \code{help(trellis.args)}, and also \code{llines} for a collection
    of functions meant to ease the conversion).  

  As of now, this implementation is very much incomplete;
  however, what has been implemented should be useful. (Note that there
  are no plans to implement the 3D display functions in the immediate
  future). 

  This implementation depends heavily on a new Graphics engine (Grid) for
  R being developed by Paul Murrell.

  Type \code{library(help = lattice)} to see a list of (public)
  Lattice graphics functions.
}
\note{
  The description of arguments in the documentation for
  \code{trellis.args} is meant to work for all Lattice
  functions. However, many of these (such as \code{aspect="xy"}) do not
  make much sense for many of the functions. These functions would still
  accept these arguments, but the resulting behaviour is not very well
  defined in many cases. The ultimate goal, of course, is to incorporate
  the most sensible behaviour and document it, but that is not a top
  priority right now. The general idea is that if something does
  not work as it should with \code{xyplot}, however weird the options, 
  it's a bug and should be reported. However, other functions such
  as \code{splom, parallel, histogram} etc are only expected to work
  when they are doing what they are designed to do, and might not handle
  weird prepanel functions and the like very well.
}
\seealso{
  To get a general idea of what arguments are generally accepted by
  Lattice  functions, see \code{\link{trellis.args}}.

  To learn how to customise the Graphical parameters used by the Lattice
  functions, see \code{\link{trellis.settings}}.

  To learn how to initialise new devices or change the settings of the
  current device, see \code{\link{trellis.device}}

  To learn about sophisticated (non-default) printing capabilities, see
  \code{\link{print.trellis}}. 

  Here is a list of `high level' functions in the Lattice library with a
  brief description of what they do:

  \bold{Univariate:}  

  \code{\link{barchart}} bar plots
  
  \code{\link{bwplot}} box and whisker plots

  \code{\link{densityplot}} kernel density plots  

  \code{\link{dotplot}} dot plots

  \code{\link{histogram}} histograms

  \code{\link{qqmath}} quantile plots against mathematical distributions

  \code{\link{stripplot}} 1-dimensional scatterplot

  \bold{Bivariate:}

  \code{\link{qq}} q-q plot for comparing two distributions
  
  \code{\link{xyplot}} scatter plot (and possibly a lot more)

  \bold{Trivariate:}

  \code{\link{levelplot}} level plots (image plots in R)
  
  \bold{Hypervariate:}
  
  \code{\link{splom}} scatterplot matrix
  
  \code{\link{parallel}} parallel coordinate plots
  
  \bold{Miscellaneous:}

  \code{\link{rfs}} residual and fitted value plot (also see
  \code{\link{oneway}})

  See \code{\link{llines}} if you need to write/port nontrivial panel
  functions.
}
\references{
  Bell Lab's Trellis Page:
  \url{http://cm.bell-labs.com/cm/ms/departments/sia/project/trellis/}
  
  Cleveland, W.S. (1993) \emph{Visualizing Data}.
  
  Becker, R.A., Cleveland, W.S. and Shyu, M. 
  ``The Visual Design and Control of Trellis Display'',
  \emph{Journal of Computational and Graphical Statistics}
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
