\name{Lattice}
\alias{Lattice}
\title{Lattice Graphics}
\description{
  Implementation of Trellis Graphics
}
\details{
  Lattice is an implementation of Trellis Graphics (see the references
  in the Reference section for details). It is intended to emulate the
  implementation of Trellis in S, and perhaps to add some functionality
  of its own. As of now, this implementation is very much incomplete;
  however, what has been implemented should be useful. (Note that there
  are no plans of implementing the 3D display functions in the immediate
  future). 

  This implementation depends heavily on a new Graphics engine (Grid) for
  R being developed by Paul Murrell. Knowledge of Grid is not necessary
  for using Lattice (except when writing new panel functions), but for
  one important aspect: Grid (and hence Lattice) graphics \bold{do not}
  mix with conventional R Graphics. The way to use both is to enclose
  any Grid graphics commands by calls to \code{grid.start} and
  \code{grid.stop}. Note that \code{library(lattice)} automatically calls
  \code{grid.start}. 

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

  \bold{Hypervariate:}
  
  \code{\link{splom}} scatterplot matrix
  
  \code{\link{parallel}} parallel coordinate plots
  
  \bold{Miscellaneous:}

  \code{\link{rfs}} residual and fitted value plot (also see
  \code{\link{oneway}})
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






