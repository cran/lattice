\name{Lattice}
\alias{Lattice}
\title{Lattice Graphics}
\description{
  Implementation of Trellis Graphics in R
}
\details{
  Trellis Graphics is a framework for data visualization developed at
  the Bell Labs by Rick Becker, Bill Cleveland et al, extending ideas
  presented in Bill Cleveland's 1993 book \emph{Visualizing Data}.

  Lattice is best thought of as an implementation of Trellis Graphics
  for R. Its interface is based on the implementation in S-Plus,
  but there are several differences. To the extent possible, care has
  been taken to ensure that existing Trellis code written for S-Plus
  works unchanged (or with minimal change) in Lattice. If you are having
  problems porting S-Plus code, read the entry for \code{panel} in
  the documentation for \code{\link{xyplot}} (and perhaps the Lattice
  website referenced below).

  Most high level Trellis functions in S-Plus are implemented, with the
  exception of \code{piechart} (also, \code{\link{wireframe}} is still
  quite slow).

  Lattice is built upon the Grid Graphics engine for R being developed
  by Paul Murrell and requires the \code{grid} add-on package.

  Type \code{library(help = lattice)} to see a list of (public)
  Lattice graphics functions for which further documentation is
  available. Apart from the documentation accompanying this package, 
  several documents outlining the use of Trellis graphics is available
  from Bell Lab's website that might provide a holistic introduction to
  the Trellis paradigm. Lattice also has a website with some tips.
}
\note{
  High level Lattice functions (like \code{\link{xyplot}}) are different
  from conventional S graphics functions because they don't actually
  draw anything. Instead, they return an object of class ``trellis''
  which has to be then \code{print}ed. This often causes confusion when
  the high level functions are called inside another function (most
  often \code{source}) and hence don't produce any output.
}
\seealso{
  To get a general idea of what arguments are generally accepted by
  Lattice functions, see \code{\link{xyplot}}.

  To learn how to customise the Graphical parameters used by the Lattice
  functions, see \code{\link{lattice.theme}} and \code{\link{lset}}.

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

  Lattice Webpage:
  \url{http://packages.r-project.org/lattice/}
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
