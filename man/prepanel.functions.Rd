\name{prepanel.functions}
\alias{prepanel.lmline}
\alias{prepanel.loess}
\alias{prepanel.qqmathline}
\title{ Useful Prepanel Function for Lattice}
\description{
  These are predefined prepanel functions available in Lattice.
}
\usage{
prepanel.lmline(x, y, ...)
prepanel.loess(x, y, span = 2/3, iter = 3, ...)
prepanel.qqmathline(x, distribution, ...) 
}
\arguments{
  \item{x, y}{x and y values.}
  \item{distribution}{ quantile function for theoretical distribution}
  \item{\dots}{other arguments}
}

\value{
  usually a list with components \code{xlim, ylim, dx, dy}, the first
  two being used to calculate panel axes limits, the last two for
  banking computations.
}
\seealso{\code{\link{lowess}}, \code{\link{trellis.par.get}},
  \code{\link{Lattice}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




