\name{prepanel.functions}
\alias{prepanel.lmline}
\alias{prepanel.loess}
\alias{prepanel.qqmathline}
\title{ Useful Prepanel Function for Lattice}
\description{
  These are predefined prepanel functions available in Lattice.
}
\synopsis{
prepanel.loess(x, y, span, degree, family, evaluation, lwd, lty,
               col, ...)
}
\usage{
prepanel.lmline(x, y, ...)
prepanel.loess(x, y, ...)
prepanel.qqmathline(y, distribution, f.value = ppoints, ...) 
}
\arguments{
  \item{x, y}{x and y values.}
  \item{distribution}{ quantile function for theoretical distribution}
  \item{f.value}{ as used in \code{qqmath}}
  \item{\dots}{other arguments}
}
\value{
  usually a list with components \code{xlim, ylim, dx, dy}, the first
  two being used to calculate panel axes limits, the last two for
  banking computations.
}
\seealso{\code{\link{lowess}}, \code{\link{trellis.par.get}},
  \code{\link{Lattice}}. See \code{\link[modreg]{loess.smooth}} for
    further options to \code{prepanel.loess}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




