\name{parallel}
\title{Parallel Coordinate Plots}
\usage{
parallel(formula, ...)
}
\description{
  Draw scatter plot matrices
}
\alias{parallel}
\arguments{
  \item{formula}{a formula describing the form of the plot.  A
    formula of the form \code{~ x | g1 * g2 * ...}, where \code{x} is a
    matrix/data frame. Parallel coordinate plots of the columns 
    will be produced, conditional on the variables \code{g1,g2,...}.
    
    Each of \code{g1,g2,...} must be either factors or shingles.
  }
}
\details{
  see the documentation for \code{trellis.args}
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{panel.parallel}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
parallel(~iris[,1:4]|Species, data = iris, layout=c(3,1))
}
\keyword{hplot}







