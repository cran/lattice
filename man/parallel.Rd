\name{parallel}
\title{Parallel Coordinate Plots}
\alias{parallel}
\synopsis{
parallel(formula, data = parent.frame(), aspect = "fill", between = list(x = 0.5, 
    y = 0.5), layout = NULL, panel = panel.parallel, prepanel = NULL, 
    scales = list(), strip = TRUE, groups = NULL, xlab = NULL, 
    xlim, ylab = NULL, ylim, varnames, ..., subscripts = !is.null(groups), 
    subset = TRUE)
}
\usage{
parallel(formula, data, between, varnames, ...)
}
\description{
  Draw scatter plot matrices
}
\arguments{
  \item{formula}{a formula describing the form of the plot.  A
    formula of the form \code{~ x | g1 * g2 * ...}, where \code{x} is a
    matrix/data frame. Parallel coordinate plots of the columns 
    will be produced, conditional on the variables \code{g1,g2,...}.
    
    Each of \code{g1,g2,...} must be either factors or shingles.
  }
  \item{data}{data frame where variables in the call are evaluated}
  \item{between}{defaults to .5 on both x and y}
  \item{varnames}{Optional column names for \code{x}}
  \item{\dots}{other arguments}
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







