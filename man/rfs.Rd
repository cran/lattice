\name{rfs}
\alias{rfs}
\title{ Residual and Fit Spread Plots }
\description{
  Plots fitted values and residuals (via qqmath) on a common scale for
  any object that has methods for fitted values and residuals. 
}
\synopsis{
rfs(model, layout=c(2, 1), xlab="f-value", ylab=NULL, panel, ...)
}
\usage{
rfs(model, layout=c(2, 1), xlab="f-value", ...)
}
\arguments{
  \item{model}{a fitted model object with methods \code{fitted.values}
    and \code{residuals}. Can be the value returned by \code{oneway} }
  \item{layout}{default layout is c(2,1)  }
  \item{xlab}{defaults to \code{"f.value"} }
  \item{\dots}{other arguments}
}
\value{
  an object of class trellis, by default plotted by \code{print.trellis}.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{oneway}}, \code{\link{qqmath}},
  \code{\link{trellis.args}}, \code{\link{Lattice}}
}
\examples{
data(singer)
rfs(oneway(height ~ voice.part, data = singer, spread = 1), aspect = 1)
}
\keyword{dplot}
