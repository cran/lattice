\name{panel.qqmathline}
\alias{panel.qqmathline}
\title{Useful panel function with qqmath}
\description{
  Useful panel function with qqmath. Draws a line passing through the
  points determined by the .25 and .75 quantiles of the sample and the
  theoretical distribution.
}
\usage{
panel.qqmathline(y, distribution, ...)
}
\arguments{
  \item{y}{Sample}
  \item{distribution}{quantile function for reference theoretical
    distribution. This is automatically passed in when this is used as a
    panel function in \code{qqmath}.}
  \item{\dots}{other arguments.}
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{prepanel.qqmathline}}, \code{\link{qqmath}}}
\keyword{dplot}
