\name{banking}
\alias{banking}
\title{Banking}
\description{
  Calculates `banking' slope
}
\usage{
banking(dx, dy)
}
\arguments{
  \item{dx, dy}{ vector of consecutive x, y differences. Look in
    \code{xyplot} code for how this is used.
  }
}
\details{
  \code{banking} is a crude banking function used when aspect="xy" in
  \code{xyplot}. It just orders the absolute slopes and returns a value
  which when adjusted by the panel scale limits will make the median of
  the above absolute slopes a 45 degree line. (Confused ? see code.)
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{Lattice}}, \code{\link{trellis.args}}}
\keyword{dplot}
















