\name{panel.stripplot}
\alias{panel.stripplot}
\title{ Default Panel Function for stripplot }
\description{
  This is the default panel function for \code{stripplot}. Also see
  \code{panel.superpose}
}
\usage{
panel.stripplot(x, y, jitter.data = F, factor, pch, col, ...)
}
\arguments{
  \item{x,y}{ coordinates of points to be plotted}
  \item{jitter.data}{ whether points should be jittered to avoid
    overplotting.} 
  \item{factor}{factor by which to jitter}
  \item{pch, col}{ graphical parameters can be supplied.}
  \item{\dots}{other arguments}
}
\details{ Creates stripplot (one dimensional scatterplot) of \code{x}
  for each level of \code{y}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{stripplot}}}
\keyword{dplot}
















