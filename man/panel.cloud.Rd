\name{panel.cloud}
\alias{panel.cloud}
\title{ Default Panel Function for cloud }
\description{
  This is the default panel function for \code{cloud}.
}
\usage{
panel.cloud(x, y, z, subscripts, distance, xlim, ylim, zlim,
            subpanel = "panel.xyplot",  
            rot.mat = rot.mat, aspect = aspect, ...) 
}
\arguments{
  \item{x, y, z}{numeric vectors}
  \item{subscripts}{which points to draw}
  \item{distance}{distance argument of Trellis 3d args}
  \item{xlim, ylim, zlim}{limits}
  \item{subpanel}{panel function used to plot the projections. Can be
    panel.superpose} 
  \item{rot.mat}{rotation matrix}
  \item{aspect}{aspect as in Trellis 3d args}
  \item{\dots}{ parameters to be passed to the \code{subpanel} function}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{cloud}}}
\keyword{dplot}
















