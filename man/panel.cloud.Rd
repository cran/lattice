\name{panel.cloud}
\alias{panel.cloud}
\alias{panel.wireframe}
\title{ Default Panel Function for cloud }
\description{
  These are the default panel functions for \code{cloud} and
  \code{wireframe}.
}
\usage{
panel.cloud((x, y, z, subscripts, distance, xlim, ylim, zlim,
    subpanel = "panel.xyplot", 
    rot.mat = rot.mat, aspect = aspect,
    zcol, col.regions, par.box = NULL,
    xlab, ylab, zlab, scales.3d, 
    proportion = 0.6, wireframe = FALSE,
    scpos = list(x = 1, y = 8, z = 12), ...)
panel.wireframe(\dots)
}
\arguments{
  \item{x, y, z}{numeric vectors}
  \item{subscripts}{which points to draw}
  \item{distance}{distance argument of Trellis 3d args}
  \item{xlim, ylim, zlim}{limits}
  \item{subpanel}{panel function used to plot the projections. Can be
    \code{panel.superpose}} 
  \item{rot.mat}{rotation matrix, supplied by \code{cloud}}
  \item{aspect}{aspect as in \code{cloud}}
  \item{xlab, ylab, zlab}{ Labels, have to be lists}
  \item{scales.3d}{ list defining the scales}
  \item{proportion}{ Gives the length of arrows as a proportion of the
    sides
  }
  \item{scpos}{A list with three components x, y and z, describing which
     of the 12 sides of the cube the scales should be drawn. There is no
     code yet to intelligently decide on defaults, so it might have to
     be given explicitly. Valid values are x: 1, 3, 9, 11; y: 8, 5, 7, 6
     and z: 4, 2, 10, 12
  }
  \item{\dots}{ parameters to be passed to the \code{subpanel} function}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{cloud}}}
\keyword{dplot}
















