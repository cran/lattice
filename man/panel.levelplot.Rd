\name{panel.levelplot}
\alias{panel.levelplot}
\title{ Default Panel Function for levelplot }
\description{
  This is the default panel function for \code{levelplot}.
}
\usage{
panel.levelplot(x, y, z, wx, wy, zcol, col.regions, subscripts, at,
                labels, contour, region, col, lty, lwd,
                ...)
}
\arguments{
  \item{x, y, z}{numeric vectors}
  \item{subscripts}{which points to draw}
  \item{wx, wy}{vectors of widths and heights for the rectangles}
  \item{zcol}{color numbers for each x,y combination}
  \item{col.regions}{colors indexed by zcol} 
  \item{at}{cut points}
  \item{labels}{contour labels}
  \item{contour}{logical}
  \item{region}{logical}
  \item{col, lty, lwd}{graphical parameters for contour lines}
  \item{\dots}{extra parameters }
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{levelplot}}}
\keyword{dplot}
















