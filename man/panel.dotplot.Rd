\name{panel.dotplot}
\alias{panel.dotplot}
\title{ Default Panel Functions for dotplot}
\description{
  This is the default panel function for \code{dotplot}.
}
\synopsis{
panel.dotplot(x, y, horizontal = TRUE, pch = dot.symbol$pch, col = dot.symbol$col, 
    lty = dot.line$lty, lwd = dot.line$lwd, col.line = dot.line$col, 
    levels.fos = NULL, ...) 
}
\usage{
panel.dotplot(x, y, horizontal, ...)
}
\arguments{
  \item{x,y}{ coordinates of points to plot}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{\dots}{ graphical parameters can be supplied}
}
\details{ Creates Dotplot of \code{x} against \code{y}
}
\seealso{\code{\link{dotplot}}}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















