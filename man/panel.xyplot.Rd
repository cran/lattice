\name{panel.xyplot}
\alias{panel.xyplot}
\alias{panel.splom}
\alias{panel.qqmath}
\title{ Default Panel Function for xyplot }
\description{
  This is the default panel function for \code{xyplot}. Also see
  \code{panel.superpose}. The default panel functions for
  \code{splom} and \code{qqmath} are essentially the same function.
}
\synopsis{
panel.xyplot(x, y, type = "p", pch = plot.symbol$pch, col = if (type == 
    "l") plot.line$col else plot.symbol$col, lty = plot.line$lty, 
    ...)
panel.splom(...)
panel.qqmath(...)
}
\usage{
panel.xyplot(x, y, type="p", ...)
panel.splom(x, y, type="p", ...)
panel.qqmath(x, y, type="p", ...)
}
\arguments{
  \item{x,y}{ coordinates of points to be plotted in the scatterplot}
  \item{type}{ if \code{type} is "p" points are plotted, if "l", the
    points sre joined by lines, if "b", both are done. The default
    graphical parameters are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}} }
  \item{\dots}{ graphical parameters can be supplied}
}
\details{ Creates scatterplot of \code{x} and \code{y}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{panel.superpose}}, \code{\link{xyplot}},
  \code{\link{splom}},\code{\link{qqmath}}}
\keyword{dplot}
















