\name{panel.xyplot}
\alias{panel.xyplot}
\alias{panel.splom}
\alias{panel.qqmath}
\alias{panel.qq}
\title{ Default Panel Function for xyplot }
\description{
  This is the default panel function for \code{xyplot}. Also see
  \code{panel.superpose}. The default panel functions for
  \code{splom} and \code{qqmath} are essentially the same function.
}
\usage{
panel.xyplot(x, y, type="p",
             pch = plot.symbol$pch,
             col,
             col.line = plot.line$col,
             col.symbol = plot.symbol$col,
             lty = plot.line$lty,
             cex = plot.symbol$cex,
             lwd = plot.line$lwd, ...)
panel.splom(...) <- function (...) panel.xyplot(...)
panel.qqmath(...) <- function (...) panel.xyplot(...)
}
\arguments{
  \item{x,y}{ coordinates of points to be plotted in the scatterplot}
  \item{type}{ character vector consisting of one or more of the
    following: \code{"p", "l", "h", "b", "o", "s", "S", "r", "smooth"}. 

    The behaviour if any of the first six are included in \code{type} is
    similar to the effect of \code{type} in \code{\link{plot}}. 
    \code{"r"} adds a  regression line (same as
    \code{\link{panel.lmline}}, except for default graphical
    parameters), and \code{"smooth"} adds a lowess fit (same as
    \code{\link{panel.loess}}).

    See example in \code{xyplot} for an example.
  }
  \item{col, col.line, col.symbol}{
    default colours are obtained from \code{plot.symbol} and
    \code{plot.line} using \code{\link{trellis.par.get}}. These default
    instead to \code{col} when the latter is specified.
  }
  \item{pch, lty, cex, lwd}{
    other graphical parameters.
  }
  \item{\dots}{ other arguments, e.g., arguments to pass to
    \code{panel.loess}.
  }
}
\details{ Creates scatterplot of \code{x} and \code{y}, with various
  modifications possible via the type argument. \code{panel.qq} also
  draws a 45 degree line. 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{panel.superpose}}, \code{\link{xyplot}},
  \code{\link{splom}},\code{\link{qqmath}}}
\keyword{dplot}
