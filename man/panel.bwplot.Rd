\name{panel.bwplot}
\alias{panel.bwplot}
\title{ Default Panel Function for bwplot }
\description{
  This is the default panel function for \code{bwplot}.
}
\synopsis{
panel.bwplot(x, y, box.ratio=1, horizontal = TRUE, pch=box.dot$pch,
             col=box.dot$col, cex = box.dot$cex,
             levels.fos = NULL, ...)
}
\usage{
panel.bwplot(x, y, box.ratio = 1, horizontal, ...)
}
\arguments{
  \item{x}{ numeric, boxplots drawn for each unique value of y }
  \item{y}{ numeric grouping variable}
  \item{box.ratio}{ ratio of box height to inter box space}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{\dots}{ graphical parameters can be supplied}
}
\details{ Creates Box and Whisker plot of \code{x} for every level of \code{y}
}
\seealso{
  \code{\link{bwplot}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















