\name{panel.bwplot}
\alias{panel.bwplot}
\title{ Default Panel Function for bwplot }
\description{
  This is the default panel function for \code{bwplot}.
}
\synopsis{
panel.bwplot(x, y, box.ratio=1, horizontal = TRUE, pch=box.dot$pch,
             col=box.dot$col, cex = box.dot$cex, fill = box.rectangle$fill,
             levels.fos = NULL, ...)
}
\usage{
panel.bwplot(x, y, box.ratio = 1, horizontal, pch, col, cex,
            fill = "transparent", ...)
}
\arguments{
  \item{x, y}{ numeric vector or factor. Boxplots drawn for each unique value of
    \code{y} (\code{x}) if \code{horizontal} is \code{TRUE} (\code{FALSE})}
  \item{box.ratio}{ ratio of box height to inter box space}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{pch, col, cex}{ graphical parameters controlling the dot}
  \item{fill}{ color to fill the }
  \item{\dots}{ graphical parameters can be supplied}
}
\details{ Creates Box and Whisker plot of \code{x} for every level of \code{y}
}
\seealso{
  \code{\link{bwplot}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















