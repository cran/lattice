\name{panel.barchart}
\alias{panel.barchart}
\title{ Default Panel Functions for barchart}
\description{
  This is the default panel function for \code{barchart}.
}
\synopsis{
panel.barchart(x, y, box.ratio = 1, horizontal, col = bar.fill$col, ...) 
}
\usage{
panel.barchart(x, y, horizontal, col, \dots)
}
\arguments{
  \item{x}{ Endpoints of Bars. Bars start at left of panel.}
  \item{y}{ Horizontal location of bars, possibly factor}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{col}{ Color of the bars. By default, the trellis parameter
    \code{bar.fill} is used.
  }
  \item{\dots}{ extra arguments will be accepted but ignored }
}
\seealso{
  \code{\link{barchart}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















