\name{panel.barchart}
\alias{panel.barchart}
\title{ Default Panel Functions for barchart}
\description{
  This is the default panel function for \code{barchart}.
}
\synopsis{
panel.barchart(x, y, box.ratio = 1, col = bar.fill$col, ...) 
}
\usage{
panel.barchart(x, y, \dots)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ Endpoints of Bars. Bars start at left of panel.}
  \item{y}{ Horizontal location of bars.}
  \item{\dots}{ graphical parameters can be supplied.
    By default, the trellis parameter bar.fill is used to get the color
    with which to fill the bars.}
}
\seealso{
  \code{\link{barchart}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















