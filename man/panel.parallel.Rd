\name{panel.parallel}
\alias{panel.parallel}
\title{ Default Panel Function for parallel}
\description{
  This is the default panel function for \code{parallel}.
}
\usage{
panel.parallel(z, subscripts, col, lty, ...)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displyed in this panel.}
  \item{col, lty}{graphical parameters (chosen by default as the settings
    for \code{superpose.line}).}
  \item{\dots}{other arguments}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{parallel}}, \code{\link{trellis.par.get}}}
\keyword{dplot}
















