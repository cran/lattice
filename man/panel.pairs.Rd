\name{panel.pairs}
\alias{panel.pairs}
\title{ Default Superpanel Function for splom}
\description{
  This is the default superpanel function for \code{splom}.
}
\usage{
panel.pairs(z,
            panel = panel.xyplot,
            groups = NULL,
            panel.subscripts, 
            subscripts,
            fontsize.small = 8,
            scales, \dots)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{panel}{ The panel function used inside.}
  \item{groups}{ Grouping variable, if any.}
  \item{panel.subscripts}{ logical specifying whether the panel
    function accepts an argument named \code{subscripts}.}
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displayed in this (super)panel.}
  \item{fontsize.small}{ Fontsize to be used for the axis labels inside.}
  \item{scales}{ List to control axis annotation. Not much use right now.}
  \item{\dots}{ graphical parameters may be supplied.}
}
\details{
  This is the function that is actually passed in as the \code{panel}
  function in a trellis object produced by splom (taking the `panel'
  function as its argument).
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{splom}}}
\keyword{dplot}
















