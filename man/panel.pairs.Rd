\name{panel.pairs}
\alias{panel.pairs}
\title{ Default Superpanel Function for splom}
\description{
  This is the default superpanel function for \code{splom}.
}
\usage{
panel.pairs(z, panel = panel.splom,
            groups = NULL, panel.subscripts, 
            subscripts,
            pscales = 5,
            panel.number, ...)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{panel}{ The panel function used inside.}
  \item{groups}{ Grouping variable, if any.}
  \item{panel.subscripts}{ logical specifying whether the panel
    function accepts an argument named \code{subscripts}.}
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displayed in this (super)panel.}
%%  \item{fontsize.small}{ Fontsize to be used for the axis labels inside.}
  \item{pscales}{ Controls axis labels, passed down from \code{splom}}
  \item{panel.number}{ the sequential index of the panel in the Lattice
    plot which this function will draw. Passed to the `panel' function
    if it accepts such an argument.
  }
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
















