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
            panel.number,
            prepanel.limits,
            ...)
}
\arguments{
  \item{z}{ The data frame used for the plot.}
  \item{panel}{ The panel function used inside.}
  \item{groups}{ Grouping variable, if any.}
  \item{panel.subscripts}{ logical specifying whether the panel
    function accepts an argument named \code{subscripts}.}
  \item{subscripts}{ The indices of the rows of \code{z} that are to be
    displayed in this (super)panel.}
  \item{pscales}{ Controls axis labels, passed down from
    \code{splom}. If pscales is a single number, it indicates the
    approximate number of equally-spaced ticks that should appear on
    each axis. If pscales is a list, it should have p components, each
    of which is itself a list with the following valid components:

    at: a numeric vector specifying tick locations

    labels: character vector labels to go with at

    limits: numeric 2-vector specifying axis limits (should be made more
    flexible at some point to handle factors)

    These are specifications on a per-variable basis, and used on all
    four sides in  the diagonal cells used for labelling.

    Factor variables are labelled with the factor names. Use pscales=0
    to supress the axes entirely.
  }
  \item{panel.number}{ the sequential index of the panel in the Lattice
    plot which this function will draw, meant to keep track of the
    conditioning variable for the panel. Passed to the `panel' function
    if it accepts such an argument.
  }
  \item{prepanel.limits}{ The `regular' high level lattice plots such as
    \code{xyplot} use the \code{prepanel} function for deciding on axis
    limits from data. This function serves a similar function, and works
    on a per-variable basis, by calculating the limits, which can be
    overridden by the corresponding \code{limits} component in the
    \code{pscales} list.
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
















