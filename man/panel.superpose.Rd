\name{panel.superpose}
\alias{panel.superpose}
\title{ Panel Function for Display Marked by groups }
\description{
  This is an useful panel function for trellis displays. Uses a grouping
  variable, plotting the \code{x} and \code{y} variables with different
  graphical properties for each distinct value of the grouping variable.
}
\synopsisi{
panel.superpose(x, y, subscripts, groups, type = "p",
                col = superpose.symbol$col,  
                pch = superpose.symbol$pch,
                lty = superpose.line$lty, ...)
}
\usage{
panel.superpose(x, y, subscripts, groups, type="p", ...)
}
\arguments{
  \item{x,y}{coordinates of the points to be displayed}
  \item{subscripts}{ subscripts giving indices in original data frame}
  \item{groups}{ a grouping variable. Different graphical parameters
    will be used for each distinct value of \code{groups}. The default
    graphical parameters are obtained from \code{superpose.symbol} and
    \code{superpose.line} using \code{\link{trellis.par.get}} }
  \item{type}{ if "p" points are plotted, if "l", the points are joined
    by lines, if "b", both are done.}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
















