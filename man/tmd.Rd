\name{tmd}
\alias{tmd}
\title{Tukey Mean-Difference Plot}
\description{
  Create Tukey Mean-Difference Plots from a trellis object returned by
  \code{xyplot} and \code{qq}
}
\synopsis{
tmd(object, aspect = "fill", as.table = object$as.table, 
    between = list(x = object$x.between, y = object$y.between), 
    key = object$key, layout = object$layout, main = object$main, 
    page = object$page, panel = panel.tmd, par.strip.text = object$par.strip.text, 
    prepanel = NULL, scales = list(), strip = object$strip, sub = object$sub, 
    xlab = "mean", xlim = NULL, ylab = "difference", ylim = NULL, 
    ..., subscripts = !is.null(groups), subset = TRUE)
}
\usage{
tmd(object, xlab = "mean", ylab = "difference", ...)
}
\arguments{
  \item{object}{ An object of class ``trellis'' returned by
    \code{xyplot} or \code{qq}
  }
  \item{xlab}{ x label}
  \item{ylab}{ y label}
  \item{\dots}{ other arguments}
}
\details{
  The Tukey Mean-difference plot is produced by modifying the (x,y)
  values of each panel as follows: the new coordinates are given by
  \code{x=(x+y)/2, y=y-x}.

  Most arguments given to produce object are transmitted to \code{tmd},
  with the notable exceptions of \code{aspect} and \code{scales}. (This
  behaviour is different from S-Plus.)
}
\value{
  An object of class ``trellis''.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{ \code{\link{panel.tmd}}, \code{\link{xyplot}},
  \code{\link{Lattice}}
}
\examples{
data(singer)
tmd(qqmath(~height|voice.part, data = singer))
}
\keyword{dplot}
