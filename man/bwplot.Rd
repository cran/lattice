\name{bwplot}
\title{Box and Whisker Plots}
\synopsis{
bwplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.bwplot, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, box.ratio = 1, ..., 
    subscripts = !missing(groups), subset = TRUE)
}
\usage{
bwplot(formula, box.ratio = 1, \dots)
}
\description{
  Draw Box and Whisker Plots
}
\alias{bwplot}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | g1 * g2 * ...} indicates that
    plots of \code{y} versus \code{x} should be produced conditional on
    the variables \code{g1,g2,...}.
    
    \code{x} must be numeric, and each of \code{g1,g2,...} must
    be either factors or shingles. \code{y} can be a factor or a
    shingle. If \code{y} is a character vector, it will be coerced into
    a factor; if it is numeric, it will be coerced to a shingle.
  }
  \item{box.ratio}{ gives the ratio of the width of the rectangles to the
    inter rectangle space.
  }
  \item{\dots}{ other arguments}
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\details{
  see the documentation for \code{trellis.args}. Behaviour when trying
  to modify the y-axis via \code{scales$y} is not defined. (It might give
  what you want, but again, it might not.) The default value of
  \code{ylim} is an interval containing c(1,number of levels of y), with
  the extra amount calculated based on \code{box.ratio} (not 7\%, the
  usual default) .
}
\seealso{
  \code{\link{trellis.args}},
  \code{\link{panel.bwplot}},\code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(singer)
bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
}
\keyword{hplot}


