\name{densityplot}
\title{Kernel Probability Density Plots}
\alias{densityplot}
\description{
  Draw Kernel Density plots
}
\synopsis{
densityplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.densityplot, prepanel = NULL, scales = list(), 
    strip = TRUE, groups = NULL, xlab, xlim, ylab, ylim, bw = NULL, 
    adjust = NULL, kernel = NULL, window = NULL, width = NULL, 
    give.Rkern = FALSE, n = 50, from = NULL, to = NULL, cut = NULL, 
    na.rm = NULL, ..., subscripts = !is.null(groups), subset = TRUE)
}
\usage{
densityplot(formula, data, n = 50, plot.points = TRUE, ref = FALSE, \dots)
}
\arguments{
  \item{formula}{ A  formula of the form \code{ ~ x | g1 * g2 * \ldots}
    indicating that density plots of \code{x} should be produced
    conditional on the levels of the variables \code{g1,g2,\ldots}.
    
    \code{x} must be numeric, and each of \code{g1,g2, \ldots} must
    be either factors or shingles.
  }
  \item{data}{ data frame in which variables are to be evaluated.}
  \item{n}{number of points at which density is to be evaluated}
  \item{plot.points}{ logical specifying whether the \code{x} values
    should be plotted.
  }
  \item{ref}{logical specifying whether a reference x-axis should be
    drawn.} 
  \item{\dots}{ arguments to be passed down to the panel function, in
    turn to be passed to \code{density} (if the default panel function
    is used). 
  }
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\details{
  See the documentation for \code{trellis.args} for description of other
  valid arguments. The density estimate is actually calculated using the
  function \code{density}, and all arguments accepted by it can be
  passed (as \code{\ldots}) in the call to \code{densityplot} to control
  the output. See documentation of \code{density} for details. (Note: The
  default value of the argument \code{n} of \code{density} is changed to
  50.)
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{density}},
  \code{\link{panel.densityplot}},
  \code{\link{panel.mathdensity}},\code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(singer)
densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)", bw = 5)
## Using a predefined panel function to fit a normal distribution
densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)",
            ylab = "Kernel Density/ Normal Fit",
            main = list("Estimated Density", cex = 2, col = "DarkOliveGreen"),
            panel = function(x, ...) {
                panel.xyplot(x = jitter(x),
                             y = rep(0, length(x)))
                panel.densityplot(x, ...)
                panel.mathdensity(dmath = dnorm,
                                  args = list(mean=mean(x),sd=sd(x)))
            } )
}
\keyword{hplot}
