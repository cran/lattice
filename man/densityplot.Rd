\name{densityplot}
\title{Kernel Probability Density Plots}
\alias{densityplot}
\description{
  Draw Kernel Density plots
}
\synopsis{
densityplot(formula, aspect = "fill", as.table = FALSE, between = list(x = 0, 
    y = 0), data, groups = NULL, key = NULL, layout, main = NULL, 
    page = NULL, panel = panel.densityplot, par.strip.text, prepanel, 
    scales, skip = FALSE, strip = strip.default, sub = NULL, 
    subscripts = !missing(groups), subset, xlab, xlim, ylab, 
    ylim, n = 50, \dots) 
}
\usage{
densityplot(formula, n = 50, \dots)
}
\arguments{
  \item{formula}{ A  formula of the form \code{ ~ x | g1 * g2 * \ldots}
    indicating that density plots of \code{x} should be produced
    conditional on the levels of the variables \code{g1,g2,\ldots}.
    
    \code{x} must be numeric, and each of \code{g1,g2, \ldots} must
    be either factors or shingles.
  }
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
