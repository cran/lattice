\name{histogram}
\title{Histograms}
\synopsis{
histogram(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.histogram, prepanel = NULL, scales = list(), 
    strip = TRUE, groups = NULL, xlab, xlim, ylab, ylim, type = c("percent", 
    "count", "density"), nint, endpoints = range(x[!is.na(x)]), breaks,
    equal.widths = TRUE, ..., subscripts = !is.null(groups), 
    subset = TRUE)
}
\usage{
histogram(formula,
          data, 
          type = c("percent", "count", "density"),
          nint = if(is.factor(x)) length(levels(x))
                 else round(log2(length(x))+1),
          endpoints = range(x[!na.x]),
          breaks = if(is.factor(x)) seq(0.5, length = length(levels(x))
          + 1) else do.breaks(endpoints, nint),
          equal.widths = FALSE, 
          \dots)
}
\description{
  Draw Histograms
}
\alias{histogram}
\arguments{
  \item{formula}{A formula of the form \code{~ x | g1 * g2 * \ldots}
    indicates that histograms of \code{x} should be produced conditioned
    on the levels of the (optional) variables \code{g1,g2,\ldots}.
    
    \code{x} can be numeric or factor, and each of \code{g1,g2,\ldots}
    must be either factors or shingles.
  }
  \item{data}{data frame in which variables are to be evaluated}
  \item{type}{ Character string indicating type of histogram to be
    drawn. "percent" and "count" give relative frequency and frequency
    histograms, and can be misleading when breakpoints are not equally
    spaced. "density" produces a density scale histogram. (See second
    example below.) (This option appears to be missing in S-Plus.)
    
    \code{type} defaults to "percent", except when the breakpoints
    are unequally spaced or \code{breaks = NULL}. (The specified value of
    \code{type} is often overridden (e.g., when \code{log=T}), sometimes
    unnecessarily. )
  }
  \item{nint}{ Number of bins. Applies only when \code{breaks} is
    unspecified in the call.
  }
  \item{endpoints}{ vector of length 2 indicating the range of x-values
    that is to be covered by the  histogram. Again, applies only when
    \code{breaks} is unspecified.
  }
  \item{breaks}{ numeric vector of length = (number of bins + 1)
    defining the breakpoints of the bins. Note that when breakpoints are
    not equally spaced, the only value of \code{type} that makes sense
    is density.

    Usually all panels use the same breakpoints. This can be changed by
    specifying \code{breaks = NULL}. This has the effect of letting each
    panel choose its own breakpoints. The choice of these breakpoints
    are made as follows: The number of bins is calculated by the formula
    for \code{nint} above, and then breakpoints are chosen according to
    the value of \code{equal.widths}.
  }
  \item{equal.widths}{ logical, relevant only when \code{breaks=NULL}.
    If \code{TRUE}, equally spaced bins will be selected, otherwise, 
    approximately equal area bins will be selected (this would mean that
    the breakpoints will \bold{not} be equally spaced).
  }
  \item{\dots}{ other arguments}
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\details{
  see the documentation for \code{trellis.args}.
}
\note{
  The form of the arguments accepted by the default
  panel function \code{panel.histogram} is different from that in
  S-Plus. Whereas S-Plus calculates the heights inside \code{histogram}
  and passes only the breakpoints and the heights to the panel function,
  here the original variable \code{x} is passed along with the
  breakpoints. This allows plots as in the second example below.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{panel.histogram}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(singer)
histogram( ~ height | voice.part, data = singer, nint = 17,
          endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
          xlab = "Height (inches)")
## The following would not be possible in S-Plus
histogram( ~ height | voice.part, data = singer,
          xlab = "Height (inches)", type = "density",
          panel = function(x, ...) {
              panel.histogram(x, ...)
              panel.mathdensity(dmath = dnorm,
                                args = list(mean=mean(x),sd=sd(x)))
          } )
}
\keyword{hplot}
