\name{qqmath}
\alias{qqmath}
\title{ Q-Q Plot with Theoretical Distribution }
\description{
  Quantile-Quantile plot of a sample and a theoretical distribution
}
\synopsis{
qqmath(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.qqmath, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, f.value = ppoints, 
    distribution = qnorm, ..., subscripts = !is.null(groups), 
    subset = TRUE)
}
\usage{
qqmath(formula, distribution = qnorm, f.value = ppoints,...)
}
\arguments{
  \item{formula}{formula of the form \code{~ x | g1 * g2 * \ldots},
    where \code{x} must be a numeric.
  }
  \item{distribution}{ a quantile function that takes a vector of
    probabilities as argument and produces the corresponding
    quantiles. Possible values are \code{qnorm,qunif} etc.
    Distributions with other required arguments need to be passed in as
    user defined functions. This is also passed in as an argument to the
    panel function (though not used by the default panel function), 
    and the prepanel function if specified, for use in the bery commonly
    used \code{(pre)panel.qqmathline}.
  }
  \item{f.value}{function of a single integer (representing sample
    size), returning a vector of probabilities corresponding to which
    quantiles should be plotted.
  }
  \item{\dots}{Other arguments}
}
\value{
  An object of class trellis, by default plotted by \code{print.trellis}.
}
\details{
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.
}
\author{Deepayan Sarkar \url{deepayan@stat.wisc.edu}}
\seealso{\code{\link{xyplot}}, \code{\link{panel.qqmath}},
  \code{\link{panel.qqmathline}}, \code{\link{prepanel.qqmathline}},
  \code{\link{Lattice}}
}
\examples{
qqmath(~ rnorm(100), distribution = function(p) qt(p, df = 10))
data(singer)
qqmath(~ height | voice.part, aspect = 1, data = singer)
}
\keyword{dplot}

