\name{qq}
\alias{qq}
\title{ Quantile-Quantile Plots of Two Samples }
\description{
  Quantile-Quantile plots for comparing two Distributions
}
\synopsis{
qq(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.qq, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, f.value = ppoints, 
    ..., subscripts = !is.null(groups), subset = TRUE)
}
\usage{
qq(formula, f.value = ppoints, ...)
}
\arguments{
  \item{formula}{formula of the form \code{y ~ x | g1 * g2 * \ldots},
    where \code{x} must be a numeric, and \code{y} can be a factor,
    shingle, character or numeric vector, with the restriction that there
    must be exactly two levels of \code{y}, which divide the values of
    \code{x} into two groups. Quantiles for these groups will be plotted
    along the two axes.
  }
  \item{f.value}{function of a single integer (representing sample
    size), returning a vector of probabilities corresponding to which
    quantiles should be plotted.}
  \item{\dots}{Other arguments}
}
\value{
  An object of class trellis, by default plotted by \code{print.trellis}.
}
\author{Deepayan Sarkar \url{deepayan@stat.wisc.edu}}
\note{The x and y scale limits are by default the ranges of the original
  data, instead of the range of the actual quantiles that are to be
  plotted. This will probably change later.}
\seealso{\code{\link{trellis.args}}, \code{\link{panel.qq}},
  \code{\link{Lattice}}
}
\examples{
data(singer)
qq(voice.part ~ height, aspect = 1, data = singer,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))
}
\keyword{dplot}




