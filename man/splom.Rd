\name{splom}
\title{Scatter Plot Matrices}
\synopsis{
splom(formula, aspect = 1, as.table = FALSE, between = list(x = 0.5, 
    y = 0.5), data, groups = NULL, key = NULL, layout, main = NULL, 
    page = NULL, panel = panel.splom, superpanel = panel.pairs, 
    par.strip.text, prepanel, scales, skip = FALSE, strip = strip.default, 
    sub = NULL, subscripts = !missing(groups), subset, xlab = NULL, 
    xlim, ylab = NULL, ylim, ...)
}
\usage{
splom(formula, ...)
}
\description{
  Draw scatter plot matrices
}
\alias{splom}
\arguments{
  \item{formula}{a formula describing the form of the plot.  A
    formula of the form \code{~ x | g1 * g2 * ...}, where \code{x} is a
    matrix with rows giving the variables of which the pairwise
    scatterplots will be produced, conditional on the 
    variables \code{g1,g2,...}.
    
    Each of \code{g1,g2,...} must be either factors or shingles.

    The \code{scales} argument does not have its ususal interpretation,
    in particular \code{x} and \code{y} components do not make
    sense. Other components of scale which are sensible would be
    supported (in future if not already supported). The \code{rot}
    component should be specified as a vector of length 2 for the labels
    of the horizontal and vertical labels respectively.
  }
  \item{\dots}{ other arguments}
}
\details{
  see the documentation for \code{trellis.args}
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
splom(~iris[1:4], groups = Species, data = iris, panel = panel.superpose)
new.iris <- iris[1:3]
colnames(new.iris) <- c("Sepal\nLength", "Sepal\nWidth",
"Petal\nLength")
splom(~new.iris|Species, data = iris)
}
\keyword{hplot}
