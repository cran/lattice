\name{dotplot}
\title{Dot Plots}
\usage{
dotplot(formula,
        panel = panel.dtplot,
        data = sys.frame(sys.parent()), 
        \dots)
}
\description{
  Draw Cleveland Dot plots
}
\alias{dotplot}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | g1 * g2 * ...} indicates that plots of
    \code{y} versus \code{x} should be produced conditional on the
    variable \code{g1,g2,...}.
    
    \code{x} must be numeric, and each of \code{g1,g2,...} must
    be either factors or shingles. \code{y} can be a factor or a
    shingle. If \code{y} is a character vector, it will be coerced into
    a factor; if it is numeric, it will be coerced to a shingle.
  }
  \item{panel}{ panel function}
  \item{data}{ data frame for variables in the call}
  \item{\dots}{ other arguments}
}
\details{
  see the documentation for \code{trellis.args} as well as \code{bwplot}
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{panel.dotplot}},
  \code{\link{Lattice}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(VADeaths)
deathrate <- as.vector(VADeaths)
age <- rep(rownames(VADeaths), ncol(VADeaths))
type <- as.ordered(c(rep("Rural Male", 5),
          rep("Rural Female", 5),
          rep("Urban Male", 5),
          rep("Urban Female", 5)))
dotplot(age ~ deathrate | type, layout=c(1,4))
dotplot(type ~ deathrate | age, layout=c(1,5), ylab=NULL)
data(barley)
dotplot(variety~yield|year*site, data=barley)
dotplot(variety ~ yield | site, data = barley, groups = year,
        panel = function(x, y, subscripts, ...) {
            dot.line <- trellis.par.get("dot.line")
            panel.abline(h = y, col = dot.line$col,
                         lty = dot.line$lty)
            panel.superpose(x, y, subscripts, ...)
        },
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)
}
\keyword{hplot}

