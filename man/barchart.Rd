\name{barchart}
\title{Bar Graphs}
\usage{
barchart(formula,
         panel = panel.barchart,
         data = sys.frame(sys.parent()), 
         box.ratio =2,
         \dots)
}
\description{
  Draw Bar plots
}
\alias{barchart}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | g1 * g2 * ...} indicates that plots of
    \code{y} versus \code{x} should be produced conditional on the
    variable \code{g1,g2,...}.
    
    \code{x} must be numeric, and each of \code{g1,g2,...} must
    be either factors or shingles. \code{y} can be a factor or a
    shingle, but there should be only one value of \code{x} per level of
    \code{y} for each panel. If not, the highest value will be used.
  }
  \item{panel}{ panel function to be used to draw panels}
  \item{data}{ data frame for the variables in the formula etc}
  \item{box.ratio}{ ratio of bar width to inter bar width}
  \item{\dots}{ other arguments}
}
\details{
  see the documentation for \code{trellis.args}.
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\note{
  The x-axis limits might not be meaningful. The bars are drawn from the
  extreme left of the panel, whatever be the x-limits.
}
\seealso{
  \code{\link{trellis.args}},
  \code{\link{panel.barchart}},\code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(barley)
barchart(variety ~ yield | year * site, data = barley, aspect = 0.4,
        xlab = "Barley Yield (bushels/acre)")
}
\keyword{hplot}
