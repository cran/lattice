% $Id: xyplot.Rd,v 1.11 2001/09/17 03:11:47 deepayan Exp $
\name{xyplot}
\title{Conditioning Plots}
\synopsis{
xyplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.xyplot, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, ..., subscripts = !is.null(groups), 
    subset = TRUE)
}
\usage{
xyplot(formula, ...)
}
\description{
  Draw conditioning plots
}
\alias{xyplot}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | g1 * g2 * ...} indicates that plots of
    \code{y} versus \code{x} should be produced conditional on the
    variable \code{g1,g2,...}.
    
    \code{x} and \code{y} must be numeric, and each of \code{g1,g2,...} must
    be either factors or shingles.
  }
  \item{\dots}{other arguments}
}
\details{
  see the documentation for \code{trellis.args}
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
## Tonga Trench Earthquakes
data(quakes)
xyplot(long ~ lat , data = quakes)
Depth <- equal.count(quakes$depth, number=8, overlap=.1)
## Example with panel.superpose. 
data(iris)
xyplot(Petal.Length~Petal.Width, data = iris, groups=Species, 
       panel = panel.superpose,
       type = c("p", "smooth"), span=.75,
       col.line = trellis.par.get("strip.background")$col,
       col.symbol = trellis.par.get("strip.shingle")$col,
       key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
           border = TRUE, 
           points = list(col=trellis.par.get("strip.shingle")$col[1:3],
                         pch = 1),
           text = list(levels(iris$Species))))
## Examples with data from `Visualizing Data' (Cleveland)
## (obtained from Bill Cleveland's Homepage :
## http://cm.bell-labs.com/cm/ms/departments/sia/wsc/, also
## available at statlib)
data(ethanol)
xyplot(NOx ~ E, data = ethanol)
xyplot(NOx ~ E | C, data = ethanol)
EE <- equal.count(ethanol$E, number=9, overlap=1/4)
## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
           panel.grid(h=-1, v= -1)
           panel.xyplot(x, y)
           panel.loess(x,y, span=1)
       },
       aspect = "xy")
xyplot(NOx ~ C | EE, data = ethanol, subset = C>8,
       main = "Example using subset")  # subset
data(melanoma)
## banking
xyplot(incidence ~ year, data = melanoma, aspect="xy",
       xlab = "Year", ylab = "Incidence", sub = "log scales",
       scales = list(y = list(log="e")))
data(sunspot)
xyplot(sunspot ~ 1:37 ,type = "l", aspect="xy")
data(state)
## user defined panel functions (needs knowledge of the grid package)
states <- data.frame(state.x77, 
                     state.name = dimnames(state.x77)[[1]], 
                     state.region = state.region) 
xyplot(Murder ~ Population | state.region, data = states, 
       groups = as.character(state.name), 
       panel = function(x, y, subscripts, groups)  
       grid.text(x=x, y=y, label=groups[subscripts],
                 gp = gpar(fontsize = 8),
                 default.units = "native"))
}
\keyword{hplot}
