% $Id: xyplot.Rd,v 1.4 2001/05/15 16:57:14 deepayan Exp $
\name{xyplot}
\title{Conditioning Plots}
\synopsis{
xyplot(formula, aspect = "fill", as.table = FALSE, between = list(x = 0, 
    y = 0), data, groups = NULL, key = NULL, layout, main = NULL, 
    page = NULL, panel = panel.xyplot, par.strip.text, prepanel, 
    scales, skip = FALSE, strip = strip.default, sub = NULL, 
    subscripts = !missing(groups), subset, xlab, xlim, ylab, 
    ylim, ...) 
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
## panel.superpose. doesn't work very well as there are too many
## values of the grouping variable. See splom example.
xyplot(long ~ lat | Depth, groups = mag, panel = panel.superpose, data=quakes)
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
xyplot(NOx ~ C | EE, data = ethanol, subset = C>8) # subset
data(melanoma)
## banking
xyplot(incidence ~ year, data = melanoma, aspect="xy",
       xlab = "Year", ylab = "Incidence", ylim = c(0,5))
data(sunspot)
xyplot(sunspot ~ 1:37 ,type = "l", aspect="xy")
data(state)
## user defined panel functions (needs knowledge of G2)
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



