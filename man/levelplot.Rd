% $Id: levelplot.Rd,v 1.9 2002/01/11 04:04:30 deepayan Exp $
\name{levelplot}
\title{Level Plots}
\synopsis{
levelplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = "panel.levelplot", prepanel = NULL, scales = list(), 
    strip = TRUE, groups = NULL, xlab, xlim, ylab, ylim, at, 
    col.regions = terrain.colors(length(at) - 1), colorkey = NULL, 
    contour = FALSE, cuts = 15, labels = format(at), pretty = FALSE, 
    region = TRUE, ..., subscripts = TRUE, subset = TRUE) 
}
\usage{
levelplot(formula,
          at,
          col.regions = terrain.colors(length(at)-1),
          colorkey = NULL,
          contour = FALSE,
          cuts = 15,
          labels = format(at),
          pretty = FALSE,
          region = TRUE,
          ...)
}
\description{
  Draw Level Plots. Contourplots are as yet unimplemented.
}
\alias{levelplot}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g1 * g2 * ...},
    where \code{z} is a numeric response, and \code{x, y} are numeric
    values evaluated on a rectangular grid. 
    
    \code{g1,g2,...}, if present, must be either factors or shingles.

    Calculations are based on the assumption that all x and y values are
    evaluated on a grid (defined by \code{(unique(x))} and
    \code{(unique(y))}. The function will not return an error if this is
    not true, but the display will contain lots of blank spaces.

    However, the x and y values need not be equally spaced. See example
    below with log scales.

    As an extension to partially support the form used in
    \code{filled.contour} and \code{image}, \code{formula} can be a
    matrix.
  }
  \item{at}{
    numeric vector giving breaks along the range of \code{z}. Contours
    (if any) will be drawn at these heights, and the regions in between
    would be colored using \code{col.regions}.
  }
  \item{col.regions}{color vector to be used if regions is TRUE}
  \item{colorkey}{logical specifying whether a color key is to be drawn
    alongside the plot, or a list describing the color key. The list may
    contain the following components:

    \code{space} can only be "right" now.
    
    \code{x,y} location, unused

    \code{col} vector of colors

    \code{at} numeric vector specifying where the colors change. must be
    of length 1 more than the col vector.

    \code{labels} a character vector for labelling the \code{at} values,
    or more commonly, a list of components \code{labels, at, cex, col,
      font} describing label positions.

    \code{tick.number} approximate number of ticks.

    \code{corner} interacts with x, y; unimplemented

    \code{width} width of the key in terms of character widths

    \code{height} length of key w.r.t side of plot. Currently ignored,
    using a value of 1.
  }
  \item{contour}{ logical, whether to draw contour lines. TRUE not
    inplemented yet.
  }
  \item{cuts}{ number of levels the range of \code{z} would be divided into}
  \item{labels}{ character vector of labels for contour lines, not
    implemented yet.}
  \item{pretty}{ logical, whether to use pretty labels, not implemented}
  \item{region}{ logical, whether regions between contour lines should
    be filled
  }
  \item{\dots}{other arguments}
}
\details{
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage. 
}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}}, \code{\link{panel.levelplot}} 
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
x <- seq(pi/4, 5*pi, length = 100)
y <- seq(pi/4, 5*pi, length = 100)
r <- sqrt(outer(x^2, y^2, "+"))
grid <- expand.grid(x=x, y=y)
grid$z <- cos(r^2) * exp(-r/(pi^3))
levelplot(z~x*y, grid, cuts = 50, xlab="", ylab="",
          main="Weird Function", colorkey = FALSE)
levelplot(z~x*y, grid, cuts = 50, scales=list(log="e"), xlab="",
          ylab="", main="Weird Function", sub="with log scales",
          colorkey = FALSE)
#S+ example
data(environmental)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
       parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length = 50)
t.marginal <- seq(min(temperature), max(temperature), length = 50)
r.marginal <- seq(min(radiation), max(radiation), length = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
        radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
levelplot(fit ~ wind * temperature | radiation, data = grid,
          contour = T, labels = F,
          xlab = "Wind Speed (mph)",
          ylab = "Temperature (F)",
          main = "Cube Root Ozone (cube root ppb)",
          col.regions = trellis.par.get("regions")$col)
detach()
}
\keyword{hplot}



