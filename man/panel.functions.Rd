\name{panel.functions}
\alias{panel.abline}
\alias{panel.fill}
\alias{panel.grid}
\alias{panel.lmline}
\alias{panel.loess}
\alias{panel.linejoin}
\alias{panel.mathdensity}
\title{Useful Panel Functions}
\description{
  These are predefined panel functions available in Trellis for use in
  constructing new panel functions (usually on-the-fly).
}
\synopsis{
panel.abline(a, b = NULL, h = numeric(0), v = numeric(0),
    col = add.line$col,
    lty = add.line$lty,
    lwd = add.line$lwd, 
    ...)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col = reference.line$col, col.line, 
    ...)
}
\usage{
panel.abline(a, b, ...)
panel.abline(coef, ...)
panel.abline(reg, ...)
panel.abline(h= ,v= , ...)
panel.fill(col="grey", ...)
panel.grid(h=3, v=3, ...)
panel.lmline(x,y,...) = panel.abline(lm(y~x),...)
panel.loess(x, y, span = 2/3, degree = 1, family = c("symmetric", 
            "gaussian"), evaluation = 50, \dots)
panel.linejoin(x, y, fun = mean, horizontal = TRUE, ...)
panel.mathdensity(dmath = dnorm, args = list(mean=0, sd=1))
}
\arguments{
  \item{x, y}{ numeric variates}
  \item{a, b}{ Coefficients of the line to be added}
  \item{coef}{ Coefficients of the line to be added as a length 2 vector}
  \item{reg}{ A regression object. The corresponding fitted line will
    be drawn }
  \item{h,v}{ For panel.abline, numerical vectors giving y and x
    locations respectively of horizontal and vertical lines to be
    added to the plot. For panel.grid, number of horizontal and vertical
    reference lines to be added to the plot; h=-1 and v=-1 make the
    grids aligned with the axis labels (this doesn't always work).
  }
  \item{col}{color}
  \item{span, degree, family, evaluation}{ arguments to 
    \code{loess.smooth} in the \code{modreg} library. \code{panel.loess}
    is essentially a wrapper on \code{loess.smooth} 
  }
  \item{fun}{ the function that will be applied to the subset of x(y)
    determined by the unique values of y(x)}
  \item{horizontal}{ logical. If FALSE, the plot is `transposed' in the
    sense that the behaviours of x and y are switched. x is now
    the `factor'.  Interpretation of other arguments change
    accordingly. See documentation of \code{bwplot} for a fuller
    explanation.
  }
  \item{dmath}{ A vectorized function that produces density values
    given a numeric vector named \code{x}, e.g., \code{dnorm}}
  \item{args}{ list giving additional arguments to be passed to dmath}
  \item{\dots}{ graphical parameters can be supplied. see function
    definition for details. 
  }
}
\details{
  \code{panel.abline} adds a line of the form \code{y=a+bx} or vertical
  and/or horizontal lines. Graphical parameters are obtained from 
  \code{reference.line} for panel.grid, and \code{add.line} for the
  others (can be set using \code{trellis.par.set} )

  \code{panel.linejoin} treats one of x and y as a factor (according to
  the value of \code{horizontal}, calculates \code{fun} applied to the
  subsets of the other variable determined by each unique value of the
  factor, and joins them by a line. Can be used in conjunction with
  \code{panel.xyplot} and more commonly with panel.superpose to produce
  interaction plots. See \code{\link{xyplot}} documentation for an example.

  \code{panel.mathdensity} plots a (usually theoretical) probability
  density function. Can be useful in conjunction with \code{histogram}
  and \code{densityplot} to visually estimate goodness of fit.
}
\seealso{\code{\link[modreg]{loess.smooth}},
  \code{\link{trellis.par.get}}}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}




