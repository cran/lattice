\name{print.trellis}
\title{Print Trellis Objects}
\usage{
\method{print}{trellis}(x, position, split, more = FALSE,
  newpage = TRUE, ...)
}
\description{
  Print (plot) a trellis object.
}
\alias{print.trellis}
\arguments{
  \item{x}{ The object to be plotted, of class ``trellis''}
  \item{position}{ a vector of 4 numbers, typically c(xmin, ymin, xmax,
    ymax) that give the lower-left and upper-right corners of a
    rectangle in which the Trellis plot of x is to be positioned. The
    coordinate system for this rectangle is [0-1] in both the x and y
    directions.
  }
  \item{split}{ a vector of 4 integers, c(x,y,nx,ny) , that says to
    position the current plot at the x,y position in a regular array of
    nx by ny plots. (Note: this has origin at top left)
  }
  \item{more}{ A logical specifying whether more plots will follow on
    this page.
  }
  \item{newpage}{ A logical specifying whether the plot should be on a
    new page. This option is specific to lattice, and is useful for
    including lattice plots in an arbitrary grid viewport (see the
    details section).
  }
  \item{\dots}{extra arguments, ignored}
}
\details{
  This is the default print method for objects of class
  \code{"trellis"}, produced by calls to functions like \code{xyplot,
    bwplot} etc. It is usually called automatically when a trellis
  object is produced. It can also be called explicitly to control plot
  positioning by means of the arguments \code{split} and
  \code{position}.
}
\note{ Trying to position multipage displays will
mess things up.
  }
\seealso{
  \code{\link{Lattice}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(singer)
p11 <- histogram( ~ height | voice.part, data = singer, xlab="Height")
p12 <- densityplot( ~ height | voice.part, data = singer, xlab = "Height")
p2 <- histogram( ~ height, data = singer, xlab = "Height")
data(sunspot)
p3 <- xyplot(sunspot~1:37, aspect="xy", type = "l")
## simple positioning by split
print.trellis(p11, split=c(1,1,1,2), more=TRUE)
print.trellis(p2, split=c(1,2,1,2))

## Combining split and position:
print.trellis(p11, position = c(0,0,.75,.75), split=c(1,1,1,2), more=TRUE)
print.trellis(p12, position = c(0,0,.75,.75), split=c(1,2,1,2), more=TRUE)
print.trellis(p3, position = c(.5,.75,1,1), more=FALSE)

## Embedding lattice plots inside a grid viewport
## Note: this is lattice specific, won't work in S-Plus

data(iris)
cur.settings <- trellis.par.get()

grid.newpage()
lset(list(background = list(col = "transparent")))

grid.rect(gp = gpar(fill = "#fffff0"))

push.viewport(viewport(x = .6, y = .8, h = .25, w = .8, angle = 5))
print(densityplot(~ Petal.Length | Species, iris, plot.p = FALSE, col = 4,
      layout = c(3, 1)), newpage = FALSE)
pop.viewport()

push.viewport(viewport(x = .6, y = .6, h = .25, w = .75, angle = 25))
print(densityplot(~ Petal.Width | Species, iris, plot.p = FALSE, col = 3,
      layout = c(3, 1)), newpage = FALSE)
pop.viewport()

push.viewport(viewport(x = .6, y = .3, h = .25, w = .7, angle = -5))
print(densityplot(~ Sepal.Length | Species, iris, plot.p = FALSE,  col = 2,
      layout = c(3, 1)), newpage = FALSE)
pop.viewport()

push.viewport(viewport(x = .15, y = .5, h = .8, w = .25, angle = 10))
lset(list(background = list(col = "#f0ffff")))
print(densityplot(~ Sepal.Width | Species, iris, plot.p = FALSE, col = 1,
      layout = c(1, 3)), newpage = FALSE)
grid.rect()
pop.viewport()

grid.text(lab = "Densities by Species in the Iris Data",
          vp = viewport(x = .6, y = .1))



lset(cur.settings)

}
\keyword{hplot}



