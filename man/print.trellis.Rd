\name{print.trellis}
\title{Print Trellis Objects}
\usage{
\method{print}{trellis}(x, position, split, more = F, ...)
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
  \item{\dots}{extra arguments, ignored}
}
\details{
  This is the default print method for objects of type trellis, produced
  by calls to functions like \code{xyplot, bwplot} etc. It is usually
  called automatically when a trellis object is produced. It can also be
  called explicitly to control plot positioning by means of the
  arguments \code{split} and \code{position}.
}
\note{ Trying to position multipage displays will mess things up.
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
print.trellis(p11, split=c(1,1,1,2), more=T)
print.trellis(p2, split=c(1,2,1,2))
## Combining split and position:
print.trellis(p11, position = c(0,0,.75,.75), split=c(1,1,1,2), more=T)
print.trellis(p12, position = c(0,0,.75,.75), split=c(1,2,1,2), more=T)
print.trellis(p3, position = c(.5,.75,1,1), more=F)
}
\keyword{hplot}



