% $Id: shingles.Rd,v 1.5 2001/05/17 03:07:09 deepayan Exp $
\name{shingles}
\alias{equal.count}
\alias{plot.shingle}
\alias{as.shingle}
\alias{is.shingle}
\alias{shingle}
\title{shingles}
\usage{
shingle(x, intervals=sort(unique(x)))
equal.count(x, \dots)
as.shingle(x)
is.shingle(x)
\method{plot}{shingle}(x, col)
}
\description{
  Functions to handle shingles
}
\arguments{
  \item{x}{ numeric variable or R object, shingle in
    \code{plot.shingle}} 
  \item{intervals}{ numeric vector or matrix with 2 columns}
  \item{col}{ color to fill the rectangles, defaults to
    \code{bar.fill$col}}
  \item{\dots}{ other arguments, passed to \code{co.intervals}}
}
\details{ \code{equal.count} Converts \code{x} to a
  shingle. Essentially a wrapper around  
  \code{co.intervals}. All arguments are passed to \code{co.intervals}

  \code{shingle} creates a shingle using the given \code{intervals}. If
  \code{intervels} is a vector, these are used to form 0 length
  intervals.

  \code{as.shingle} returns \code{shingle(x)} if \code{x} is not a
  shingle.

  \code{is.shingle} tests whether \code{x} is a shingle.

  \code{plot.shingle} displays the ranges of shingles via rectangles.
}
\value{
  logical for \code{is.shingle}, an object of class ``trellis'' for
  \code{plot} (printed by default by \code{print.trellis}), and 
  an object of class ``shingle'' for the others.
}
\examples{
plot(equal.count(rnorm(100)))
}
\note{
  A shingle is a data structure used in Trellis. It consists of a
  numeric vector along with some possibly overlapping intervals.
  }
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.args}},
  \code{\link{co.intervals}}, \code{\link{Lattice}} 
}
\keyword{dplot}


