\name{update.trellis}
\alias{update.trellis}
\title{Update Trellis Object}
\description{
  Takes an object of class \code{trellis} and updates it by changing
  some of its components.
}
\usage{
\method{update}{trellis}(object,
       aspect,
       as.table,
       between,
       key,
       layout,
       main,
       page,
       panel,
       par.strip.text,
       scales,
       skip,
       strip,
       sub,
       xlab,
       xlim,
       ylab,
       ylim,
       ...)
}
\arguments{
  \item{object}{ The object to be updated, of class \code{trellis}. }

  The following arguments are described in \code{trellis.args}. Not all
  arguments accepted by a Lattice function are processed by
  \code{update}. Only the following are, with certain restrictions:
  
  \item{aspect}{ Cannot be "xy", can be "fill" or numeric.}
  \item{as.table, between, key}{}
  \item{layout, main, page, panel}{}
  \item{par.strip.text, scales, skip, strip}{}
  \item{sub, xlab, ylab, xlim, ylim}{As described in \code{trellis.args}
  }
  \item{\dots}{ extra arguments, ignored}
}
\details{
  Not implemented very well yet, should work better in subsequent
  releases 
}
\value{
  An object of class \code{trellis}, by default plotted by
  \code{print.trellis}.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.object}}, \code{\link{Lattice}}
}
\keyword{dplot}
