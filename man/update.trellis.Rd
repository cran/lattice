\name{update.trellis}
\alias{update.trellis}
\title{Update Trellis Object}
\description{
  Takes an object of class \code{trellis} and updates it by changing
  some of its components.
}
\usage{
\method{update}{trellis}(foo,
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
       ylim)
}
\arguments{
  \item{foo}{ The object to be updated, of class \code{trellis}. }

  The following arguments are described in \code{trellis.args}. Not all
  arguments accepted by a Lattice function are processed by
  \code{update}. Only the following are, with certain restrictions:
  
  \item{aspect}{ Cannot be "xy", can be "fill" or numeric.}
  \item{as.table}{ As described in \code{trellis.args}}
  \item{between}{ As described in \code{trellis.args}}
  \item{key}{ As described in \code{trellis.args} }
  \item{layout}{ As described in \code{trellis.args} }
  \item{main}{  As described in \code{trellis.args} }
  \item{page}{  As described in \code{trellis.args} }
  \item{panel}{ As described in \code{trellis.args} }
  \item{par.strip.text}{  As described in \code{trellis.args} }
  \item{scales}{ Mostly ignored if relation is not "same" }
  \item{skip}{ As described in \code{trellis.args} }
  \item{strip}{ As described in \code{trellis.args} }
  \item{sub}{ As described in \code{trellis.args} }
  \item{xlab}{ As described in \code{trellis.args} }
  \item{xlim}{ As described in \code{trellis.args} }
  \item{ylab}{ As described in \code{trellis.args} }
  \item{ylim}{ As described in \code{trellis.args} }
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
