\name{draw.colorkey}
\alias{draw.colorkey}
\title{Produce a Colorkey for levelplot}
\description{
  Produces (and possibly draws) a Grid frame grob which is a colorkey
  that can be placed in other Grid plots.
}
\usage{
draw.colorkey(key, draw=FALSE, vp=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{key}{ A list determining the key. See documentation for
    \code{trellis.args}, in particular the section describing the `key'
    argument for details.
  }
  \item{draw}{ logical, whether the grob is to be drawn. }
  \item{vp}{ viewport. Ignored right now(?)(don't remember) }
}
\details{
  Maybe later :)
}
\value{
  A Grid frame object (that inherits from ``grob'')
}
\author{Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{trellis.args}}}
\keyword{dplot}

