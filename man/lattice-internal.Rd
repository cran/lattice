\name{lattice-Internal}
\alias{Rows}
\alias{extend.limits}
\alias{lpretty}
\alias{cupdate}
\alias{.lattice.print.more}
\alias{do.breaks}
\alias{ldrop.levels}
\alias{ldrop.levels.factor}
\alias{ldrop.levels.shingle}
\alias{ldrop.levels.default}
\title{Miscellaneous Functions used within Lattice}
\description{
  These functions are used by various Lattice routines.
}
\synopsis{
lpretty(x, ...)
}
\usage{
Rows(x, which)
extend.limits(lim, length = 1, prop = 0.07) 
lpretty(lim)
.lattice.print.more
cupdate(index, maxim)
do.breaks(endpoints, nint)
ldrop.levels(x, subset)
}
\arguments{
  \item{length}{ desired length of the resulting interval, with center
    given by the center of lim. Used for relation="sliced".
  }
  \item{prop}{ Proportion by which to extend limits. 
  }
  \item{x}{ numeric, factor or shingle.
  }
  \item{subset}{ indices to keep.
  }
  \item{which}{logical ?}
  \item{lim}{length 2 numeric}
  \item{index}{current values of levels}
  \item{maxim}{max values of levels}
  \item{enpoints}{see histogram}
  \item{nint}{see histogram}
}
\details{
  Some of these functions are indigenous, some are counterparts of
  functions in the S-Plus Trellis Displays library.

  \code{lpretty} is used instead of pretty (which gives very non-pretty
  results in some cases, particularly densityplot).
  
  \code{banking} is a crude banking function used when aspect="xy" in
  \code{xyplot}. It just orders the absolute slopes and returns a value
  which when adjusted by the panel scale limits will make the median of
  the above absolute slopes a 45 degree line. (Confused ? see code.)

  \code{ldrop.levels} returns an object of the same class as \code{x},
  with the redundant factor/shingle levels dropped, if any. Used to
  avoid empty panels when using the \code{subset} argument.

  \code{.lattice.print.more} is a global logical to handle the
  \code{more} argument in print.trellis.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{Lattice}}}
\keyword{dplot}
















