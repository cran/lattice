\name{lattice-Internal}
\alias{Rows}
\alias{extend.limits}
\alias{lpretty}
\alias{cupdate}
\alias{.lattice.print.more}
\alias{do.breaks}
\alias{compute.layout}
\alias{construct.scales}
\alias{limits.and.aspect}
\alias{trellis.skeleton}
\alias{make.list.from.intervals}
\alias{prepanel.default.bwplot}
\alias{prepanel.default.levelplot}
\alias{prepanel.default.cloud}
\alias{prepanel.default.densityplot}
\alias{prepanel.default.histogram}
\alias{prepanel.default.parallel}
\alias{prepanel.default.qq}
\alias{prepanel.default.qqmath}
\alias{prepanel.default.splom}
\alias{prepanel.default.tmd}
\alias{prepanel.default.xyplot}
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
construct.scales(draw = TRUE, tck = 1, tick.number = 5, cex = 1, rot = FALSE, 
    at = FALSE, labels = FALSE, col = FALSE, log = FALSE, alternating = TRUE, 
    relation = "same", x = NULL, y = NULL, ...)  
compute.layout(layout, cond.max.level, skip = FALSE)  
limits.and.aspect(prepanel.default.function, prepanel = NULL, have.xlim = FALSE, 
    xlim = NULL, have.ylim = FALSE, ylim = NULL, x.relation, 
    y.relation, panel.args.common = list(), panel.args = list(), 
    aspect, nplots, ...)
trellis.skeleton(as.table = FALSE, aspect = "fill", between = list(x = 0, 
    y = 0), key = NULL, page = NULL, main = NULL, sub = NULL, 
    par.strip.text = list(), skip = FALSE, strip = strip.default, 
    xlab = NULL, ylab = NULL, panel, ...)
}
\usage{
Rows(x, which)
extend.limits(lim, length = 1, prop = 0.07) 
lpretty(lim)
.lattice.print.more
cupdate(index, maxim)
do.breaks(endpoints, nint)
ldrop.levels(x, subset)
compute.layout(...)
make.list.from.intervals(x)
construct.scales(...)
limits.and.aspect(...)
trellis.skeleton(...)
prepanel.default.bwplot(...)
prepanel.default.levelplot(x, y, wx, wy, subscripts, ...)
prepanel.default.cloud(distance, xlim, ylim,
        zlim, zoom = 1, rot.mat = rot.mat, 
        aspect = aspect,...)
prepanel.default.densityplot(...)
prepanel.default.histogram(...)
prepanel.default.parallel(...)
prepanel.default.qq(...)
prepanel.default.qqmath(...)
prepanel.default.splom(...)
prepanel.default.tmd(...)
prepanel.default.xyplot(x, y, type, ...)
}
\arguments{
  \item{length}{ desired length of the resulting interval, with center
    given by the center of lim. Used for relation="sliced".
  }
  \item{prop}{ Proportion by which to extend limits. 
  }
  \item{x,y}{ numeric, factor or shingle.
  }
  \item{subset}{ indices to keep.
  }
  \item{which}{logical ?}
  \item{lim}{length 2 numeric}
  \item{index}{current values of levels}
  \item{maxim}{max values of levels}
  \item{endpoints}{see histogram}
  \item{nint}{see histogram}
  \item{zoom, rot.mat,aspect}{see panel.cloud}
  \item{distance, xlim, ylim, zlim}{see panel.cloud}
  \item{wx, wy, subscripts}{see panel.levelplot}
  \item{type}{ type argument as in \code{panel.xyplot}}
  \item{\dots}{other arguments}
}
\details{
  Some of these functions are indigenous, some are counterparts of
  functions in the S-Plus Trellis Displays library.

  \code{lpretty} is used instead of pretty (which gives very non-pretty
  results in some cases, particularly densityplot).
  
  \code{ldrop.levels} returns an object of the same class as \code{x},
  with the redundant factor/shingle levels dropped, if any. Used to
  avoid empty panels when using the \code{subset} argument.

  \code{.lattice.print.more} is a global logical to handle the
  \code{more} argument in print.trellis.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{Lattice}}}
\keyword{dplot}
















