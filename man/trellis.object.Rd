\name{trellis.object}
\alias{trellis.object}
\non_function{}
\title{Lattice Display Object}
\description{
  This class of objects is returned by Lattice display functions, and is
  usually plotted by default by its `print' method.
}
\value{
  A ``trellis'' object is a list with the following components:
  \item{formula}{ formula that was used in the call. Not used}
  \item{fname}{ function that produced the object. Not used}
  \item{aspect.fill}{ logical specifying whether aspect==``fill''}
  \item{aspect.ratio}{ numeric, aspect ratio to be used IF aspect.fill
    is FALSE}
  \item{as.table }{ logical, whether rows are drawn top to bottom}
  \item{cond     }{ The conditioning variables. Not used}
  \item{key      }{ list defining the key to be drawn}
  \item{layout   }{ layout of the trellis. Must be of length 3, but
    first element can be 0, in which case the second element is taken to
    be the desired number of panels per page and a `nice' layout is
    calculated based on aspect ratio, screen dimensions etc.}
  \item{page     }{ the page function}
  \item{panel    }{ the panel function}
  \item{panel.args     }{ a list as long as the number of panels, each
    element being a list itself, containing the arguments in named form
    to be passed to the panel function in that panel. If \code{skip=T}
    for a particular panel, then the corresponding element is
    \code{FALSE} instead of a list.}
  \item{panel.args.common }{ a list containing the arguments common to
    all the panel functions in name=value form}
  \item{par.strip.text }{ list containing graphical parameters
    \code{cex, col, font} for the strip labels}
  \item{skip     }{ logical vector, not used}
  \item{strip    }{ the function to draw the strip labels}
  \item{main     }{ \code{NULL}, or list with components \code{label,
      cex, col, font}}
  \item{sub      }{ same as main}
  \item{xlab     }{ same as sub}
  \item{ylab     }{ same as xlab}
  \item{x.draw   }{ logical specifying whether x-axis is to be drawn}
  \item{y.draw   }{ logical specifying whether y-axis is to be drawn}
  \item{x.scales }{ list describing x-scale, can consist of several
    other lists, paralleling panel.args, if x-relation is not ``same''}
  \item{y.scales }{ list describing y-scale, same as x.scales}
  \item{x.between}{ numeric vector of interpanel x-space}
  \item{y.between}{ numeric vector of interpanel y-space}
  \item{x.relation.same}{ logical, whether x-relation = "same"}
  \item{y.relation.same}{ logical, whether y-relation = "same"}
  \item{x.alternating  }{ numeric vector, with components 0, 1 or 2,
    defining on which side x-axis labels are to be drawn}
  \item{y.alternating  }{ numeric vector, with components 0, 1 or 2,
    defining on which side y-axis labels are to be drawn}
  \item{fontsize.normal}{ default fontsize}
  \item{fontsize.small }{ fontsize for the axis labels, can be further
    modified by the \code{cex} component of \code{scales}.}
}
\keyword{datasets}

