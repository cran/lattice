\name{lset}
\alias{lset}
\alias{lset.textfont}
\alias{lset.textcol}
\alias{lset.textcex}
\title{Simple Interfaces to modify Trellis Settings and Lattice Themes}
\description{
  These functions are simple interfaces to do some of the more common
  tasks that can be achieved by \code{trellis.par.get/set}. These are
  NOT S-compatible
}
\usage{
lset(theme = c("col.bg", "white.bg", "bw"), file)
lset.textfont(xlab, ylab, main, sub, add)
lset.textcol(xlab, ylab, main, sub, add)
lset.textcex(xlab, ylab, main, sub, add)
}
\arguments{
  \item{file}{ the name of a file that will be \code{source()}d.
  }
  \item{theme}{ a character string specifying the name of a predefined
    `theme'. This is at a very early stage of development, and not many
    interesting themes are available yet.
  }
  \item{xlab, ylab, main, sub, add}{ intended replacement values for
    respective components of \code{trellis.settings}
  }
}
\details{
  These functions are meant to be interfaces to
  \code{trellis.settings}. \code{lset} deals with the whole list of
  settings. \code{lset.text*} modifies components of the various text
  components. More similar functions should come later.

  \code{lset} implements a concept of `themes' in Lattice. Themes are
  predefined values of \code{trellis.settings} that modify the overall
  look. The only current theme significantly different from the defaults
  is the \code{"white.bg"} theme, which hopefully 
}
\seealso{\code{\link{trellis.settings}}, \code{\link{Lattice}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}
