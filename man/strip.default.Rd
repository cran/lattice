% $Id: strip.default.Rd,v 1.4 2001/05/15 16:57:14 deepayan Exp $
\name{strip.default}
\title{Default Trellis strip function}
\usage{
strip.default(x, level, style = 1,
              strip.names = is.shingle(x), name="", bg, fg,
              par.strip.text)

}
\description{ function that draws strips in trellis functions.
}
\alias{strip.default}
\arguments{
  \item{x}{ the conditioning variable to which the strip corresponds.
  }
  \item{level}{ current level of the conditioning variable \code{x}.
  }
  \item{style}{ integer, with values 1,2,3 and 4 currently supported.
    Applicable only when \code{x} is a factor. Determines how the current
    level of \code{x} is indicated on the strip. Details later. Note
    that unlike S-Plus the default value is 1.
  }
  \item{strip.names}{ logical specifying whether the name of the
    current conditioning variable should be included on the strip.
    Ignored for \code{style}=2 and 4 when \code{x} is a factor.
  }
  \item{name}{ the name of the conditioning variable for the current strip
  }
  \item{par.strip.text}{ list with parameters controlling the text on
    each strip, with components \code{col, cex}
  }
  \item{bg}{ strip background color.
  }
  \item{fg}{ strip foreground color.
  }
}
\details{ default strip function for trellis functions. Useful 
  mostly because of the \code{style} argument --- non-default styles
  are often more informative, especially when the names of the levels
  of the factor \code{x} are small. Typical use is as 
  \code{strip = function(\dots) strip.default(style=2,\dots)}
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}


