\name{trellis.device}
\alias{trellis.device}
\title{ Initializing Trellis Displays }
\description{
  \code{trellis.device} initializes a Trellis display device with
  appropriate settings of the graphics control parameter
  \code{trellis.settings}.
}
\usage{
trellis.device(device, color = T, bg, new = T, ...)
}
\arguments{
  \item{device}{ device to initialize (possibly as a character string).
    Admissible values are "x11", "windows", "mac" and "postscript". 
    }
  \item{color}{ whether the display should be color or black and white.
    }
  \item{bg}{ background color for the device. Can be changed by par(bg=).
  }
  \item{new}{ logical flag indicating whether a new device should be
    startted. If \code{new = F} is specified, then the options for the
    current device are changed to the defaults determined by the other
    arguments.
    }
  \item{\dots}{ additional parameters to be passed to the \code{device}
    function. See \code{postscript}.
    }
}
\details{
  \code{trellis.device} calls the \code{device} function to start a
  display, sets the global variable \code{trellis.settings} to either of
  two possible values depending on whether \code{color} is set, and
  initializes the underlying \code{Grid}
  display. \code{trellis.settings} can subsequently be handled by the 
  interface functions \code{trellis.par.get} and \code{trellis.par.set}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{trellis.par.set}}, \code{\link[grid]{Grid}},
  \code{\link{postscript}},\code{\link{x11}}, \code{\link{Lattice}}
}
\keyword{dplot}

