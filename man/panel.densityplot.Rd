\name{panel.densityplot}
\alias{panel.densityplot}
\title{ Default Panel Function for densityplot }
\description{
  This is the default panel function for \code{densityplot}.
}
\usage{
panel.densityplot(x, darg, plot.points = TRUE, ref = FALSE, ...)
}
\arguments{
  \item{x}{ data points for which density is to be estimated}
  \item{darg}{list of arguments to be passed to the \code{density}
    function. Typically, this should be a list with some of the
    following components : bw,adjust, kernel, window, width,
    give.Rkern, n, from, to, cut, na.rm (see \code{density} for
    details)
} 
  \item{plot.points}{ logical, specifying whether or not the data 
    points should be plotted along with the estimated density. The
    default (TRUE) is S-Plus behaviour.}
  \item{ref}{ logical, whether to draw x-axis}
  \item{\dots}{ parameters to be passed to the function \code{density},
    as well as graphical parameters like \code{col}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{\code{\link{densityplot}}}
\keyword{dplot}
















