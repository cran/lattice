\name{stripplot}
\title{Strip Plots}
\synopsis{
stripplot(formula, data = parent.frame(), panel = "panel.stripplot", 
    prepanel = NULL, strip = TRUE, jitter = FALSE, factor = 0.5, 
    box.ratio = if (jitter) 1 else 0, groups = NULL, ..., subset = TRUE)  
}
\usage{
stripplot(formula,
          data = parent.frame(),
          jitter = FALSE,
          factor = .5,
          box.ratio = if (jitter) 1 else 0,
          ...)
}
\description{
  Draw Strip plots (One dimensional scatter plots)
}
\alias{stripplot}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula of the form \code{y ~ x | g1 * g2 * ...} indicates that
    stripplots of \code{x} should be produced for each level of \code{y} 
    conditional on the variables \code{g1,g2,...}.
    
    \code{x} must be numeric, and each of \code{g1,g2,...} must
    be either factors or shingles. \code{y} can be a factor or a
    shingle. If \code{y} is a character vector, it will be coerced into
    a factor; if it is numeric, it will be coerced to a shingle.}
  \item{data}{ optional data frame in which the elements in the formula
    and some other arguments are to be evaluated}
  \item{jitter}{ logical specifying whether the values should be jittered
    by adding a random noise.}
  \item{factor}{ numeric controlling amount of jitter.}
  \item{box.ratio}{ as in \code{bwplot}}
  \item{\dots}{other arguments}
}
\details{
  see the documentation for \code{trellis.args}.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{panel.stripplot}},
  \code{\link{jitter}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(singer)
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter = T, xlab = "Height (inches)")
}
\keyword{hplot}



