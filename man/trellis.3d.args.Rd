\name{trellis.3d.args}
\title{Arguments to 3d Trellis Functions}
\usage{
trellis.3d.function(formula,
                    data = parent.frame(),
                    aspect = c(1, 1),
                    scales = list(cex = 0.5, lty = 1, lwd = 1,
                                 col = "black", distance = rep(1, 3),
                                 arrows = TRUE),
                    zlab,
                    zlim = range(z), 
                    distance = 0.2,
                    par.box,
                    perspective = TRUE,
                    R.mat = diag(4), 
                    screen = list(z = 40, x = -60),
                    zoom = 1)
}
\description{
  Arguments common to the 3d Trellis display functions \code{cloud} (and
  the as yet unimplemented wireframe)
}
\alias{trellis.3d.args}
\arguments{
  \item{formula}{ formula of the form \code{z ~ x * y | g_1 * g_2 * \dots}}
  \item{data}{ data frame in which variables are evaluated.}
  \item{aspect}{ vector of length 2, giving the relative aspects of the
    y-size/x-size and z-size/x-size of the enclosing rectangle.}
  \item{scales}{describes scales. not used yet }
  \item{zlab}{z label}
  \item{zlim}{z limits}
  \item{distance}{ between 0 and 1, controls amount of perspective. No
    connection with the physical concept of distance in this
    implementation (not clear what S+ does). individual values don't give
    the same results as S-Plus, but all instances should be achievable.}
  \item{par.box}{graphical parameters for box, not implemented yet}
  \item{perspective}{logical, whether to plot a perspective view}
  \item{R.mat}{initial rotation matrix (homogeneous coordinates). Not
    implemented. }
  \item{screen}{A list determining the rotations to applied to the data
    before being plotted. The initial position starts with the view
    somewhere in the positive z-axis, and the x and y axes in the usual
    position. Each component of the list should be named one of \code{x,
      y, z}, with their values indicating the amount of rotation about
    that axis in degrees.   }
  \item{zoom}{factor by which to scale the picture.}
}
\details{
  These functions also take other arguments, as described in the
  documentation for \code{trellis.args}.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{hplot}





