\name{cloud}
\title{3d Scatter Plot --- not functional yet}
\synopsis{
cloud(formula, data = parent.frame(), aspect = c(1, 1), layout = NULL, 
    panel = "panel.cloud", subpanel = "panel.xyplot", prepanel = NULL, 
    scales = list(cex = 0.5, lty = 1, lwd = 1, col = "black", 
        distance = rep(1, 3), arrows = TRUE), strip = TRUE, groups = NULL, 
    xlab, xlim = range(x), ylab, ylim = range(y), zlab, zlim = range(z), 
    distance = 0.2, par.box, perspective = TRUE, R.mat = diag(4), 
    screen = list(z = 40, x = -60), zoom = 1, ..., subscripts = TRUE, 
    subset = TRUE) 
}
\usage{
cloud(formula,
      data,
      subpanel = "panel.xyplot",
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
      zoom = 1,
      \dots)
}
\description{
  Draw 3d scatter plots
}
\alias{cloud}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g1 * g2 *
      \dots}}
  \item{data}{data frame in which variables are evaluated}
  \item{subpanel}{the function used to plot the points once coordinates
    of their projections are computed. \code{subpanel="panel.superpose"}
    along with the \code{groups} argument can be used to create grouped
    displays. See iris example below}
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
  \item{\dots}{other arguments}
}
\details{
  This function is by far incomplete,  but is still included because of
  the structure it puts into place. Conceptually, \code{cloud} itself is
  almost complete (except for the \code{R.mat} argument), and once
  proper panel functions are  written, it and also \code{wireframe}
  should be complete. That, however, is probably not going to happen
  soon. 

  This and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage. 
}
\seealso{
  \code{\link{xyplot}},
  \code{\link{Lattice}}  
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,
      groups = Species, screen = list(z = 20, x = -70),
      subpanel = panel.superpose,
      key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(iris$Species))))
}
\keyword{hplot}
