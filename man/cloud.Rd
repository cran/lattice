\name{cloud}
\title{3d Scatter Plot}
\synopsis{
    cloud(formula,
             data = parent.frame(),
             aspect = c(1,1),
             layout = NULL,
             panel = "panel.cloud",
             subpanel = "panel.xyplot",
             prepanel = NULL,
             scales = NULL,
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = range(x),
             ylab,
             ylim = range(y),
             zlab,
             zlim = range(z),
             distance = .2,
             par.box,
             perspective = TRUE,
             R.mat = diag(4),
             screen = list(z = 40, x = -60),
             zoom = .9,
             at,
             pretty = FALSE,
             drape = FALSE,
             colorkey = any(drape),
             col.regions, cuts = 1,
             ...,
             subscripts = TRUE,
             subset = TRUE)
wireframe(formula,
             data = parent.frame(),
             panel = "panel.wireframe",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 70,
             pretty = FALSE,
             col.regions = trellis.par.get("regions")$col,
             drape = FALSE,
             colorkey = any(drape),
             ...,
             subset = TRUE)
}
\usage{
cloud(formula,
      data,
      subpanel = "panel.xyplot",
      aspect = c(1, 1),
      scales = list(cex = 0.5, lty = 1, lwd = 1,
                    col = "black", distance = rep(1, 3),
                    arrows = TRUE, draw = TRUE),
      zlab,
      zlim = range(z), 
      distance = 0.2,
      par.box,
      perspective = TRUE,
      R.mat = diag(4), 
      screen = list(z = 40, x = -60),
      zoom = .9,
      \dots)
wireframe(formula, data,
          at = pretty(z, cuts),
          col.regions,
          drape = FALSE,
          pretty = FALSE,
          colorkey = any(drape),
          cuts = 70,
          distance = 0.2,
          par.box,
          screen = list(z = 40, x = -60),
          zoom = .9,
          scales = list(cex = 0.5, distance = rep(1, 3), arrows = TRUE),
          \dots)
}
\description{
  Draws 3d scatter plots. The default panel and prepanel function makes
  the necessary 3d to 2d transformations and draws the bounding box and
  the scales. The transformed points are then passed to the subpanel
  function which actually plots them. See details below for
  shortcomings. 
}
\alias{cloud}
\alias{wireframe}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g1 * g2 *
      \dots}}
  \item{data}{data frame in which variables are evaluated}
  \item{subpanel}{the function used to plot the points in \code{cloud}
    once coordinates of their projections are computed.
    \code{subpanel="panel.superpose"} along with the \code{groups}
    argument can be used to create grouped displays. See iris example
    below}
  \item{aspect}{ vector of length 2, giving the relative aspects of the
    y-size/x-size and z-size/x-size of the enclosing rectangle.}
  \item{scales}{ describes scales. Can contain lists named x, y and
    z. Arrows are drawn if \code{arrows=TRUE}, otherwise tick marks with
    labels are drawn. Both can be suppressed by \code{draw=FALSE}.
    Several other components that work in the usual \code{scales}
    argument also work here.
  }
  \item{zlab}{z label}
  \item{zlim}{z limits}
  \item{distance}{ between 0 and 1, controls amount of perspective. No
    connection with the physical concept of distance in this
    implementation (not clear what S+ does). individual values don't give
    the same results as S-Plus, but all instances should be achievable.}
  \item{par.box}{graphical parameters for box, namely, col, lty and
    lwd. By default obtained from the parameter \code{box.3d}}
  \item{perspective}{logical, whether to plot a perspective view}
  \item{R.mat}{initial rotation matrix (homogeneous
    coordinates). Currently ignored.
  }
  \item{screen}{A list determining the rotations to applied to the data
    before being plotted. The initial position starts with the view
    somewhere in the positive z-axis, and the x and y axes in the usual
    position. Each component of the list should be named one of \code{x,
      y, z}, with their values indicating the amount of rotation about
    that axis in degrees.   }
  \item{zoom}{factor by which to scale the picture.}
  \item{drape}{ whether the wireframe is to be draped in color}
  \item{at}{ these arguments are analogous to those in
    \code{levelplot}. if \code{drape=TRUE}, \code{at} gives the vector
    of values where the colors change}
  \item{col.regions}{ the vector of colors to be used in that case}
  \item{cuts}{ the default number of cutpoints if \code{drape=TRUE}}
  \item{pretty}{ whether the cutpoints should be pretty}
  \item{colorkey}{ whether a color key should be drawn alongside. See
    \code{levelplot} for details}
  \item{\dots}{other arguments}
}
\details{
  cloud draws a 3d Scatter Plot, while wireframe draws a wireframe 3d
  surface. wireframe is very much in a proof of concept stage, and is
  extremely slow in rendering; this should improve sometime in the
  future. 

  The R.mat argument is currently ignored, and there are no plans to
  implement it in the near future.

  The algorithm for identifying which edges of the bounding box should
  be drawn before the points are plotted fails in some cases.
  
  Of the four possible edges that correspond to the x-axis, any one can
  be chosen as the one along which the arrows or tick marks are to be
  drawn. However, the default choice is not always a good choice, 
  and might need to be overridden. See help for \code{panel.cloud} for
  details. The same holds for y and z axes as well.

  This and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage. 
}
\seealso{
  \code{\link{xyplot}}, \code{\link{levelplot}},
  \code{\link{panel.cloud}},
  \code{\link{Lattice}}  
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
x <- seq(-pi, pi, len = 20)
y <- seq(-pi, pi, len = 20)
g <- expand.grid(x = x, y = y)
g$z <- sin(sqrt(g$x^2 + g$y^2))
wireframe(z ~ x * y, g, drape = TRUE, col.regions = "white",
          aspect = c(3,1), colorkey = FALSE)
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,
      groups = Species, screen = list(z = 20, x = -70),
      subpanel = panel.superpose,
      key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(iris$Species))))
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
            data = iris, cex = .8, perspective = FALSE,
            groups = Species, 
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 3)),
      split = c(1,1,2,1), more = TRUE)
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, cex = .8, perspective = FALSE,
            groups = Species,
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 0)),
      split = c(2,1,2,1))
}
\keyword{hplot}
