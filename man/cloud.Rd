\name{cloud}
\title{3d Scatter Plot}
\synopsis{
cloud(formula,
             data = parent.frame(),
             allow.multiple = FALSE,
             outer = FALSE,
             auto.key = FALSE,
             aspect = c(1,1),
             layout = NULL,
             panel = "panel.cloud",
             prepanel = NULL,
             scales = NULL,
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = if (is.factor(x)) levels(x) else range(x),
             ylab,
             ylim = if (is.factor(y)) levels(y) else range(y),
             zlab,
             zlim = if (is.factor(z)) levels(z) else range(z),
             distance = .2,
             perspective = TRUE,
             R.mat = diag(4),
             screen = list(z = 40, x = -60),
             zoom = .8,
             at,
             pretty = FALSE,
             drape = FALSE,
             ...,
             colorkey = any(drape),
             col.regions, cuts = 1,
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
             drape = FALSE,
             ...,
             col.regions = trellis.par.get("regions")$col,
             colorkey = any(drape),
             subset = TRUE)
}
\usage{
cloud(formula,
      data,
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
  Draws 3d scatter plots and surfaces.
}
\alias{cloud}
\alias{wireframe}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g1 * g2 * ...},
    where \code{z} is a numeric response, and \code{x, y} are numeric
    values.
    
    \code{g1,g2,...}, if present, must be either factors or shingles.
    
    In the case of \code{wireframe}, calculations are based on the
    assumption the \code{x} and \code{y} values are evaluated on a grid
    (defined by \code{(unique(x))} and \code{(unique(y))}. The function
    will not return an error if this is not true, but the display might
    be nonsense.  However, the \code{x} and \code{y} values need not be
    equally spaced.

    As an extension to partially support the form used in
    \code{filled.contour} and \code{image}, \code{formula} can be a
    matrix.
  }
  \item{data}{data frame in which variables are evaluated}
  \item{aspect}{ vector of length 2, giving the relative aspects of the
    y-size/x-size and z-size/x-size of the enclosing rectangle.}
  \item{scales}{ describes scales. Can contain lists named x, y and
    z. Arrows are drawn if \code{arrows=TRUE}, otherwise tick marks with
    labels are drawn. Both can be suppressed by \code{draw=FALSE}.
    Several other components that work in the usual \code{scales}
    argument also work here (see \code{\link{xyplot}}).
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
  \item{R.mat}{initial rotation matrix in homogeneous
    coordinates. Implemented but untested.
  }
  \item{screen}{A list determining the rotations to applied to the data
    before being plotted. The initial position starts with the viewing
    point somewhere in the positive z-axis, and the x and y axes in the
    usual position. Each component of the list should be named one of
    \code{x, y, z} (repititions allowed), with their values indicating
    the amount of rotation about that axis in degrees.  }
  \item{zoom}{factor by which to scale the picture. Useful to get the
    variable names into the plot}
  \item{drape}{ whether the wireframe is to be draped in color}
  \item{at}{ these arguments are analogous to those in
    \code{\link{levelplot}}. if \code{drape=TRUE}, \code{at} gives the
    vector of values where the colors change}
  \item{col.regions}{ the vector of colors to be used in that case}
  \item{cuts}{ the default number of cutpoints if \code{drape=TRUE}}
  \item{pretty}{ whether the cutpoints should be pretty}
  \item{colorkey}{ whether a color key should be drawn alongside. See
    \code{\link{levelplot}} for details}
  \item{\dots}{other arguments, passed to the panel function}
}
\details{
  \code{cloud} draws a 3d Scatter Plot, while \code{wireframe} draws a
  wireframe 3d surface evaluated on a grid. Multiple surfaces can be
  drawn by \code{wireframe} using the \code{groups} argument. Specifying
  \code{groups} with \code{cloud} results in a
  \code{panel.superpose}-like effect (via \code{\link{panel.3dscatter}}).

  Wireframe can optionally render the surface as being illuminated by a
  light source (no shadows though). Details can be found in the help
  page for \code{\link{panel.cloud}}. Note that although arguments
  controlling these are actually arguments for the panel function, they
  can be supplied to \code{cloud} and \code{wireframe} directly.

  \code{wireframe} can be extremely slow in rendering; this should
  improve sometime in the future. (For some reason, postscript output is
  much faster than most other devices.)

  The algorithm for identifying which edges of the bounding box should
  be drawn before the points are plotted fails in some cases.

  This and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage. 
}
\value{
  An object of class ``trellis''. The `update' method can be used to
  update components of the object and the `print' method (usually called
  by default) will plot it on an appropriate plotting device.
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
wireframe(z ~ x * y, g, drape = TRUE,
          perspective = FALSE,
          aspect = c(3,1), colorkey = FALSE)
g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$g + g$y^2) * g$gr)
wireframe(z ~ x * y, data = g, groups = gr,
          scales = list(arrows = FALSE),
          shade = TRUE,
          shade.colors = function(cosangle, height)
          palette.shade(cosangle, height = .15, saturation = .05),
          light.source = c(0, 0, 1),
          screen = list(z = 30, x = -60))
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,
      groups = Species, screen = list(x = -90, y = 70),
      aspect = c(1, 1), distance = .4, zoom = .6,
      key = list(title = "Iris Data", x = .1, y=.9,
                 corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(iris$Species))))

ocol <- trellis.par.get("axis.line")$col
oclip <- trellis.par.get("clip")$panel
lset(list(axis.line = list(col = "transparent"), clip = list(panel = FALSE)))
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
            data = iris, cex = .8, 
            groups = Species, 
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 3)),
      split = c(1,1,2,1), more = TRUE)
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, cex = .8, 
            groups = Species,
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 0)),
      split = c(2,1,2,1))
lset(list(axis.line = list(col = ocol), clip = list(panel = oclip)))
}
\keyword{hplot}
