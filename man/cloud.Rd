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
cloud(formula, data, subpanel = "panel.xyplot", ...)
}
\description{
  Draw 3d scatter plots
}
\alias{cloud}
\arguments{
  \item{formula}{a formula of the form \code{z ~ x * y | g_1 * \dots *
      g_n}} 
  \item{data}{data frame in which variables are evaluated}
  \item{subpanel}{the function used to plot the points once coordinates
    of their projections are computed. \code{subpanel="panel.superpose"}
    along with the \code{groups} argument can be used to create grouped
    displays. See iris example below}
  \item{\dots}{other arguments}
}
\details{
  see the documentation for \code{trellis.args} and
  \code{trellis.3d.args}. This function is by far incomplete,  but is
  still included because of the structure it puts into
  place. Conceptually, \code{cloud} itself is almost complete (except
  for the \code{R.mat} argument), and once proper panel functions are
  written, it and also \code{wireframe} should be complete. That,
  however, is probably not going to happen soon.
}
\seealso{
  \code{\link{trellis.args}}, \code{\link{trellis.3d.args}},
  \code{\link{Lattice}}  
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,
      groups = Species, screen = list(z = 20, x = -70),
      subpanel = panel.superpose,
      key = list(title = "Iris Data", x = .25, y=.75, corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(iris$Species))))
}
\keyword{hplot}
