\name{splom}
\title{Scatter Plot Matrices}
\synopsis{
splom(formula,
             data = parent.frame(),
             aspect = 1,
             between = list(x = 0.5, y = 0.5),
             layout = NULL,
             panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = "Scatter Plot Matrix",
             xlim,
             ylab = NULL,
             ylim,
             superpanel = "panel.pairs",
             pscales = 5,
             varnames,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
parallel(formula, data = parent.frame(), aspect = "fill", between = list(x = 0.5, 
    y = 0.5), layout = NULL, panel = panel.parallel, prepanel = NULL, 
    scales = list(), strip = TRUE, groups = NULL, xlab = NULL, 
    xlim, ylab = NULL, ylim, varnames, ..., subscripts = !is.null(groups), 
    subset = TRUE)
}
\usage{
splom(formula,
      data,
      aspect = 1,
      between = list(x = 0.5, y = 0.5),
      panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
      superpanel = "panel.pairs",
      pscales = 5,
      varnames, \dots)
parallel(formula,
         data = list(x = 0.5, y = 0.5),
         between,
         panel = "panel.parallel",
         varnames,
         \dots)
}
\description{
  Draw Conditional Scatter Plot Matrices and Parallel Coordinate Plots
}
\alias{splom}
\alias{parallel}
\arguments{
  \item{formula}{ a formula describing the structure of the plot, which
    should be of the form \code{~ x | g1 * g2 * \dots}, where \code{x}
    is a data frame or a matrix. Each of \code{g1,g2,\dots} must be
    either factors or shingles. The conditioning variables
    \code{g1,g2,\dots} may be omitted, in which case the leading
    \code{~} may also be omitted.
  }
  \item{data}{a data frame containing values for any variables in the
    formula. By default the environment where the function 
    was called from is used.
  }
  \item{aspect}{ 
    aspect ratio of each panel (and subpanel), square by default for
    \code{splom}.
  }
  \item{between}{ 
    to avoid confusion between panels and subpanels, the default is to
    show the panels of a splom plot with space between them.
  }
  \item{panel}{
    function that is used to plot the data on each subpanel of a splom
    display. Usual interpretation for \code{densityplot}.
  }
  \item{superpanel}{ 
    function that sets up the splom display, by default as a scatterplot
    matrix.
  }
  \item{pscales}{ 
    a numeric value or a list. If pscales is a single number, it tells
    the approximate number of equally-spaced ticks that should appear on 
    each axis. If pscales is a list, it should have p components,
    each of which is itself a list with two components: a numeric
    vector at and a character vector labels. These two vectors tell
    where labelled tick marks are placed on the axes, which are
    drawn inside the diagonal cells. Factor variables are labelled
    with the factor names. Use pscales=0 to supress the axes
    entirely.  
  }
  \item{varnames}{ 
    character vector giving the names of the p variables in x. By
    default, the column names of x.
  }
  \item{\dots}{ other arguments}
}
\details{
  \code{splom} produces Scatter Plot Matrices. The role usually played by
  \code{panel} is taken over by \code{superpanel}, which determines how
  the columns of \code{x} are to be arranged for pairwise plots. The
  only available option currently is \code{panel.pairs}. (Writing new
  superpanel functions would need knowledge of grid.) 

  The \code{scales} argument does not have its usual interpretation in
  \code{splom}. Its function is partly served by \code{pscales}. However,
  components of scale which are sensible would be supported (in future
  if not already supported). The \code{rot} component should be
  specified as a vector of length 2 for the labels of the horizontal and 
  vertical labels respectively.

  \code{parallel} draws Parallel Coordinate Plots. (Difficult to
  describe, see example.)
  
  These and all other high level Trellis functions have several
  arguments in common. These are extensively documented only in the
  help page for \code{xyplot}, which should be consulted to learn more
  detailed usage.
}
\value{
  An object of class ``trellis'', plotted by default by
  \code{print.trellis}.
}
\seealso{
  \code{\link{xyplot}}, \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
data(iris)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                 col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~iris[1:3]|Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
          grid.text(x = seq(.6, .8, len = 4), 
                    y = seq(.9, .6, len = 4), 
                    label = c("Three", "Varieties", "of", "Iris"),
                    gp = gpar(fontsize=20))
      })
}
\keyword{hplot}






