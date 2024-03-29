\name{B_04_qqmath}
\alias{qqmath}
\alias{qqmath.formula}
\alias{qqmath.data.frame}
\alias{qqmath.numeric}
\title{Q-Q Plot with Theoretical Distribution}
\description{
  Draw quantile-Quantile plots of a sample against a theoretical
  distribution, possibly conditioned on other variables.
}
\usage{
qqmath(x, data, ...)

\method{qqmath}{formula}(x,
       data,
       allow.multiple = is.null(groups) || outer,
       outer = !is.null(groups),
       distribution = qnorm,
       f.value = NULL,
       auto.key = lattice.getOption("default.args")$auto.key,
       aspect = "fill",
       panel = lattice.getOption("panel.qqmath"),
       prepanel = NULL,
       scales, strip, groups,
       xlab, xlim, ylab, ylim,
       drop.unused.levels = lattice.getOption("drop.unused.levels"),
       ...,
       lattice.options = NULL,
       default.scales = list(),
       default.prepanel = lattice.getOption("prepanel.default.qqmath"),
       subscripts,
       subset)

\method{qqmath}{data.frame}(x, data = NULL, formula = data, ...)

\method{qqmath}{numeric}(x, data = NULL, ylab, ...)
}
\arguments{
  \item{x}{
    The object on which method dispatch is carried out.

    For the \code{"formula"} method, \code{x} should be a formula of the
    form \code{~ x | g1 * g2 * ...}, where \code{x} should be a
    numeric variable.  For the \code{"numeric"} method, \code{x} should
    be a numeric vector.

  }
  \item{data}{

    For the \code{formula} method, an optional data source (usually a
    data frame) in which variables are to be evaluated (see
    \code{\link{xyplot}} for details).  \code{data} should not be
    specified for the other methods, and is ignored with a warning if it
    is.

  }
  \item{formula}{
    The formula to be used for the \code{"data.frame"} methods. See
    documentation for argument \code{x} for details.
  }
  \item{distribution}{

    A quantile function that takes a vector of probabilities as argument
    and produces the corresponding quantiles from a theoretical
    distribution.  Possible values are \code{\link{qnorm}},
    \code{\link{qunif}}, etc.  Distributions with other required
    arguments need to be provided as user-defined functions (see example
    with \code{\link{qt}}).

  }
  \item{f.value}{

    An optional numeric vector of probabilities, quantiles corresponding
    to which should be plotted.  This can also be a function of a single
    integer (representing sample size) that returns such a numeric
    vector.  A typical value for this argument is the function
    \code{ppoints}, which is also the S-PLUS default.  If specified, the
    probabilities generated by this function is used for the plotted
    quantiles, through the \code{\link{quantile}} function for the
    sample, and the function specified as the \code{distribution}
    argument for the theoretical distribution.

    \code{f.value} defaults to \code{NULL}, which has the effect of
    using \code{ppoints} for the quantiles of the theoretical
    distribution, but the exact data values for the sample.  This is
    similar to what happens for \code{qqnorm}, but different from the
    S-PLUS default of \code{f.value=ppoints}.

    For large \code{x}, this argument can be used to restrict the number
    of points plotted.  See also the \code{tails.n} argument in
    \code{\link{panel.qqmath}}.

  }
  \item{panel}{

    A function, called once for each panel, that uses the packet (subset
    of panel variables) corresponding to the panel to create a display.
    The default panel function \code{\link{panel.qqmath}} is documented
    separately, and has arguments that can be used to customize its
    output in various ways.  Such arguments can usually be directly
    supplied to the high-level function.

  }
  \item{allow.multiple, outer}{ See \code{\link{xyplot}}. }
  \item{auto.key}{ See \code{\link{xyplot}}. }
  \item{aspect}{ See \code{\link{xyplot}}. }
  \item{prepanel}{ See \code{\link{xyplot}}. }
  \item{scales}{ See \code{\link{xyplot}}. }
  \item{strip}{ See \code{\link{xyplot}}. }
  \item{groups}{ See \code{\link{xyplot}}.  }
  \item{xlab, ylab}{ See \code{\link{xyplot}}. }
  \item{xlim, ylim}{ See \code{\link{xyplot}}. }
  \item{drop.unused.levels}{ See \code{\link{xyplot}}. }
  \item{lattice.options}{ See \code{\link{xyplot}}. }
  \item{default.scales}{ See \code{\link{xyplot}}. }
  \item{subscripts}{ See \code{\link{xyplot}}. }
  \item{subset}{ See \code{\link{xyplot}}. }

  \item{default.prepanel}{
    Fallback prepanel function.  See \code{\link{xyplot}}.
  }
  \item{...}{
    Further arguments.  See corresponding entry in \code{\link{xyplot}}
    for non-trivial details.
  }
}
\value{

  An object of class \code{"trellis"}. The
  \code{\link[lattice:update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[lattice:print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.

}
\details{

  \code{qqmath} produces Q-Q plots of the given sample against a
  theoretical distribution.  The default behaviour of \code{qqmath} is
  different from the corresponding S-PLUS function, but is similar to
  \code{qqnorm}.  See the entry for \code{f.value} for specifics.

  The implementation details are also different from S-PLUS.  In
  particular, all the important calculations are done by the panel (and
  prepanel function) and not \code{qqmath} itself.  In fact, both the
  arguments \code{distribution} and \code{f.value} are passed unchanged
  to the panel and prepanel function.  This allows, among other things,
  display of grouped Q-Q plots, which are often useful.  See the help
  page for \code{\link{panel.qqmath}} for further details.

  This and all other high level Trellis functions have several arguments
  in common. These are extensively documented only in the help page for
  \code{\link{xyplot}}, which should be consulted to learn more detailed
  usage.

}
\author{Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{xyplot}}, \code{\link{panel.qqmath}},
  \code{\link{panel.qqmathline}}, \code{\link{prepanel.qqmathline}},
  \code{\link{Lattice}}, \code{\link{quantile}}
}
\examples{
qqmath(~ rnorm(100), distribution = function(p) qt(p, df = 10))
qqmath(~ height | voice.part, aspect = "xy", data = singer,
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
          panel.qqmathline(x, ...)
          panel.qqmath(x, ...)
       })
vp.comb <-
    factor(sapply(strsplit(as.character(singer$voice.part), split = " "),
                  "[", 1),
           levels = c("Bass", "Tenor", "Alto", "Soprano"))
vp.group <-
    factor(sapply(strsplit(as.character(singer$voice.part), split = " "),
                  "[", 2))
qqmath(~ height | vp.comb, data = singer,
       groups = vp.group, auto.key = list(space = "right"),
       aspect = "xy",
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
          panel.qqmathline(x, ...)
          panel.qqmath(x, ...)
       })
}
\keyword{dplot}

