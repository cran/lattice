\name{trellis.args}
\title{Trellis Arguments}
\usage{
trellis.function(
           formula,
           aspect = "fill",
           as.table = FALSE,
           between,
           data,
           groups, 
           key,
           layout,
           main,
           page,
           panel,
           par.strip.text,
           prepanel,
           scales,
           skip,
           strip = strip.default,
           sub,
           subscripts,
           subset,
           xlab,
           xlim,
           ylab,
           ylim,
           ...)
}
\description{
  These arguments are common to (almost?) all trellis functions.
}
\alias{trellis.args}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot.  A
    formula is generally of the form \code{y ~ x | g1 * g2 * \ldots},
    indicating that plots of \code{y} versus \code{x} should be produced
    conditional on the given variables \code{g1,g2,\ldots}. However, the
    given variables \code{g1,g2,\ldots} may be omitted. (In certain
    cases, even \code{y} may be omitted). The given variables
    \code{g1,g2,\ldots} must be  either factors or shingles (Shingles are a
    way of processing numeric variables for use in conditioning. See
    documentation of \code{shingle} for details.) Numeric conditioning 
    variables are converted to shingles by the function \code{shingle}
    (however, using \code{equal.count} might be more appropriate in many
    cases), and character vectors are made into factors. The
    formula can involve expressions, e.g. \code{sqrt(),\log()}. 
    All points with at least one of its coordinates missing (NA) are
    omitted from the plot.
  }
  \item{aspect}{ controls physical aspect ratio of the panels (same for
    all the planets). It can be specified as a number
    (verical/horizontal size) or as a character string. Legitimate
    values are "fill" (the default) which tries to make the panels as
    big as possible to fill the available space, and "xy", which
    \bold{tries} to compute the aspect based on the 45 degree banking
    rule (see \emph{Visualizing Data} by William S. Cleveland for
    details). 

    Of the available functions, banking is sensible only for
    \code{xyplot}. If a \code{prepanel} function is specified, the
    results are used to compute the aspect, otherwise some internal
    computations are done inside each function. While this is allowed
    for all functions, it's behaviour is not defined for any function
    other than \code{xyplot}.

    The current implementation of banking is not very sophisticated, but
    is not totally vague either. See \code{banking} for details.
  }
  \item{as.table}{ logical that controls the order in which panels
    should be plotted: if FALSE, panels are drawn left to right,
    bottom to top (graph), if TRUE, left to right, top to bottom
    (matrix).
  }
  \item{between}{a list with components \code{x} and \code{y} (both
    usually 0 by default), numeric vectors specifying the space between
    the panels (units are character heights). \code{x} and \code{y} are
    repeated to account for all panels in a page and any extra
    components are ignored. The result is used for all pages in a
    multipage display. (In other words, it is not possible to use
    different \code{between} values for different pages).
  }
  \item{data}{a data frame containing values for any variables in the
    formula. By default the environment where the function 
    was called from is used.
  }
  \item{groups}{ used typically with \code{panel=panel.superpose}
    to allow display controls (color, lty etc) to vary according
    to a grouping variable. Formally, if groups is specified, then
    \code{groups} along with \code{subscripts} is passed
    to the panel function, which is expected to handle these arguments.
  }
  \item{key}{ supposedly a list of arguments to be passed to the key
    function for drawing legends. Currently ignored.
  }
  \item{layout}{ numeric vector giving the number of columns, rows and
    pages in a multipanel display. By default, the number of columns is
    determined by the number of levels in the first given variable; the
    number of rows is the number of levels of the second given 
    variable. If there is one given variable, the default layout vector
    is c(0,n) , where n is the number of levels of the given vector. Any
    time the first value in the layout vector is 0 , the second value is
    used as the desired number of panels per page and the actual layout
    is computed from this, taking into account the aspect ratio of the
    panels and the device dimensions (via \code{par("din")}). The number
    of pages is by default set to as many as is required to plot all the
    panels. In general, giving a high value of \code{layout[3]} is not
    wasteful because blank pages are never created.
  }
  \item{main}{character string for main title to be placed on top of
    each page. Defaults to \code{NULL}. Can be a character string, or a
    list with components \code{label, cex, col}. The \code{label} tag
    can be omitted if it is the first element of the list.
  }
  \item{page}{a function of one argument (page number) to be called
    after drawing each page.
  }
  \item{panel}{The function called for each panel. Should default to
    \code{panel.trellis.function}. See also
    \code{\ldots, subscripts}. Note that unlike S, the name of the
    function cannot be passed as a character string. (However, this in
    not difficult to implement.)
  }
  \item{par.strip.text}{ list of graphical parameters to control the
    strip text, possible components are \code{col, cex}.
  }
  \item{prepanel}{ function that takes arguments \code{x,y} (usually)
    and returns a list containing four components \code{xlim, ylim, dx,
      dy}. If \code{xlim} and \code{ylim} are not explicitly specified
    (possibly as components in \code{scales}), then the actual limits of
    the panels are guaranteed to include the limits returned by the
    prepanel function. This happens globally if the \code{relation}
    component of \code{scales} is "same", and on a panel by panel basis
    otherwise. 

    The \code{dx} and \code{dy} components are used for banking
    computations in case \code{aspect} is specified as "xy". See
    documentation for the function \code{banking} for details regarding
    how this is done.
  }
  \item{scales}{ list determining how the x- and y-axes (tick marks and
    labels) are drawn. The list contains parameters in name=value form,
    and may also contain two other lists called \code{x} and \code{y} of
    the same form (described below). Components of \code{x} and \code{y}
    affect the respective axes only, while those in \code{scales} affect
    both. (When parameters are specified in both lists, the values in
    \code{x} or \code{y} are used.) The components are :

    relation : determines limits of the axis. Possible values are "same"
    (default), "free" and "sliced". For relation="same", the same limits
    (determined by \code{xlim, ylim, scales$limits} etc) are used for
    all the panels. For relation="free", limits for each panel is
    determined by the points in that panel. Behaviour for relation =
    "sliced" is similar, except for that the length (max - min) of the
    scales are constrained to remain the same across panels. The values
    of \code{xlim} etc, even if specified explicitly, are ignored if
    relation is different from "same".

    (Note that unlike S, there is no other way to specify the
    \code{relation} component.)  

    tick.number: Suggested number of ticks. This is currently ignored
    (because of an unrelated  bug in \code{pretty}), but would probably
    be fixed in future.
    
    draw = TRUE: logical, whether to draw the axis at all.

    alternating = TRUE/c(1,2): logical specifying whether axes alternate
    from one side of the group of panels to the other. For more accurate
    control, alternating can be a vector (replicated to be as long as
    the number of rows or columns per page) consisting of the possible
    numbers 0=do not draw, 1=bottom/left and 2=top/right. alternating
    applies only when relation="same".

    limits: same as xlim and ylim.

    at: location of tick marks along the axis (in native coordinates).

    labels: Labels to go along with \code{at}

    cex: factor to control character sizes for axis labels.

    tck: factor to control length of tick marks (ignored now).

    col: color of ticks and labels.

    rot: Angle by which the axis labels are to be rotated.

    log: Use a log scale. This is \bold{not} supported, and there isn't
    much hope of the situation changing in the near futere.

    Note: the "axs" component is ignored. The value of scales is treated
    differently from S in \code{splom}.
  }
  \item{skip}{logical vector (default \code{FALSE}), replicated to be as
    long as the number of panels in each page. If \code{TRUE}, nothing
    is plotted in the corresponding panel. Useful for arranging plots in
    an informative manner.
  }
  \item{strip}{logical flag or function. If \code{FALSE}, strips are
    not drawn. Otherwise, strips are drawn using the \code{strip}
    function, which defaults to \code{strip.default}. See documentation
    of \code{strip.default} to see the form of a strip function.
  }
  \item{sub}{character string for a subtitle to be placed at the bottom
    of each page. See entry for \code{main} for finer control options.
  }
  \item{subscripts}{ logical specifying whether or not a vector named
    subscripts should be passed to the panel function. Defaults to
    FALSE, unless \code{groups} is specified, or if the panel function
    accepts an argument named \code{subscripts}. (One should be careful
    when defining the panel function on-the-fly.)
  }
  \item{subset}{ logical vector (can be specified in terms of variables
    in \code{data}). Everything will be done on the data points for
    which \code{subset=T}. In case \code{subscripts} is TRUE, the
    subscripts will correspond to the original observations.
  }
  \item{xlab}{character string giving label for the x-axis. Defaults to
    the expression for \code{x} in \code{formula}. Specify as
    \code{NULL} to omit the label altogether. Fine control is possible,
    see entry for \code{sub}. 
  }
  \item{xlim}{ numeric vector of length 2 giving minimum and maximum for
    x-axis. 
  }
  \item{ylab}{ character string giving label for the y-axis. Defaults to
    the expression for \code{y} in \code{formula}. Fine control
    possible, see entry for \code{xlab}.
  }
  \item{ylim}{numeric vector of length 2 giving minimum and maximum
    for y-axis. 
  }
  \item{\dots}{ other arguments, passed to the panel function }
}
\details{
  These are general arguments to trellis functions.
}
\note{
  \item The following is a brief description of the functionality of
  S+ trellis graphics that has \bold{not} been implemented yet: 

  \item The \code{key} argument is totally unimplemented.
  \item Most fine tuning control of scales via \code{scales} is
  unimplemeted, most notably the `log' scales. The `relation' tag cannot
  be omitted in \code{scales}.
  \item \code{page} functions are technically implemented, but writing
  page functions would require knowledge of \code{lattice}.
  
}
\seealso{
  \code{\link{shingle}},
  \code{\link{banking}},
  \code{\link{strip.default}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}









