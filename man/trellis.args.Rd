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
           xlab,
           xlim,
           ylab,
           ylim,
           ...,
           subset)
}
\description{
  These arguments are common to (almost?) all trellis functions.
}
\alias{trellis.args}
\alias{trellis.function}
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
  \item{data}{a data frame containing values for any variables in the
    formula. By default the environment where the function 
    was called from is used.
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
  \item{groups}{ used typically with \code{panel=panel.superpose}
    to allow display controls (color, lty etc) to vary according
    to a grouping variable. Formally, if groups is specified, then
    \code{groups} along with \code{subscripts} is passed
    to the panel function, which is expected to handle these arguments.
  }
  \item{key}{
    A list of arguments that define a legend to be drawn on the plot.

    The position of the legend can be controlled in either of two
    possible  ways. If a component called \code{space} is present, the
    key is positioned outside the plot region, in one of the four sides, 
    determined by the value of \code{space}, which can be one of
    ``top'', ``bootom'', ``left'' and  ``right''. Alternately, the key
    can be positioned inside the plot region by specifying components
    \code{x,y} and \code{corner}. \code{x} and \code{y} determine the
    location of the corner of the key given by \code{corner}, which can
    be one of \code{c(0,0), c(1,0), c(1,1),c(0,1)}, which denote the
    corners of the unit square. \code{x} and \code{y} must be numbers
    between 0 and 1, giving coordinates with respect to the plotting
    area OR the whole display area, according as whether the plot is a
    single panel or multipanel plot.

    The key essentially consists of a number of columns, possibly
    divided into blocks, each containing some rows. The contents of the
    key are determined by (possibly repeated) components named
    ``rectangles'', ``lines'', ``points'' or ``text''. Each of these
    must be lists with relevant graphical parameters (see later)
    controlling their appearance. Components not given in the list take
    values defined outside in the list, default values of which are
    given below. The length (number of rows) of each such column is
    taken to be the largest of the lengths of the graphical components,
    including the ones specified outside. The ``text'' component has to
    have a character vector as its first component, and the length of
    this vector determines the number of rows.

    The graphical components that can be included in \code{key} (and
    also in the components named ``text'', ``lines'', ``points'' and
    ``rectangles'' when appropriate) are \code{cex=1, col="black",
      lty=1, lwd=1, font=1, pch=8, adj=0, type="l", size=5, angle=0,
      density=-1}. \code{adj, font, angle, density} are
    unimplemented. \code{size} determines the width of columns of
    rectangles and lines in character widths. \code{type} is relevant
    for lines; `"l"' denotes a line, `"p"' denotes a point, and `"b"'
    and `"o"' both denote both together.

    Other possible components of \code{key} are:

    \code{between}: numeric vector giving the amount of space (character
    widths) surrounding each column (split equally on both sides),

    \code{title}: character, title of the key,

    \code{cex.title}

    \code{background}: defaults to default background

    \code{border}:color of border, black if TRUE, defaluts to FALSE (no
    border drawn)

    \code{transparent=FALSE}: logical, whether key area should be cleared

    \code{columns}: the number of columns column-blocks the key is to be
    divided into, which are drawn side by side.

    \code{betwen.columns}: Space between column blocks, in addition to
    \code{between}.

    \code{divide} Number of point symbols to divide each line when
    \code{type} is `"b"' or `"o"' in \code{lines}.
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
    \code{panel.trellis.function}. See also \code{\ldots, subscripts}.
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

    log: Use a log scale. This is supported in most (but not all)
    Lattice functions, and should work perfectly.
    Defaults to \code{FALSE}, other possible values are any
    number that works as a base for taking logarithm, \code{TRUE},
    equivalent to 10, and \code{"e"} (for natural logarithm).

    Note: the "axs" component is ignored. Much of the function of
    \code{scales} is accomplished by \code{pscales} in \code{splom}.
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

  \item Fine tuning control of scales via \code{scales} is not
  reliable.

  \item \code{page} functions are technically implemented, but writing
  page functions would require knowledge of \code{Grid}. See
  \code{examples(splom)} for an example.
  
}
\seealso{
  \code{\link{shingle}},
  \code{\link{banking}},
  \code{\link{strip.default}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\keyword{dplot}









