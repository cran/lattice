\name{xyplot}
\title{Common Bivariate Trellis Plots}
\synopsis{
xyplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.xyplot, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, ..., subscripts = !is.null(groups), 
    subset = TRUE)
barchart(formula, data = parent.frame(), panel = "panel.barchart", 
    prepanel = NULL, strip = TRUE, box.ratio = 2, groups = NULL, 
    ..., subset = TRUE)  
bwplot(formula, data = parent.frame(), aspect = "fill", layout = NULL, 
    panel = panel.bwplot, prepanel = NULL, scales = list(), strip = TRUE, 
    groups = NULL, xlab, xlim, ylab, ylim, box.ratio = 1, ..., 
    subscripts = !missing(groups), subset = TRUE)
dotplot(formula, data = parent.frame(), panel = "panel.dotplot", 
    prepanel = NULL, strip = TRUE, groups = NULL, ..., subset = TRUE)  
stripplot(formula, data = parent.frame(), panel = "panel.stripplot", 
    prepanel = NULL, strip = TRUE, jitter = FALSE, factor = 0.5, 
    box.ratio = if (jitter) 1 else 0, groups = NULL, ..., subset = TRUE)  
}
\usage{
xyplot(formula,
       data = parent.frame(),
       panel = "panel.xyplot",
       aspect = "fill",
       as.table = FALSE,
       between,
       groups,
       key,
       layout,
       main,
       page,
       par.strip.text,
       prepanel,
       scales,
       skip,
       strip = "strip.default",
       sub,
       xlab,
       xlim,
       ylab,
       ylim,
       \dots,
       subscripts,
       subset)
barchart(formula, data, panel = "panel.barchart", box.ratio = 2, \dots)
bwplot(formula, data, panel = "panel.bwplot", box.ratio = 1, \dots)
dotplot(formula, data, panel = "panel.dotplot", \dots)
stripplot(formula, data, panel = "panel.stripplot",
          jitter = FALSE, factor = .5, box.ratio, \dots)
}
\description{
  These are the most commonly used Trellis functions to look at pairs of
  variables. By far the most common is \code{xyplot}, designed mainly
  for two continuous variates, which produces Conditional
  Scatterplots. The others are useful when one of the variates is a
  factor. See details below.

  Most of the arguments documented here are also applicable for many of
  the other Trellis functions. These are not described in any detail
  elsewhere, and this should be considered the canonical documentation
  for such arguments.
}
\alias{barchart}
\alias{bwplot}
\alias{dotplot}
\alias{stripplot}
\alias{xyplot}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot. The
    formula is generally of the form \code{y ~ x | g1 * g2 * \dots},
    indicating that plots of \code{y} (on the y axis) versus \code{x}
    (on the x axis) should be produced conditional on the given
    variables \code{g1,g2,\dots}. However, the given variables
    \code{g1,g2,\dots} may be omitted. For Splus compatibility, the
    formula can also be written as \code{y ~ x | g1 + g2 + \dots}.

    The given variables \code{g1,g2,\dots} must be either factors or
    shingles (Shingles are a way of processing numeric variables for use
    in conditioning. See documentation of \code{shingle} for
    details. Like factors, they have a `level' attribute, which is used
    in producing the conditioning plots). For each unique combination of
    the levels of the conditioning variables \code{g1, g2, \dots}, a
    separate panel is produced using the points \code{(x,y)} for the
    subset of the data defined by that combination.

    Numeric conditioning variables are converted to shingles by the
    function \code{shingle} (however, using \code{equal.count} might be
    more appropriate in many cases), and character vectors are made into
    factors. The formula can involve expressions,
    e.g. \code{sqrt(),\log()}.

    All points with at least one of its values missing (NA) in any
    of the variates involved are omitted from the plot.
  }
  \item{data}{a data frame containing values for any variables in the
    formula. By default the environment where the function 
    was called from is used.
  }
  \item{box.ratio}{ gives the ratio of the width of the rectangles to the
    inter rectangle space.
  }
  \item{jitter}{ logical specifying whether the values should be jittered
    by adding a random noise in stripplot.}
  \item{factor}{ numeric controlling amount of jitter. Inverse effect
    compared to S ?
  }

  The following arguments are common to all the functions documented
  here, as well as most other high level Trellis functions. These are
  not documented elsewhere, except to override the usage given here.


  \item{panel}{ Once the subset of rows defined by each unique
    combination of the levels of the grouping variables are obtained
    (see above), the corresponding \code{x} and \code{y} variables (or
    some other variables, as appropriate, in the case of other
    functions) are passed on to be plotted in each panel. The actual
    plotting is done by the function specified by the \code{panel}
    argument. Each high level function has its own default panel
    function. 

    The panel function can be a function object or a character string
    giving the name of a predefined function. (The latter is preferred
    when possible, especially when the trellis object returned by the
    high level function is to be stored and plotted later.)

    Much of the power of Trellis Graphics comes from the ability to
    define customized panel functions. A panel function appropriate
    for the functions described here would usually expect arguments
    named \code{x} and \code{y}, which would be provided by the
    conditioning process. It can also have other arguments. (It might be
    useful to know in this context that all arguments passed to a high
    level Trellis function such as \code{xyplot} that are not recognized
    by it are passed through to the panel function. It is thus generally
    good practice when defining panel functions to allow a \code{\dots}
    argument.) Such extra arguments typically control graphical
    parameters, but other uses are also common. See documentation for
    individual panel functions for specifics.

    Technically speaking, panel functions must be written using Grid
    graphics functions. However, knowledge of Grid is usually not 
    necessary to construct new custom panel functions, there are several
    predefined panel functions which can help; for example,
    \code{panel.grid}, \code{panel.loess} etc. There are also some
    grid-compatible replacements of base R graphics functions useful for
    this purpose, such as \code{llines}. (Note that the corresponding
    base R graphics functions like \code{lines} would not work.) These
    are usually sufficient to convert existing custom panel functions
    written for S-Plus.
    
    One case where a bit more is required of the panel function is when
    the \code{groups} argument is not null. In that case, the panel
    function should also accept arguments named \code{groups} and
    \code{subscripts} (see below fr details). A very useful panel
    function predefined for use in such cases is
    \code{panel.superpose}. 

    Both \code{panel.superpose} and \code{panel.xyplot} have an argument
    called \code{type} which is often useful. See the separate
    documentation for these functions for details.
  }
  \item{aspect}{ controls physical aspect ratio of the panels (same for
    all the panels). It can be specified as a ratio (vertical
    size/horizontal size) or as a character string. Legitimate 
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
    other than \code{xyplot} (usually an aspect ratio of 1 results in
    such cases).

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
    \code{groups} along with \code{subscripts} is passed to the panel
    function, which is expected to handle these arguments. 
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
    between 0 and 1, giving coordinates with respect to the whole
    display area.

    The key essentially consists of a number of columns, possibly
    divided into blocks, each containing some rows. The contents of the
    key are determined by (possibly repeated) components named
    ``rectangles'', ``lines'', ``points'' or ``text''. Each of these
    must be lists with relevant graphical parameters (see later)
    controlling their appearance. The \code{key} list itself can contain
    graphical parameters, these would be used if relevant graphical
    components are omitted from the other components.

    The length (number of rows) of each such column is taken to be the
    largest of the lengths of the graphical components, including the
    ones specified outside. The ``text'' component has to have a
    character vector as its first component, and the length of this
    vector determines the number of rows.

    The graphical components that can be included in \code{key} (and
    also in the components named ``text'', ``lines'', ``points'' and
    ``rectangles'' when appropriate) are \code{cex=1, col="black",
      lty=1, lwd=1, font=1, pch=8, adj=0, type="l", size=5, angle=0,
      density=-1}. \code{adj, angle, density} are
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
  \item{layout}{ In general, a Trellis conditioning plot consists of
    several panels arranged in a rectangular array, possibly spanning
    multiple pages. \code{layout} determines this arrangement.

    \code{layout} is a numeric vector giving the number of columns, rows
    and pages in a multipanel display. By default, the number of columns
    is determined by the number of levels in the first given variable;
    the number of rows is the number of levels of the second given
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
    list with components \code{label, cex, col, font}. The \code{label}
    tag can be omitted if it is the first element of the list.
  }
  \item{page}{a function of one argument (page number) to be called
    after drawing each page. The function must be `grid-compliant', and
    is called with the whole display area as the default viewport.
  }
  \item{par.strip.text}{ list of graphical parameters to control the
    strip text, possible components are \code{col, cex, font, lines}.
    The first three control graphical parameters while the last is a
    means of altering the height of the strips. This can be useful, for
    example, if the strip labels (derived from factor levels, say) are
    double height (i.e., contains ``\\n''-s) or if the default height
    seems too small or too large.
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

    The return value of the prepanel function need not have all the
    components named above; in case some are missing, they are replaced
    by the usual componentwise defaults.
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

    tick.number: Suggested number of ticks.
    
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

    font: font face for axis labels (integer 1-5).
    
    tck: factor to control length of tick marks.

    col: color of ticks and labels.

    rot: Angle by which the axis labels are to be rotated.

    log: Use a log scale. Defaults to \code{FALSE}, other possible
    values are any number that works as a base for taking logarithm,
    \code{TRUE}, equivalent to 10, and \code{"e"} (for natural
    logarithm).

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
\seealso{
  \code{\link{shingle}},
  \code{\link{banking}},
  \code{\link{strip.default}},
  \code{\link{Lattice}} 
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
## Tonga Trench Earthquakes
data(quakes)
Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(long ~ lat | Depth, data = quakes)

## Examples with data from `Visualizing Data' (Cleveland)
## (obtained from Bill Cleveland's Homepage :
## http://cm.bell-labs.com/cm/ms/departments/sia/wsc/, also
## available at statlib)
data(ethanol)
EE <- equal.count(ethanol$E, number=9, overlap=1/4)
## Constructing panel functions on the fly; prepanel
xyplot(NOx ~ C | EE, data = ethanol,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Compression Ratio", ylab = "NOx (micrograms/J)",
       panel = function(x, y) {
           panel.grid(h=-1, v= 2)
           panel.xyplot(x, y)
           panel.loess(x,y, span=1)
       },
       aspect = "xy")
## banking
data(sunspot)
xyplot(sunspot ~ 1:37 ,type = "l", aspect="xy",
       scales = list(y = list(log = TRUE)),
       sub = "log scales")

data(state)
## user defined panel functions
states <- data.frame(state.x77,
                     state.name = dimnames(state.x77)[[1]], 
                     state.region = state.region) 
xyplot(Murder ~ Population | state.region, data = states, 
       groups = as.character(state.name), 
       panel = function(x, y, subscripts, groups)  
       ltext(x=x, y=y, label=groups[subscripts], cex=.7, font=3))
data(barley)
barchart(variety ~ yield | year * site, data = barley, aspect = 0.4,
        xlab = "Barley Yield (bushels/acre)")
data(singer)
bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
dotplot(variety~yield|year*site, data=barley)
dotplot(variety ~ yield | site, data = barley, groups = year,
        panel = function(x, y, subscripts, ...) {
            dot.line <- trellis.par.get("dot.line")
            panel.abline(h = y, col = dot.line$col,
                         lty = dot.line$lty)
            panel.superpose(x, y, subscripts, ...)
        },
        key = list(space="right", transparent = TRUE,
                   points=list(pch=trellis.par.get("superpose.symbol")$pch[1:2],
                               col=trellis.par.get("superpose.symbol")$col[1:2]),
                   text=list(c("1932", "1931"))),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter = T, xlab = "Height (inches)")
}
\keyword{dplot}









