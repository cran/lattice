\name{xyplot}
\title{Common Bivariate Trellis Plots}
\alias{barchart}
\alias{bwplot}
\alias{dotplot}
\alias{stripplot}
\alias{xyplot}
\synopsis{
xyplot(formula,
       data = parent.frame(),
       allow.multiple = FALSE,
       outer = FALSE,
       auto.key = FALSE,
       aspect = "fill",
       layout = NULL,
       panel = if (is.null(groups)) "panel.xyplot"
       else "panel.superpose",
       prepanel = NULL,
       scales = list(),
       strip = TRUE,
       groups = NULL,
       xlab,
       xlim,
       ylab,
       ylim,
       ...,
       subscripts = !is.null(groups),
       subset = TRUE)
bwplot(formula,
       data = parent.frame(),
       allow.multiple = FALSE,
       outer = FALSE,
       auto.key = FALSE,
       aspect = "fill",
       layout = NULL,
       panel = "panel.bwplot",
       prepanel = NULL,
       scales = list(),
       strip = TRUE,
       groups = NULL,
       xlab,
       xlim,
       ylab,
       ylim,
       box.ratio = 1,
       horizontal = NULL,
       ...,
       subscripts = !is.null(groups),
       subset = TRUE)
}
\usage{
xyplot(formula,
       data = parent.frame(),
       panel = if (is.null(groups)) "panel.xyplot"
               else "panel.superpose",
       allow.multiple = TRUE,
       outer,
       aspect = "fill",
       as.table = FALSE,
       between,
       groups,
       key,
       auto.key = FALSE,
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
dotplot(formula,
        data, 
        panel = "panel.dotplot",
        groups = NULL,
        ...,
        subset = TRUE)
barchart(formula,
         data,
         panel = "panel.barchart",
         box.ratio = 2,
         groups = NULL,
         ...,
         subset = TRUE)
stripplot(formula,
          data,
          panel = "panel.stripplot",
          jitter = FALSE,
          factor = .5,
          box.ratio = if (jitter) 1 else 0,
          groups = NULL,
          ...,
          subset = TRUE)
bwplot(formula,
       data,
       panel = "panel.bwplot",
       box.ratio = 1,
       ...,
       subset = TRUE)
}

\description{
  These are the most commonly used Trellis functions to look at pairs of
  variables. By far the most common is \code{xyplot}, designed mainly
  for two continuous variates (though factors can be supplied as well,
  in which case they will simply be coerced to numeric), which produces
  Conditional Scatterplots. The others are useful when one of the
  variates is a factor or a shingle. See details below.

  Most of the arguments documented here are also applicable for many of
  the other Trellis functions. These are not described in any detail
  elsewhere, and this should be considered the canonical documentation
  for such arguments.

  Note that any arguments passed to these functions and not recognized
  by them will be passed to the panel function. Most predefined panel
  functions have arguments that customize its output. These arguments
  are described only in the help pages for these panel functions, but
  can usually be supplied as arguments to the high level plot.  
}
\arguments{
  \item{formula}{a formula describing the form of conditioning plot. The
    formula is generally of the form \code{y ~ x | g1 * g2 * \dots},
    indicating that plots of \code{y} (on the y axis) versus \code{x}
    (on the x axis) should be produced conditional on the given
    variables \code{g1,g2,\dots}. However, the given variables
    \code{g1,g2,\dots} may be omitted. For S-PLUS compatibility, the
    formula can also be written as \code{y ~ x | g1 + g2 + \dots}.

    The given variables \code{g1, g2, \dots} must be either factors or
    shingles (Shingles are a way of processing numeric variables for use
    in conditioning. See documentation of \code{\link{shingle}} for
    details. Like factors, they have a `level' attribute, which is used
    in producing the conditioning plots). For each unique combination of
    the levels of the conditioning variables \code{g1, g2, \dots}, a
    separate panel is produced using the points \code{(x,y)} for the
    subset of the data (also called packet) defined by that combination.

    Numeric conditioning variables are converted to shingles by the
    function \code{shingle} (however, using \code{\link{equal.count}}
    might be more appropriate in many cases) and character vectors are
    coerced to factors.

    The formula can involve expressions, e.g. \code{sqrt(),log()}.

    A special case is when the left and/or right sides of the formula
    (before the conditioning variables) contain a `+' sign, e.g.,
    \code{y1+y2 ~ x | a*b}. Normally, this would result in the sum of
    the vectors \code{y1} and \code{y2} being plotted against
    \code{x}. However, if the \code{allow.multiple} flag is set to
    \code{TRUE}, this is treated differently. The formula given above,
    for instance, would be taken to mean that the user wants to plot
    both \code{y1~x | a*b} and \code{y2~x | a*b}, but with the
    \code{y1~x} and \code{y2~x} superposed in each panel (this is
    slightly more complicated in \code{barchart}). The two parts
    would be distinguished by different graphical parameters. This is
    essentially what the \code{groups} argument would produce, if
    \code{y1} and \code{y2} were concatenated to produce a longer
    vector, with the \code{groups} argument being an indicator of which
    rows come from which variable. In fact, this is exactly what is done
    internally using the \code{\link{reshape}} function. This feature
    cannot be used in conjunction with the \code{groups} argument.

    A variation on this feature is when the \code{outer} argument is set
    to \code{TRUE} as well as \code{allow.multiple}. In that case, the
    plots are not superposed in each panel, but instead separated into
    different panels (as if a new conditioning variable has been added).
    
    The \code{x} and \code{y} variables should both be numeric in
    \code{xyplot}, and an attempt is made to coerce them if
    not. However, if either is a factor, the levels of that factor are
    used as axis labels. In the other four functions documented here,
    exactly one of \code{x} and \code{y} should be numeric, and the
    other a factor or shingle. Which of these will happen is determined
    by the \code{horizontal} argument --- if \code{horizontal=TRUE},
    then \code{y} will be coerced to be a factor or shingle, otherwise
    \code{x}. The default value of \code{horizontal} is \code{FALSE} if
    \code{x} is a factor or shingle, \code{TRUE} otherwise. (The
    functionality provided by \code{horizontal=FALSE} is not
    S-compatible.)
  
    All points with at least one of its values missing (NA) in any
    of the variates involved are omitted from the plot.
  }
  \item{data}{a data frame containing values for any variables in the
    formula, as well as \code{groups} and \code{subset} if applicable. 
    By default the environment where the function was called from is
    used. 
  }
  \item{allow.multiple, outer}{
    logical flags to control what happens with formulas like \code{y1 +
      y2 ~ x}. See the entry for \code{formula} for details.
  }
  \item{box.ratio}{ applicable to \code{bwplot, bachart} and
    \code{strpplot}, specifies the ratio of the width of the rectangles
    to the inter rectangle space.
  }
  \item{horizontal}{ logical, applicable to \code{bwplot, dotplot,
      barchart} and \code{stripplot}. Determines which of \code{x} and
    \code{y} is to be a factor or shingle (\code{y} if TRUE, \code{x}
    otherwise). Defaults to \code{FALSE} if \code{x} is a factor or
    shingle, \code{TRUE} otherwise. This argument is used to process the  
    arguments to these high level functions, but more importantly, it is
    passed as an argument to the panel function, which is supposed to
    use it as approporiate.

    A potentially useful component of \code{scales} is this case might
    be \code{abbreviate=TRUE}, in which case long labels which would
    usually overlap will be abbreviated. \code{scales} could also
    contain a \code{minlength} argument in this case, which would be
    passed to the \code{abbreviate} function.
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
    function, which could depend on whether the \code{groups} argument
    was supplied.

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

    Note that unlike in S-PLUS, it is not guaranteed that panel
    functions will be supplied only numeric vectors for the \code{x} and
    \code{y} arguments; they can be factors as well (but not
    shingles). panel functions need to handle this case, which to get
    the old behaviour could simply coerce them to numeric.

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
    \code{subscripts} (see below for details). A very useful panel
    function predefined for use in such cases is \code{panel.superpose},
    which can be combined with different \code{panel.groups}
    functions. See the examples section for an interaction plot
    constructed this way. Several other panel functions can also handle
    the groups argument, including the default ones for \code{barchart,
      dotplot} and \code{stripplot}.

    \code{panel.xyplot} has an argument called \code{type} which is
    worth mentioning here because it is quite frequently used (and as
    mentioned above, can be passed to \code{xyplot} directly).  panel
    functions for \code{bwplot} and friends should have an argument
    called \code{horizontal} to account for the cases when \code{x} is
    the factor or shingle.
  }
  \item{panel.groups}{ relevant for \code{xyplot} and \code{densityplot}
    only, applies when \code{panel} is \code{panel.superpose} (which
    happens by default in these cases if \code{groups} is non-null)
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
    for all functions, its behaviour is not defined for any function
    other than \code{xyplot} (usually an aspect ratio of 1 results in
    such cases).

    The current implementation of banking is not very sophisticated, but
    is not totally vague either. See \code{\link{banking}} for details.
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

    It is very common to use a key (legend) when a grouping
    variable is specified. See entries for \code{key, auto.key} and
    \code{\link{simpleKey}} for how to draw a key.
  }
  \item{auto.key}{
    A logical (indicating whether a key is to be drawn automatically when
    a grouping variable is present in the plot), or a list of parameters
    to be passed to \code{\link{simpleKey}}. If \code{auto.key} is not
    \code{FALSE} and no \code{key} argument is specified in the call,
    \code{simpleKey} is called with \code{levels{groups}} as the first
    argument and the result is added as the \code{key} component of the
    final ``trellis'' object. (Note: this may not work in all high level
    functions.)

    \code{simpleKey} uses the trellis settings to determine the
    graphical parameters in the key, so this will be meaningful only if
    the settings are used in the plot as well.
  }
  \item{key}{
    A list of arguments that define a legend to be drawn on the plot
    (see also the less flexible but usually sufficient shortcut function
    \code{\link{simpleKey}} that can generate such a list).

    The position of the legend can be controlled in either of two
    possible  ways. If a component called \code{space} is present, the
    key is positioned outside the plot region, in one of the four sides, 
    determined by the value of \code{space}, which can be one of
    ``top'', ``bottom'', ``left'' and  ``right''. Alternatively, the key
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
    character or expression vector as its first component, and the
    length of this vector determines the number of rows.

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

    \code{title}: string or expression, title of the key,

    \code{cex.title}

    \code{background}: defaults to default background

    \code{border}: color of border, black if TRUE, defaults to FALSE (no
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
  \item{main}{character string or expression or list describing main
    title to be placed on top of each page. Defaults to \code{NULL}. Can
    be a character string or expression, or a list with components
    \code{label, cex, col, font}. The \code{label} tag can be omitted if
    it is the first element of the list. Expressions are treated as
    specification of LaTeX-like markup as in \code{\link{plotmath}}
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
    otherwise. See \code{xlim} to see what forms of the components
    \code{xlim, ylim} are allowed.

    The \code{dx} and \code{dy} components are used for banking
    computations in case \code{aspect} is specified as "xy". See
    documentation for the function \code{banking} for details regarding
    how this is done.

    The return value of the prepanel function need not have all the
    components named above; in case some are missing, they are replaced
    by the usual componentwise defaults.

    The prepanel function is responsible for providing a meaningful
    return value when the \code{x, y} (etc.) variables are zero-length
    vectors. When nothing is appropriate, values of NA should be
    returned for the \code{xlim} and \code{ylim} components.
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

    at: location of tick marks along the axis (in native coordinates),
    or a list as long as the number of panels describing tick locations
    for each panel.

    labels: Labels (strings or expressions) to go along with
    \code{at}. Can be a list like \code{at} as well.

    cex: factor to control character sizes for axis labels.

    font: font face for axis labels (integer 1-4).
    
    tck: factor to control length of tick marks.

    col: color of ticks and labels.

    rot: Angle by which the axis labels are to be rotated.

    abbreviate: logical, whether to abbreviate the labels using
    \code{abbreviate}. Can be useful for long labels (e.g., in factors),
    especially on the x-axis.

    minlength: argument to \code{abbreviate} is \code{abbreviate=TRUE}.
    
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
  \item{sub}{character string or expression for a subtitle to be placed
    at the bottom of each page. See entry for \code{main} for finer
    control options.
  }
  \item{subscripts}{ logical specifying whether or not a vector named
    subscripts should be passed to the panel function. Defaults to
    FALSE, unless \code{groups} is specified, or if the panel function
    accepts an argument named \code{subscripts}. (One should be careful
    when defining the panel function on-the-fly.)
  }
  \item{subset}{ logical vector (can be specified in terms of variables
    in \code{data}). Everything will be done on the data points for
    which \code{subset=TRUE}. In case \code{subscripts} is TRUE, the
    subscripts will correspond to the original observations.
  }
  \item{xlab}{character string or expression giving label for the
    x-axis. Defaults to the expression for \code{x} in
    \code{formula}. Specify as \code{NULL} to omit the label
    altogether. Fine control is possible, see entry for \code{sub}.
  }
  \item{xlim}{ numeric vector of length 2 (possibly a DateTime object)
    giving minimum and maximum for x-axis, or, a character vector,
    expected to denote the levels of \code{x}. The latter form is
    interpreted as a range containing c(1, length(xlim)), with the
    character vector determining labels at tick positions
    \code{1:length(xlim)}
  }
  \item{ylab}{ character string or expression giving label for the
    y-axis. Defaults to the expression for \code{y} in
    \code{formula}. Fine control possible, see entry for \code{xlab}.
  }
  \item{ylim}{ same as \code{xlim}, applied to the y-axis. 
  }
  \item{\dots}{ other arguments, passed to the panel function }
}
\value{
  An object of class ``trellis''. This does not actually plot anything
  until printed by \code{print.trellis} (which usually happens
  automatically when on the command line).
}

\details{
  These are for the most part decriptions generally applicable to all
  high level Lattice functions, with special emphasis on \code{xyplot,
    bwplot} etc. For other functions, their individual documentation
  should be studied in addition to this. 
}
\seealso{
  \code{\link{Lattice}}
  \code{\link{print.trellis}},
  \code{\link{shingle}},
  \code{\link{banking}},
  \code{\link{reshape}}
  \code{\link{panel.xyplot}},
  \code{\link{panel.bwplot}},
  \code{\link{panel.barchart}},
  \code{\link{panel.dotplot}},
  \code{\link{panel.stripplot}},
  \code{\link{panel.superpose}},
  \code{\link{panel.loess}},
  \code{\link{panel.linejoin}},
  \code{\link{strip.default}},
  \code{\link{simpleKey}}
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\examples{
## Tonga Trench Earthquakes
data(quakes)
Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(lat ~ long | Depth, data = quakes)

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

## banking to 45 degrees

data(sunspot)
xyplot(sunspot ~ 1:37 ,type = "l", aspect="xy",
       scales = list(y = list(log = TRUE)),
       sub = "log scales")

## Multiple variables in formula for grouped displays

data(iris)
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species, 
       data = iris, allow.multiple = TRUE, scales = "free",
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

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
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(abbreviate = TRUE,
                       minlength = 5)))
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE, 
         auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))

data(singer)
bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
dotplot(variety ~ yield | year * site, data=barley)

dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)

stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter = TRUE, xlab = "Height (inches)")
## Interaction Plot
data(OrchardSprays)
bwplot(decrease ~ treatment, OrchardSprays, groups = rowpos,
       panel = "panel.superpose",
       panel.groups = "panel.linejoin",
       xlab = "treatment",
       key = list(lines = Rows(trellis.par.get("superpose.line"),
                  c(1:7, 1)), 
                  text = list(lab = as.character(unique(OrchardSprays$rowpos))),
                  columns = 4, title = "Row position"))
}
\keyword{hplot}
