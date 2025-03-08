

### Copyright 2001  Deepayan Sarkar <deepayan@stat.wisc.edu>
###
### This file is part of the lattice library for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA









prepanel.default.histogram <-
    function(x,
             breaks = NULL,
             equal.widths = TRUE,
             type = "density",
             ...)
{
    if (!is.numeric(x)) x <- as.numeric(x)

    if (length(x)<1)
        list(xlim = NA,
             ylim = NA,
             dx = NA,
             dy = NA)
    else
    {
        if (is.null(breaks)) {
            nint <- round(log2(length(x)) + 1)
            breaks <-
                if (equal.widths) do.breaks(range(x), nint)
                else quantile(x, 0:nint/nint)
        }
        h <- hist(x, breaks = breaks, plot = FALSE, ...)
        y <-
            if (type == "count") h$counts
            else if (type == "percent") 100 * h$counts/length(x)
            else h$intensities
        xlim <- range(x)
        ##lbreak <- max(xlim[1], breaks[breaks<=xlim[1]])
        ##ubreak <- min(xlim[2], breaks[breaks>=xlim[2]])
        ## why ?
        ##list(xlim = range(x, lbreak, ubreak),
        list(xlim = range(x, breaks),
             ylim = range(0,y),
             dx = 1,
             dy = 1)
    }
}









panel.histogram <- function(x,
                            breaks,
                            equal.widths = TRUE,
                            type = "density",
                            col = bar.fill$col,
                            ...)
{
    x <- as.numeric(x)

    grid.lines(x = c(0.05, 0.95),
               y = unit(c(0,0),"native"),
               default.units = "npc")
        
    if (length(x)>0) {
        bar.fill  <- trellis.par.get("bar.fill")

        if (is.null(breaks)) {

            nint <- round(log2(length(x)) + 1)
            breaks <-
                if (equal.widths) do.breaks(range(x), nint)
                else quantile(x, 0:nint/nint)

        }

        h <- hist(x, breaks = breaks, plot = FALSE, ...)
        y <-
            if (type == "count") h$counts
            else if (type == "percent") 100 * h$counts/length(x)
            else h$intensities

        nb <- length(breaks)
        if (nb != (length(y)+1)) warning("something is probably wrong")

        if (nb>1) {
            for(i in 1:(nb-1))
                if (y[i]>0) {
                    grid.rect(gp = gpar(fill = col),
                              x = breaks[i],
                              y = 0,
                              height = y[i],
                              width = breaks[i+1]-breaks[i],
                              just = c("left", "bottom"),
                              default.units = "native")
                }
        }
    }
}










histogram <-
    function(formula,
             data = parent.frame(),
             allow.multiple = FALSE,
             outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
             layout = NULL,
             panel = "panel.histogram",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             type = c("percent", "count", "density"),
             nint = if (is.factor(x)) length(levels(x))
             else round(log2(length(x)) + 1),
             endpoints = extend.limits(range(x[!is.na(x)]), prop = 0.04),
             breaks = if (is.factor(x)) seq(0.5, length = length(levels(x))+1)
             else do.breaks(endpoints, nint),
             equal.widths = TRUE,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    formname <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())

    if (!inherits(formula, "formula"))
        formula <- as.formula(paste("~", formname))
    
    form <-
        latticeParseFormula(formula, data, subset = subset,
                            groups = groups, multiple = allow.multiple,
                            outer = outer, subscripts = TRUE)

    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    cond <- form$condition
    number.of.cond <- length(cond)
    x <- form$right
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }






    
    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- TRUE

    ##if(!(is.numeric(x) || is.factor(x)))
    ##    warning("x should be numeric")
    ##x <- as.numeric(x)
    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab), dots))
                          

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()
    foo$fontsize.normal <- 10
    foo$fontsize.small <- 8

    ## This is for cases like xlab/ylab = list(cex=2)
    if (is.list(foo$xlab) && !is.characterOrExpression(foo$xlab$label))
        foo$xlab$label <- form$right.name

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character(scales)) scales <- list(relation = scales)
    foo <- c(foo,
             do.call("construct.scales", scales))


    ## Step 3: Decide if limits were specified in call:
    
    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit)) {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limit
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)
        
        x <- log(x, xbase)
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        warning("Can't have log Y-scale")
        have.ylog <- FALSE
        foo$y.scales$log <- FALSE
    }

    if ((have.xlog || is.null(breaks) ||
         length(unique(round(diff(breaks)))) != 1) &&
        missing(type))
        type <- "density"
    else type <- match.arg(type)

    if (is.logical(foo$ylab$label)) foo$ylab$label <- 
        if (type == "count") "Count"
        else if (type == "percent") "Percent of Total"
        else "Density"

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    ## equal.widths <- eval(equal.widths, data, parent.frame()) #keep this way ?
    foo$panel.args.common <- c(list(breaks = breaks,
                                    type = type,
                                    equal.widths = equal.widths), dots)
    if (subscripts) foo$panel.args.common$groups <- groups

    layout <- compute.layout(layout, cond.max.level, skip = foo$skip)
    plots.per.page <- max(layout[1] * layout[2], layout[2])
    number.of.pages <- layout[3]
    foo$skip <- rep(foo$skip, length = plots.per.page)
    foo$layout <- layout

    nplots <- plots.per.page * number.of.pages
    
    foo$panel.args <- as.list(1:nplots)
    cond.current.level <- rep(1,number.of.cond)
    panel.number <- 1 # this is a counter for panel number
    for (page.number in 1:number.of.pages)
        if (!any(cond.max.level-cond.current.level<0))
            for (plot in 1:plots.per.page) {

                if (foo$skip[plot]) foo$panel.args[[panel.number]] <- FALSE
                else if(!any(cond.max.level-cond.current.level<0)) {

                    id <- !id.na
                    for(i in 1:number.of.cond)
                    {
                        var <- cond[[i]]
                        id <- id &
                        if (is.shingle(var))
                            ((var >=
                              levels(var)[[cond.current.level[i]]][1])
                             & (var <=
                                levels(var)[[cond.current.level[i]]][2]))
                        else (as.numeric(var) == cond.current.level[i])
                    }

                    ##if (any(id)) {
                    foo$panel.args[[panel.number]] <-
                        list(x = x[id])
                    if (subscripts)
                        foo$panel.args[[panel.number]]$subscripts <-
                            subscr[id]
                    ##}
                    ##else
                    ##    foo$panel.args[[panel.number]] <-FALSE
                    
                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.histogram,
                               prepanel = prepanel, 
                               have.xlim = have.xlim, xlim = xlim, 
                               have.ylim = have.ylim, ylim = ylim, 
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = aspect,
                               nplots = nplots,
                               x.axs = foo$x.scales$axs,
                               y.axs = foo$y.scales$axs))


    if (is.null(foo$key) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
        foo$key <- do.call("simpleKey",
                           c(list(levels(as.factor(groups))),
                             if (is.list(auto.key)) auto.key else list()))

    class(foo) <- "trellis"
    foo
}
