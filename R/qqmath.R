



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




panel.qqmathline <-
    function(y, distribution, ...)
{
    y <- as.numeric(y)

    if (length(y) > 0) {
        yy <- quantile(y, c(0.25, 0.75))
        xx <- distribution(c(0.25, 0.75))
        r <- diff(yy)/diff(xx)
        panel.abline(c( yy[1]-xx[1]*r , r), ...)
    }
}


prepanel.qqmathline <-
    function(y, distribution, f.value = ppoints, ...)
{
    if (!is.numeric(y)) y <- as.numeric(y)

    yy <- quantile(y, c(0.25, 0.75))
    xx <- distribution(c(0.25, 0.75))
    n <- length(y)
    list(ylim = range(y), xlim = range(distribution(f.value(n))),
         dx = diff(xx), dy = diff(yy))
}


prepanel.default.qqmath <-
    function(...)
    prepanel.default.xyplot(...)


panel.qqmath <-
    function(...) panel.xyplot(...)


qqmath <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             layout = NULL,
             panel = "panel.qqmath",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             f.value = ppoints,
             distribution = qnorm,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    distribution.name <- deparse(substitute(distribution))
    distribution <-
        if (is.function(distribution)) distribution 
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <- latticeParseFormula(formula, data)
    cond <- form$condition
    number.of.cond <- length(cond)
    x <- form$right
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())
    if ("subscripts" %in% names(formals(eval(panel)))) subscripts <- TRUE
    if(subscripts) subscr <- seq(along=x)
    x <- x[subset, drop = TRUE]
    if (subscripts) subscr <- subscr[subset, drop = TRUE]

    if(missing(ylab)) ylab <- form$right.name
    if(missing(xlab)) xlab <- distribution.name
    if (is.shingle(x)) stop("x cannot be a shingle")
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
        foo$xlab$label <- deparse(substitute(distribution))
    if (is.list(foo$ylab) && !is.characterOrExpression(foo$ylab$label))
        foo$ylab$label <- form$right.name

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
        warning("cannot use log scale for y, use different distribution")
        foo$y.scales$log <- FALSE
    }
    
    ## Step 5: Process cond

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- dots
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
                    if (any(id)) {
                        foo$panel.args[[panel.number]] <-
                            list(x = distribution(f.value(length(x[id]))), 
                                 y = quantile(x[id], f.value(length(x[id]))))
                    }
                    else
                        foo$panel.args[[panel.number]] <-
                            list(x = numeric(0), y = numeric(0))

                    if (subscripts)
                        foo$panel.args[[panel.number]]$subscripts <-
                            subscr[id]

                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.qqmath,
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
                               y.axs = foo$y.scales$axs,
                               distribution = distribution))

    class(foo) <- "trellis"
    foo
}





