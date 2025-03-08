

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




prepanel.default.xyplot <-
    function(x, y, type, subscripts, groups, ...)
{

    ## Note: shingles satisfy is.numeric()
    if (any(!is.na(x)) && any(!is.na(y))) {

        if (!missing(groups))
        {
            vals <-
                if (is.factor(groups)) levels(groups)
                else sort(unique(groups))

            dx <- numeric(0)
            dy <- numeric(0)
            for (i in seq(along=vals))
            {
                id <- (groups[subscripts] == vals[i])
                ord <- sort.list(x)
                dx <- c(dx, as.numeric(diff(x[ord])))
                dy <- c(dy, as.numeric(diff(y[ord])))
            }
        }
        else
        {
            ord <- sort.list(x)
            dx = as.numeric(diff(x[ord]))
            dy = as.numeric(diff(y[ord]))            
        }
        list(xlim = if (is.numeric(x)) range(x[is.finite(x)]) else levels(x),
             ylim = if (is.numeric(y)) range(y[is.finite(y)]) else levels(y),
             dx = dx, dy = dy)

    }
    else list(xlim = c(NA, NA),
              ylim = c(NA, NA),
              dx = NA, dy = NA)
}




panel.xyplot <-
    function(x, y, type="p",
             pch = plot.symbol$pch,
             col,
             col.line = plot.line$col,
             col.symbol = plot.symbol$col,
             lty = plot.line$lty,
             cex = plot.symbol$cex,
             lwd = plot.line$lwd, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x)>0) {


        if (!missing(col)) {
            if (missing(col.line)) col.line <- col
            if (missing(col.symbol)) col.symbol <- col
        }

        plot.symbol <- trellis.par.get("plot.symbol")
        plot.line <- trellis.par.get("plot.line")

        if ("o" %in% type || "b" %in% type)
            type <- c(type, "p", "l")


        if ("p" %in% type)
            lpoints(x = x, y = y, cex = cex,
                    col = col.symbol, pch=pch)


        if ("l" %in% type)
            llines(x=x, y=y, lty=lty, col=col.line, lwd=lwd)


        if ("h" %in% type)
            llines(x=x, y=y, type = "h", lty=lty, col=col.line, lwd=lwd)


        if ("s" %in% type) {
            ord <- sort.list(x)
            n <- length(x)
            xx <- numeric(2*n-1)
            yy <- numeric(2*n-1)

            xx[2*1:n-1] <- x[ord]
            yy[2*1:n-1] <- y[ord]
            xx[2*1:(n-1)] <- x[ord][-1]
            yy[2*1:(n-1)] <- y[ord][-n]
            llines(x=xx, y=yy,
                   lty=lty, col=col.line, lwd=lwd)
        }
        if ("S" %in% type) {
            ord <- sort.list(x)
            n <- length(x)
            xx <- numeric(2*n-1)
            yy <- numeric(2*n-1)

            xx[2*1:n-1] <- x[ord]
            yy[2*1:n-1] <- y[ord]
            xx[2*1:(n-1)] <- x[ord][-n]
            yy[2*1:(n-1)] <- y[ord][-1]
            llines(x=xx, y=yy,
                   lty=lty, col=col.line, lwd=lwd)
        }
        if ("r" %in% type) {
            panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, ...)
        }
        if ("smooth" %in% type) {
            panel.loess(x, y, col = col.line, lty = lty, lwd = lwd, ...)
        }
    }
}





xyplot <-
    function(formula,
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
{

    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

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
    y <- form$left
    x <- form$right
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- form$left.name

    ## S-Plus requires both x and y to be numeric, but we
    ## don't. Question is, should we give a warning ?

    if (!(is.numeric(x) && is.numeric(y)))
        warning("x and y are not both numeric")


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
    if (is.list(foo$ylab) && !is.characterOrExpression(foo$ylab$label))
        foo$ylab$label <- form$left.name

    ## Step 2: Compute scales.common (leaving out limits for now)

    ##scales <- eval(substitute(scales), data, parent.frame())
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
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    id.na <- is.na(x)|is.na(y)
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
                            ((var >= levels(var)[[cond.current.level[i]]][1])
                             & (var <= levels(var)[[cond.current.level[i]]][2]))
                        else (as.numeric(var) == cond.current.level[i])
                    }
                    foo$panel.args[[panel.number]] <-
                        list(x = x[id], y = y[id])
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
             limits.and.aspect(prepanel.default.xyplot,
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








