

### Copyright 2001-2003 Deepayan Sarkar <deepayan@stat.wisc.edu>
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






prepanel.default.bwplot <-
    function(x, y, box.ratio,
             horizontal = TRUE,
             origin = NULL, stack = FALSE,
             levels.fos = length(unique(y)), ...)
{

    ## This function needs to work for all high level functions in the
    ## bwplot family, namely bwplot, dotplot, stripplot and
    ## barchart. For all but barchart, this is simply a question of
    ## getting the ranges. For stacked barcharts, things are slightly
    ## complicated.

    if (length(x) && length(y)) {
        
        #if (!is.numeric(x)) x <- as.numeric(x)
        #if (!is.numeric(y)) y <- as.numeric(y)

        temp <- .5  #* box.ratio/(box.ratio+1)
        if (horizontal)
            list(xlim =
                 if (stack)
             {
                 foo1 <- if (any(x > 0)) range( by(x[x>0], y[x>0, drop = TRUE], sum)) else 0
                 foo2 <- if (any(x < 0)) range( by(x[x<0], y[x<0, drop = TRUE], sum)) else 0
                 range(foo1, foo2)
             }
                 else if (is.numeric(x)) range(x[is.finite(x)], origin)
                 else levels(x),
                 ylim =
                 if (is.numeric(y)) range(y[is.finite(y)])
                 else levels(y),
                 ##ylim = c(1-temp, levels.fos + temp),
                 dx = 1,
                 dy = 1)
        else 
            list(xlim = if (is.numeric(x)) range(x[is.finite(x)]) else levels(x),
                 ##xlim = c(1-temp, levels.fos + temp),
                 ylim =
                 if (stack)
             {
                 foo1 <- if (any(y > 0)) range( by(y[y>0], x[y>0], sum)) else 0
                 foo2 <- if (any(y < 0)) range( by(y[y<0], x[y<0], sum)) else 0
                 range(foo1, foo2)
             }
                 else if (is.numeric(y)) range(y[is.finite(y)], origin)
                 else levels(y),
                 dx = 1,
                 dy = 1)
    }
    else list(c(NA, NA),
              c(NA, NA),
              1, 1)
}





panel.barchart <-
    function(x, y, box.ratio = 1,
             horizontal = TRUE,
             origin = NULL, reference = TRUE,
             stack = FALSE,
             groups = NULL, 
             col = if (is.null(groups)) bar.fill$col else
             regions$col,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    bar.fill <- trellis.par.get("bar.fill")
    reference.line <- trellis.par.get("reference.line")


    ## function defined here so that panel.barchart doesn't need to
    ## have a subscript argument (which would make stripplot always
    ## pass the subscripts to the trellis object, which is unnecessary
    ## when groups = NULL)

    groupSub <- function(groups, subscripts, ...)
        groups[subscripts]

    if (horizontal) {

        ## No grouping

        if (is.null(groups)) {
            if (is.null(origin)) {
                origin <- current.viewport()$xscale[1]
                reference <- FALSE
            }
            height <- box.ratio/(1+box.ratio)
        
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            grid.rect(gp = gpar(fill = col),
                      y = y,
                      x = rep(origin, length(y)),
                      height = rep(height, length(y)),
                      width = x - origin,
                      just = c("left", "centre"),
                      default.units = "native")
        }

        ## grouped, with stacked bars

        else if (stack) {

            if (!is.null(origin) && origin != 0)
                warning("origin forced to 0 for stacked bars")

            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            regions <- trellis.par.get("regions")
            numcol.r <- length(col)
            col <- 
                if (numcol.r <= nvals) rep(col, length = nvals)
                else col[floor(1+(vals-1)*(numcol.r-1)/(nvals-1))]

            height <- box.ratio/(1 + box.ratio)

            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            for (i in unique(y)) {
                ok <- y == i
                ord <- sort.list(groups[ok])
                pos <- x[ok][ord] > 0
                nok <- sum(pos)
                if (nok > 0)
                    grid.rect(gp = gpar(fill = col[groups[ok][ord][pos]]),
                              y = rep(i, nok),
                              x = cumsum(c(0, x[ok][ord][pos][-nok])),
                              height = rep(height, nok),
                              width = x[ok][ord][pos],
                              just = c("left", "centre"),
                              default.units = "native")
                neg <- x[ok][ord] < 0
                nok <- sum(neg)
                if (nok > 0)
                    grid.rect(gp = gpar(fill = col[groups[ok][ord][neg]]),
                              y = rep(i, nok),
                              x = cumsum(c(0, x[ok][ord][neg][-nok])),
                              height = rep(height, nok),
                              width = x[ok][ord][neg],
                              just = c("left", "centre"),
                              default.units = "native")
            }
        }

        ## grouped, with side by side bars

        else {
            if (is.null(origin)) {
                origin <- current.viewport()$xscale[1]
                reference <- FALSE
            }
            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            regions <- trellis.par.get("regions")
            numcol.r <- length(col)
            col <- 
                if (numcol.r <= nvals) rep(col, length = nvals)
                else col[floor(1+(vals-1)*(numcol.r-1)/(nvals-1))]
            
            height <- box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            for (i in unique(y)) {
                ok <- y == i
                nok <- sum(ok)
                grid.rect(gp = gpar(fill = col[groups[ok]]),
                          y = (i + height * (groups[ok] - (nvals + 1)/2)),
                          x = rep(origin, nok), 
                          height = rep(height, nok),
                          width = x[ok] - origin,
                          just = c("left", "centre"),
                          default.units = "native")
            }
        }
    }
    
    ## if not horizontal

    else {
        if (is.null(groups)) {
            if (is.null(origin)) {
                origin <- current.viewport()$yscale[1]
                reference <- FALSE
            }
            width <- box.ratio/(1+box.ratio)
        
            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            grid.rect(gp = gpar(fill = col),
                      x = x,
                      y = rep(origin, length(x)),
                      width = rep(width, length(x)),
                      height = y - origin,
                      just = c("centre", "bottom"),
                      default.units = "native")
        }
        else if (stack) {

            if (!is.null(origin) && origin != 0)
                warning("origin forced to 0 for stacked bars")

            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            regions <- trellis.par.get("regions")
            numcol.r <- length(col)
            col <- 
                if (numcol.r <= nvals) rep(col, length = nvals)
                else col[floor(1+(vals-1)*(numcol.r-1)/(nvals-1))]
            
            width <- box.ratio/(1 + box.ratio)

            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)

            for (i in unique(x)) {
                ok <- x == i
                ord <- sort.list(groups[ok])
                pos <- y[ok][ord] > 0
                nok <- sum(pos)
                if (nok > 0)
                    grid.rect(gp = gpar(fill = col[groups[ok][ord][pos]]),
                              x = rep(i, nok),
                              y = cumsum(c(0, y[ok][ord][pos][-nok])),
                              width = rep(width, nok),
                              height = y[ok][ord][pos],
                              just = c("centre", "bottom"),
                              default.units = "native")
                neg <- y[ok][ord] < 0
                nok <- sum(neg)
                if (nok > 0)
                    grid.rect(gp = gpar(fill = col[groups[ok][ord][neg]]),
                              x = rep(i, nok),
                              y = cumsum(c(0, y[ok][ord][neg][-nok])),
                              width = rep(width, nok),
                              height = y[ok][ord][neg],
                              just = c("centre", "bottom"),
                              default.units = "native")
            }

            
        }
        else {
            if (is.null(origin)) {
                origin <- current.viewport()$yscale[1]
                reference = FALSE
            }
            groups <- as.numeric(groups)
            vals <- sort(unique(groups))
            nvals <- length(vals)
            groups <- groupSub(groups, ...)

            regions <- trellis.par.get("regions")
            numcol.r <- length(col)
            col <- 
                if (numcol.r <= nvals) rep(col, length = nvals)
                else col[floor(1+(vals-1)*(numcol.r-1)/(nvals-1))]
            
            width <- box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd)
            for (i in unique(x)) {
                ok <- x == i
                nok <- sum(ok)
                grid.rect(gp = gpar(fill = col[groups[ok]]),
                          x = (i + width * (groups[ok] - (nvals + 1)/2)),
                          y = rep(origin, nok), 
                          width = rep(width, nok),
                          height = y[ok] - origin,
                          just = c("centre", "bottom"),
                          default.units = "native")
            }
        }
    }
}




panel.dotplot <-
    function(x, y, horizontal = TRUE,
             pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch,
             col = if (is.null(groups)) dot.symbol$col else sup.symbol$col,
             lty = dot.line$lty,
             lwd = dot.line$lwd,
             col.line = dot.line$col,
             levels.fos = NULL,
             groups = NULL,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")

    if (horizontal) {
        yscale <- current.viewport()$yscale
        if (is.null(levels.fos))
            levels.fos <- floor(yscale[2])-ceiling(yscale[1])+1
        panel.abline(h=1:levels.fos, col=col.line,
                     lty=lty, lwd=lwd)
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col, pch = pch, ...)
        else
            panel.superpose(x = x, y = y, groups = groups,
                            col = col, pch = pch, ...)
    }
    else {
        xscale <- current.viewport()$xscale
        if (is.null(levels.fos))
            levels.fos <- floor(xscale[2])-ceiling(xscale[1])+1
        panel.abline(v=1:levels.fos, col=col.line,
                     lty=lty, lwd=lwd)
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col, pch = pch, ...)
        else 
            panel.superpose(x = x, y = y, groups = groups,
                            col = col, pch = pch, ...)
    }
}





panel.stripplot <-
    function(x, y, jitter.data = FALSE, factor = 0.5,
             horizontal = TRUE, groups = NULL, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    y.jitter  <-
        if (horizontal && jitter.data) jitter(y, factor = factor)
        else y
    x.jitter  <-
        if (!horizontal && jitter.data) jitter(x, factor = factor)
        else x
    if (is.null(groups)) panel.xyplot(x = x.jitter, y = y.jitter, ...)
    else panel.superpose(x = x.jitter, y = y.jitter, groups = groups, ...)
}





panel.bwplot <-
    function(x, y, box.ratio=1, horizontal = TRUE, pch=box.dot$pch,
             col = box.dot$col, cex = box.dot$cex,
             fill = box.rectangle$fill, varwidth = FALSE,
             levels.fos = NULL, coef = 1.5, ...)
{
    
    x <- as.numeric(x)
    y <- as.numeric(y)

    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")

    ## In case levels.fos is not given (which should not happen), I'll
    ## be working on the premise that EACH INTEGER in the y-RANGE is a
    ## potential location of a boxplot. 

    if (horizontal) {

        maxn <- max(by(x, y, length)) ## used if varwidth = TRUE

        yscale <- current.viewport()$yscale
        if (is.null(levels.fos)) levels.fos <- floor(yscale[2])-ceiling(yscale[1])+1
        lower <- ceiling(yscale[1])
        height <- box.ratio/(1+box.ratio)
        xscale <- current.viewport()$xscale
        if (levels.fos > 0)
            for (i in 1:levels.fos) {

                yval  <- i
                stats <- boxplot.stats(x[y==yval], coef = coef)
                
                
                if (stats$n>0)
                {
                    push.viewport(viewport(y=unit(yval, "native"),
                                           height = unit((if (varwidth)
                                           sqrt(stats$n/maxn)  else 1) * height, "native"),
                                           xscale = xscale))
                    
                    r.x <- (stats$stats[2]+stats$stats[4])/2
                    r.w <- stats$stats[4]-stats$stats[2]
                    grid.rect(x = unit(r.x, "native"), width = unit(r.w, "native"),
                              gp = gpar(lwd = box.rectangle$lwd,
                              lty = box.rectangle$lty,
                              fill = fill,
                              col = box.rectangle$col))
                
                    grid.lines(x = unit(stats$stats[1:2],"native"),
                               y=unit(c(.5,.5), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(x = unit(stats$stats[4:5],"native"),
                               y=unit(c(.5,.5), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(x = unit(rep(stats$stats[1],2),"native"),
                               y=unit(c(0,1), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(x = unit(rep(stats$stats[5],2),"native"),
                               y=unit(c(0,1), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.points(x=stats$stats[3], y=.5, pch=pch, 
                                size = unit(cex * 2.5, "mm"),
                                gp = gpar(col = col, cex = cex))
                    
                    if ((l<-length(stats$out))>0)
                        grid.points(x = stats$out, y = rep(.5,l),
                                    size = unit(plot.symbol$cex * 2.5, "mm"),
                                    pch = plot.symbol$pch,
                                    gp = gpar(col = plot.symbol$col,
                                    cex = plot.symbol$cex))
                    
                    pop.viewport()
                
                }
            }
    
    }
    else {

        maxn <- max(by(y, x, length)) ## used if varwidth = TRUE

        xscale <- current.viewport()$xscale
        if (is.null(levels.fos)) levels.fos <- floor(xscale[2])-ceiling(xscale[1])+1
        lower <- ceiling(xscale[1])
        width <- box.ratio/(1+box.ratio)
        yscale <- current.viewport()$yscale
        if (levels.fos > 0)
            for (i in 1:levels.fos) {
                xval  <- i
                stats <- boxplot.stats(y[x==xval], coef = coef)

                if (stats$n>0)
                {
                    push.viewport(viewport(x = unit(xval, "native"),
                                           width = unit((if (varwidth)
                                           sqrt(stats$n/maxn)  else 1) * width, "native"),
                                           yscale = yscale))
                    
                    r.x <- (stats$stats[2]+stats$stats[4])/2
                    r.w <- stats$stats[4]-stats$stats[2]
                    grid.rect(y = unit(r.x, "native"), height = unit(r.w, "native"),
                              gp = gpar(lwd = box.rectangle$lwd,
                              lty = box.rectangle$lty,
                              fill = fill,
                              col = box.rectangle$col))
                
                    grid.lines(y = unit(stats$stats[1:2],"native"),
                               x = unit(c(.5,.5), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(y = unit(stats$stats[4:5],"native"),
                               x = unit(c(.5,.5), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(y = unit(rep(stats$stats[1],2),"native"),
                               x = unit(c(0,1), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.lines(y = unit(rep(stats$stats[5],2),"native"),
                               x = unit(c(0,1), "npc"),
                               gp = gpar(col = box.umbrella$col,
                               lwd = box.umbrella$lwd,
                               lty = box.umbrella$lty))
                    
                    grid.points(y = stats$stats[3], x = .5, pch = pch, 
                                size = unit(cex * 2.5, "mm"),
                                gp = gpar(col = col, cex = cex))
                    
                    if ((l<-length(stats$out))>0)
                        grid.points(y = stats$out, x = rep(.5,l),
                                    size = unit(plot.symbol$cex * 2.5, "mm"),
                                    pch = plot.symbol$pch,
                                    gp = gpar(col = plot.symbol$col,
                                    cex = plot.symbol$cex))
                    
                    pop.viewport()
                
                }
            }
    
    }
}

# The following needs to work:

# k <- 10# (optional)
# fubar <- function() {
#     k <- -1
#     data = list(x=1:10)
#     names(data$x) <- 1:10
#     barchart(x^k, data)
# }
# fubar()



dotplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.dotplot",
             groups = NULL,
             ...,
             subset = TRUE)
{

    ## m <- match.call(expand.dots = FALSE)
    ## lapply(dots, eval, data, parent.frame())))

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    try(formula <- eval(formula), silent = TRUE)
    foo <- substitute(formula)
    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", deparse(foo)))
        environment(formula) <- parent.frame()
    }
    call.list <- c(list(formula = formula, data = data,
                        groups = groups,
                        subset = subset,
                        panel = panel,
                        box.ratio = 0),
                   dots)
    do.call("bwplot", call.list)
}



barchart <-
    function(formula,
             data = parent.frame(),
             panel = "panel.barchart",
             box.ratio = 2,
             groups = NULL,
             ...,
             subset = TRUE)
{

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    foo <- substitute(formula)
    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", deparse(foo)))
        environment(formula) <- parent.frame()
    }
    call.list <- c(list(formula = formula, data = data,
                        groups = groups,
                        subset = subset,
                        panel = panel,
                        box.ratio = box.ratio),
                   dots)
    do.call("bwplot", call.list)

}


stripplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.stripplot",
             jitter = FALSE,
             factor = .5,
             box.ratio = if (jitter) 1 else 0,
             groups = NULL,
             ...,
             subset = TRUE)
{

    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    foo <- substitute(formula)
    if (!(is.call(foo) && foo[[1]] == "~")) {
        formula <- as.formula(paste("~", deparse(foo)))
        environment(formula) <- parent.frame()
    }
    call.list <- c(list(formula = formula, data = data,
                        panel = panel,
                        jitter = jitter,
                        factor = factor,
                        groups = groups,
                        subset = subset,
                        box.ratio = box.ratio),
                   dots)

    do.call("bwplot", call.list)

}


bwplot <-
    function(formula,
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
{

    ##m <- match.call(expand.dots = FALSE)
    ##dots <- m$...
    ##dots <- lapply(dots, eval, data, parent.frame())

    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

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
    y <- form$left
    if (is.null(y))
        y <- rep(if (is.null(names(x))) '' else names(x), length = length(x))
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }


    if (is.null(horizontal)) {
        horizontal <-
            if ((is.factor(x) || is.shingle(x)) && is.numeric(y)) FALSE
            else TRUE
    }


    if (horizontal) {
        if (!(is.numeric(x))) {
            warning("x should be numeric")
        }
        y <- as.factorOrShingle(y)
        is.f.y <- is.factor(y)  # used throughout the rest of the code
        num.l.y <- nlevels(y)

        if (missing(xlab)) xlab <- form$right.name
        if (missing(ylab)) ylab <- if (is.f.y) NULL else form$left.name
    }
    else {
        if (!(is.numeric(y))) {
            warning("y should be numeric")
        }
        x <- as.factorOrShingle(x)
        is.f.x <- is.factor(x)  # used throughout the rest of the code
        num.l.x <- nlevels(x)

        if (missing(ylab)) ylab <- form$left.name
        if (missing(xlab)) xlab <- if (is.f.x) NULL else form$right.name
    }

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

    ##scales <- 
    ##if (missing(scales)) scales 
    ##else eval(m$scales, data, parent.frame())


    ## The following is to make the default alternating FALSE for factors
    if (is.character(scales)) scales <- list(relation = scales)
    if (is.null(scales$alternating)) {
        if (horizontal) {
            if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
            else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
        ## bug if y="free" ? but who cares
        }
        else {
            if (is.null(scales$x)) scales$x <- list(alternating = FALSE)
            else if (is.null(scales$x$alternating)) scales$x$alternating <- FALSE
        ## bug if x="free" ? but who cares
        }
    }
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
        ## warning("Are you sure you want log scale for y ?")
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
    foo$panel.args.common$box.ratio <- box.ratio
    foo$panel.args.common$horizontal <- horizontal
    foo$panel.args.common$levels.fos <- ## fos - the factor/shingle in x/y
        if (horizontal) num.l.y else num.l.x
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

                    if (horizontal) {
                        if (is.f.y) {
                            foo$panel.args[[panel.number]] <-
                                list(x = x[id],
                                     ##y = as.numeric(y[id]))
                                     y = y[id])
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    subscr[id]
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            if (subscripts) panel.subscr <- numeric(0)
                            for (k in 1:num.l.y) {
                                tid <- id & (y >= levels(y)[[k]][1]) & (y <= levels(y)[[k]][2])
                                panel.x <- c(panel.x, x[tid])
                                panel.y <- c(panel.y, rep(k,length(tid[tid])))
                                if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                            }
                            foo$panel.args[[panel.number]] <-
                                list(x = panel.x,
                                     y = panel.y)
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    panel.subscr

                        }
                    }
                    else {
                        if (is.f.x) {
                            foo$panel.args[[panel.number]] <-
                                ##list(x = as.numeric(x[id]),
                                list(x = x[id],
                                     y = y[id])
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    subscr[id]
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            if (subscripts) panel.subscr <- numeric(0)
                            for (k in 1:num.l.x) {
                                tid <- id & (x >= levels(x)[[k]][1]) & (x <= levels(x)[[k]][2])
                                panel.y <- c(panel.y, y[tid])
                                panel.x <- c(panel.x, rep(k,length(tid[tid])))
                                if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                            }
                            foo$panel.args[[panel.number]] <-
                                list(x = panel.x,
                                     y = panel.y)
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    panel.subscr
                        }
                    }

                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.bwplot,
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

