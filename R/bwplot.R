

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




prepanel.default.bwplot <-
    function(x, y, box.ratio, num.l.y = length(unique(y)), ...)
{
    temp <- .5*box.ratio/(box.ratio+1)
    list(xlim = range(x[is.finite(x)]),
         ylim = c(1-temp, num.l.y + temp),
         dx = 1,
         dy = 1)
}





panel.barchart <-
    function(x, y, box.ratio=1, col=bar.fill$col, ...)
{

    bar.fill <- trellis.par.get("bar.fill")

    ## I'll be working on the premise that EACH INTEGER in the y-RANGE
    ## is a potential location of a boxplot. This should work for the default
    ## case, and other cases (ylim non-default) are not well defined anyway.
    
    xmin <- current.viewport()$xscale[1]
    yscale <- current.viewport()$yscale
    levels.y <- floor(yscale[2])-ceiling(yscale[1])+1
    height <- box.ratio/(1+box.ratio)
    
    for (i in seq(along=x)) {
        grid.rect(gp = gpar(fill = col),
                  y = y[i],
                  x = unit(0,"npc"),
                  height = height,
                  width = x[i]-xmin,
                  just = c("left", "centre"),
                  default.units = "native")
    }
    
}






panel.dotplot <-
    function(x, y, pch = dot.symbol$pch, col = dot.symbol$col, ...)
{

    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")

    ## as in panel.bwplot I'll be working on the premise that
    ## EACH INTEGER in the y-RANGE
    ## is a potential location of a dot-line. This should work for the default
    ## case, and other cases (ylim non-default) are not well defined anyway.
    
    yscale <- current.viewport()$yscale
    levels.y <- floor(yscale[2])-ceiling(yscale[1])+1

    panel.abline(h=1:levels.y, col=dot.line$col,
                 lty=dot.line$lty, lwd=dot.line$lwd)

    panel.xyplot(x = x, y = y, col = col, pch = pch, ...)

}





panel.stripplot <-
    function(x, y, jitter.data = FALSE, factor = 0.5,
             pch=plot.symbol$pch, col=plot.symbol$col, ...)
{

    plot.symbol <- trellis.par.get("plot.symbol")
    y.jitter  <-
        if (jitter.data) jitter(y, factor = factor)
        else y
    panel.xyplot(x = x, y = y.jitter, pch = pch, col = col, ...)
    
}





panel.bwplot <-
    function(x, y, box.ratio=1, pch=box.dot$pch,
             col=box.dot$col, cex = box.dot$cex, ...)
{
    
    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")
    
    ## OK. I'll be working on the premise that EACH INTEGER in the y-RANGE
    ## is a potential location of a boxplot. This should work for the default
    ## case, and other cases (ylim non-default) are not well defined anyway.
    
    yscale <- current.viewport()$yscale
    levels.y <- floor(yscale[2])-ceiling(yscale[1])+1
    lower <- ceiling(yscale[1])
    height <- box.ratio/(1+box.ratio)
    
    xscale <- current.viewport()$xscale
    
    if (levels.y > 0)
        for (i in 1:levels.y) {
            
            yval  <- lower+i-1
            stats <- boxplot.stats(x[y==yval])
            
            if (stats$n>0)
            {
                push.viewport(viewport(y=unit(yval, "native"),
                                            height = unit(height, "native"),
                                            xscale = xscale))
                
                r.x <- (stats$stats[2]+stats$stats[4])/2
                r.w <- stats$stats[4]-stats$stats[2]
                grid.rect(x = unit(r.x, "native"), width = unit(r.w, "native"),
                          gp = gpar(lwd = box.rectangle$lwd,
                          lty = box.rectangle$lty,
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



dotplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.dotplot",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             ...,
             subset = TRUE)
{
    ## m <- match.call(expand.dots = FALSE)
    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    do.call("bwplot",
            c(list(formula = formula, data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   box.ratio = 0),
              dots))
    ## lapply(dots, eval, data, parent.frame())))
}



barchart <-
    function(formula,
             data = parent.frame(),
             panel = "panel.barchart",
             prepanel = NULL,
             strip = TRUE,
             box.ratio = 2,
             groups = NULL,
             ...,
             subset = TRUE)
{
    ## m <- match.call(expand.dots = FALSE)
    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    do.call("bwplot",
            c(list(formula = formula, data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   box.ratio = box.ratio),
              dots))
    ## lapply(dots, eval, data, parent.frame())))
}


stripplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.stripplot",
             prepanel = NULL,
             strip = TRUE,
             jitter = FALSE,
             factor = .5,
             box.ratio = if (jitter) 1 else 0,
             groups = NULL,
             ...,
             subset = TRUE)
{
    ## m <- match.call(expand.dots = FALSE)
    dots <- list(...)
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    do.call("bwplot",
            c(list(formula = formula, data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   jitter = jitter, factor = factor, 
                   box.ratio = box.ratio),
              dots))
    ## lapply(dots, eval, data, parent.frame())))
}


bwplot <-
    function(formula,
             data = parent.frame(),
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
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ##m <- match.call(expand.dots = FALSE)
    ##dots <- m$...
    ##dots <- lapply(dots, eval, data, parent.frame())

    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <- latticeParseFormula(formula, data)
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


    groups <- eval(groups, data, parent.frame())
    subset <- eval(subset, data, parent.frame())
    if ("subscripts" %in% names(formals(eval(panel)))) subscripts <- TRUE
    if (subscripts) subscr <- seq(along=x)
    x <- x[subset, drop = TRUE]
    y <- y[subset, drop = TRUE]
    if (subscripts) subscr <- subscr[subset, drop = TRUE]
    

    if(!(is.numeric(x))) {
        x <- as.numeric(x)
        warning("x should be numeric")
    }
    y <- as.factorOrShingle(y)
    is.f.y <- is.factor(y)  # used throughout the rest of the code
    num.l.y <- numlevels(y)

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- if (is.f.y) NULL else form$left.name

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
    if (is.list(foo$xlab) && !is.character(foo$xlab$label))
        foo$xlab$label <- form$right.name
    if (is.list(foo$ylab) && !is.character(foo$ylab$label))
        foo$ylab$label <- form$left.name

    ## Step 2: Compute scales.common (leaving out limits for now)

    ##scales <- 
    ##if (missing(scales)) scales 
    ##else eval(m$scales, data, parent.frame())

    if (is.character(scales)) scales <- list(relation = scales)
    if (is.null(scales$alternating)) {
        if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
        else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
        ## bug if y="free" but who cares
    }
    foo <- c(foo, 
             do.call("construct.scales", scales))
    if (is.logical(foo$y.scales$at)) foo$y.scales$at <- 1:num.l.y
    if (is.f.y && is.logical(foo$y.scales$labels))
        foo$y.scales$labels <- levels(y)


    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit)) {
        have.ylim <- TRUE
        ylim <- foo$x.scales$limit
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
        warning("Are you sure you want log scale for y ?")
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, numlevels))


    id.na <- is.na(x)|is.na(y)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args


    foo$panel.args.common <- dots
    foo$panel.args.common$box.ratio <- box.ratio
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
                            ((var$x >=
                              var$int[cond.current.level[i], 1])
                             & (var$x <=
                                var$int[cond.current.level[i], 2]))
                        else (as.numeric(var) == cond.current.level[i])
                    }

                    if (is.f.y) 
                        foo$panel.args[[panel.number]] <-
                            list(x = x[id],
                                 y = as.numeric(y[id]))
                            
                    else {  # shingle
                        panel.x <- numeric(0)
                        panel.y <- numeric(0)
                        for (k in 1:num.l.y) {
                            tid <- id & (y$x >= y$int[k,1]) & (y$x <= y$int[k,2])
                            panel.x <- c(panel.x, x[tid])
                            panel.y <- c(panel.y, rep(k,length(tid[tid])))
                        }
                        foo$panel.args[[panel.number]] <-
                            list(x = panel.x,
                                 y = panel.y)
                    }
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
                               num.l.y = num.l.y))

    class(foo) <- "trellis"
    foo
}














