

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



prepanel.default.tmd <-
    function(...)
    prepanel.default.xyplot(...)



panel.tmd <- function(...) {
    panel.abline(h=0)
    panel.xyplot(...)
}


## Fixme: log scales not handled
tmd <-
    function(object,
             aspect = "fill",
             as.table = object$as.table,
             between = list(x=object$x.between,y=object$y.between),
             key = object$key,
             layout = object$layout,
             main = object$main,
             page = object$page,
             panel = "panel.tmd",
             par.strip.text = object$par.strip.text, 
             prepanel = NULL,
             scales = list(),
             strip = object$strip,
             sub = object$sub,
             xlab = "mean",
             xlim = NULL,
             ylab = "difference",
             ylim = NULL,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(as.table = as.table,
                          aspect = aspect,
                          between = between,
                          key = key,
                          page = page,
                          main = main,
                          panel = panel,
                          sub = sub,
                          par.strip.text = par.strip.text,
                          strip = strip,
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
        ylim <- foo$x.scales$limit
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) { ## problem
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog) { ## problem
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }
    
    ## Step 5: Process cond

    foo$condlevels <- object$condlevels

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- c(object$panel.args.common, dots)

    if (!missing(layout)) {
        number.of.cond <- length(foo$condlevels)
        cond.max.level <- integer(number.of.cond)
        for(i in 1:number.of.cond) {
            cond.max.level[i] <-
                if (is.character(foo$condlev[[i]])) length(foo$condlev[[i]])
                else nrow(foo$condlev[[i]])
        }
        foo$skip <- !unlist(lapply(object$panel.args , is.list))
        layout <- compute.layout(layout, cond.max.level, skip = foo$skip)
    }
    plots.per.page <- max(layout[1] * layout[2], layout[2])
    number.of.pages <- layout[3]
    foo$skip <- rep(foo$skip, length = plots.per.page)
    foo$layout <- layout
    nplots <- plots.per.page * number.of.pages

    foo$panel.args <- object$panel.args

    if ("x" %in% names(foo$panel.args.common)) {
        ## this would happen with subscripts. assuming that
        ## y would also be there then
        q < foo$panel.args.common
        x <- (q$x+q$y)/2
        y <- q$y-q$x       # will stop if any errors, not putting any more handlers
        foo$panel.args.common$x <- x
        foo$panel.args.common$y <- y
    }
    else {
        count <- 1
        for (p in foo$panel.args)
            if (is.logical(p)) # which means skip = T for this panel
                count <- count + 1 
            else {
                x <- (p$x+p$y)/2
                y <- p$y-p$x

                foo$panel.args[[count]]$x <- x
                foo$panel.args[[count]]$y <- y

                count <- count + 1
            }
    }    

    foo <- c(foo,
             limits.and.aspect(prepanel.default.tmd,
                               prepanel = prepanel, 
                               have.xlim = have.xlim, xlim = xlim, 
                               have.ylim = have.ylim, ylim = ylim, 
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = aspect,
                               nplots = nplots))

    class(foo) <- "trellis"
    foo
}
