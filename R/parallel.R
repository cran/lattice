

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



prepanel.default.parallel <-
    function(x, y, type, ...)
{
    list(xlim = c(0,1),
         ylim = c(0,1),
         dx = 1,
         dy = 1)
}



panel.parallel <- function(z, subscripts,
                           col=superpose.line$col,
                           lwd=superpose.line$lwd,
                           lty=superpose.line$lty, ...)
{
    superpose.line <- trellis.par.get("superpose.line")
    reference.line <- trellis.par.get("reference.line")

    n.r <- ncol(z)
    n.c <- length(subscripts)
    col <- rep(col, length=n.c)
    lty <- rep(lty, length=n.c)
    lwd <- rep(lwd, length=n.c)

    llim <- numeric(n.r)
    ulim <- numeric(n.r)
    dif <- numeric(n.r)
    if(n.r>0)
        for(i in 1:n.r) {
            grid.lines(x = c(0,1), y = c(i,i),
                       default.units = "native",
                       gp = gpar(col = reference.line$col,
                       lwd = reference.line$lwd,
                       lty = reference.line$lty))
            llim[i] <- range(z[,i])[1]
            ulim[i] <- range(z[,i])[2]
            dif[i] <- ulim[i] - llim[i]
        }
   

    for (i in seq(along=subscripts))
    {
        x <- (as.numeric(z[subscripts[i],,])-llim)/dif
        grid.lines(x = x,
                   y=1:n.r, 
                   gp = gpar(col=col[i], lty=lty[i], lwd=lwd[i]),
                   default.units="native")
    }
    
}






parallel <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             between = list(x = 0.5, y = 0.5),
             layout = NULL,
             panel = "panel.parallel",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = NULL,
             xlim,
             ylab = NULL,
             ylim,
             varnames,
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

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <- latticeParseFormula(formula, data)
    cond <- form$condition
    number.of.cond <- length(cond)
    x <- as.data.frame(form$right)
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, nrow(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }
    if (!missing(varnames)) colnames(x) <-
        eval(substitute(varnames), data, parent.frame())

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())
    if ("subscripts" %in% names(formals(eval(panel)))) subscripts <- TRUE
    subscr <- seq(along=x[,1])
    x <- x[subset,, drop = TRUE]
    subscr <- subscr[subset, drop = TRUE]
    
    ##if(!(is.numeric(x) && is.numeric(y)))
    ##    warning("Both x and y should be numeric")    WHAT ?


    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = aspect,
                          between = between,
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
        foo$xlab$label <- "Scatter Plot Matrix"
    if (is.list(foo$ylab) && !is.character(foo$ylab$label))
        foo$ylab <- NULL

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## overriding at and labels, maybe not necessary
    
    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character(scales)) scales <- list(relation = scales)
    if (is.null(scales$alternating)) {
        if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
        else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
        ## bug if y="free" but who cares
    }
    foo <- c(foo, 
             do.call("construct.scales", scales))
    foo$x.scales$at <- c(0,1)
    foo$x.scales$labels <- c("Min","Max")
    foo$y.scales$at <- 1:ncol(x)
    foo$y.scales$labels <- colnames(x)
    

    ## Step 3: Decide if limits were specified in call:

    if (missing(xlim)) xlim <- extend.limits(c(0,1))
    if (missing(ylim)) ylim <- extend.limits(c(1,ncol(x)), prop = 0.03) 
    have.xlim <- TRUE
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- TRUE
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
        foo$x.scales$log <- FALSE
        ## This is because No further changes will be
        ## necessary while printing since x-axes are not
        ## marked (many x axes)
    }
    if (have.ylog) {
        warning("cannot have log y-scale")
        foo$y.scales$log <- FALSE
    }
    
    ## Step 5: Process cond

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- FALSE
    for (j in 1:ncol(x))
        id.na <- id.na | is.na(x[,j])
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args


    foo$panel.args.common <-
        c(list(z = x, groups = groups), dots)

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

                    foo$panel.args[[panel.number]] <-
                        list(subscripts = subscr[id])

                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.parallel,
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













