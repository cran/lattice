


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




prepanel.default.levelplot <-
    function(x, y, wx, wy, subscripts, ...)
{
    xlim <- range(x[subscripts] + wx[subscripts]/2,
                  x[subscripts] - wx[subscripts]/2)
    ylim <- range(y[subscripts] + wy[subscripts]/2,
                  y[subscripts] - wy[subscripts]/2)
    list(xlim = extend.limits(xlim, prop = -0.0614),
         ylim = extend.limits(ylim, prop = -0.0614),
         dx = 1, dy = 1)
}



panel.levelplot <-
    function(x, y, z, wx, wy, zcol, col.regions, subscripts, ...)
{
    if (any(subscripts)) {
        for (i in seq(along=col.regions)) {
            ok <- (zcol==i)[subscripts]
            grid.rect(x = x[ok],
                      y = y[ok],
                      width = wx[ok],
                      height = wy[ok],
                      default.units = "native",
                      gp = gpar(fill=col.regions[i], col = NULL))
        }
    }
}








contourplot <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             layout = NULL,
             panel = "panel.levelplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             contour = TRUE,
             pretty = TRUE,
             region = FALSE,
             ...,
             subscripts = TRUE,
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

    do.call("levelplot",
            c(list(formula = formula, data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   contour = contour,
                   pretty = pretty,
                   region = region),
              dots))
}







levelplot <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             layout = NULL,
             panel = "panel.levelplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             at,
             col.regions = terrain.colors(length(at)-1),
             colorkey = region,
             contour = FALSE,
             cuts = 15,
             labels = format(at),
             pretty = FALSE,
             region = TRUE,
             ...,
             subscripts = TRUE,
             subset = TRUE)
{

    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    formula <- eval(substitute(formula), data, parent.frame())
    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data, dim = 3)
        else {
            if (!is.matrix(formula)) stop("invalid formula")
            else {
                tmp <- expand.grid(1:nrow(formula), 1:ncol(formula))
                list(left = as.vector(formula),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     left.name = "",
                     right.x.name = "", right.y.name = "")
            }
        }

    cond <- form$condition
    number.of.cond <- length(cond)
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())
    subscr <- seq(along=x)
    x <- x[subset, drop = TRUE]
    y <- y[subset, drop = TRUE]
    z <- z[subset, drop = TRUE]
    subscr <- subscr[subset, drop = TRUE]
    
    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name

    if(!(is.numeric(x) && is.numeric(y) && is.numeric(z)))
        warning("x, y and z should be numeric")
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
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


    id.na <- is.na(x)|is.na(y)|is.na(z)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    ## Most levelplot/contourplot specific code here

    zrng <- extend.limits(range(z[!id.na]))
    if (missing(at))
        at <- if (pretty) pretty(zrng, cuts)
        else seq(zrng[1], zrng[2], length = cuts+2)

    if (region) {
        numcol <- length(at)-1
        numcol.r <- length(col.regions)
        col.regions <-
            if (numcol.r <= numcol)
                rep(col.regions, length = numcol)
            else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]

    }
    else {
        at <- sort(jitter(c(at, at), factor = 2))
        col.regions <-
            rep(c("black", trellis.par.get("background")$col),
                length = length(at)-1)
    }
    ## need to handle contour = TRUE

    if (is.logical(colorkey)) {
        if (colorkey) foo$colorkey <-
            list(space = "right", col = col.regions,
                 at = at, tick.number = 7)
    }
    else if (is.list(colorkey)) {
        foo$colorkey <- colorkey
        if (is.null(foo$colorkey$col)) foo$colorkey$col <- col.regions
        if (is.null(foo$colorkey$at)) foo$colorkey$at <- at
    }

    
    ## I'm going to create vectors parallel to x y etc which would
    ## give the widths and heights of the rectangles for each point.
    ## My algo works only when the x's and y's are really evaluated
    ## on a grid, that is, there is no numerical error. Splus also
    ## doesn't work (in any meningful way, at least) in such cases,
    ## but behaviour would be dissimilar in that case.

    ux <- sort(unique(x))
    dux <- diff(ux)
    wux <- .5 * (c(dux[1], dux) + c(dux, dux[length(dux)]))
    wx <- wux[match(x, ux)]
    uy <- sort(unique(y))
    duy <- diff(uy)
    wuy <- .5 * (c(duy[1], duy) + c(duy, duy[length(duy)]))
    wy <- wuy[match(y, uy)]

    zcol <- numeric(length(z))
    for (i in seq(along=col.regions))
        zcol[z>=at[i] & z<at[i+1]] <- i

    foo$panel.args.common <-
        c(list(x=x, y=y, z=z,
               wx=wx, wy=wy, zcol=zcol, col.regions=col.regions),
          dots)

    if (!is.null(groups)) foo$panel.args.common$groups <- groups

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

                    foo$panel.args[[panel.number]] <- 
                        list(subscripts = subscr[id])

                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.levelplot,
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








