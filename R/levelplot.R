


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
    ## not allowing POSIXt objects here
    x <- as.numeric(x)
    y <- as.numeric(y)

    xlim <- range(x[subscripts] + wx[subscripts]/2,
                  x[subscripts] - wx[subscripts]/2)
    ylim <- range(y[subscripts] + wy[subscripts]/2,
                  y[subscripts] - wy[subscripts]/2)
    list(xlim = extend.limits(xlim, prop = -0.0614),
         ylim = extend.limits(ylim, prop = -0.0614),
         dx = 1, dy = 1)
}




# index2seq <- function(x) {
#     print(x)
#     n <- length(x)
#     indices <- (1:n)[x==2]
#     ans <- rep(0, n)
#     for (i in indices)
#         ans[1:i] <- ans[1:i] + 1
#     ans
# }




panel.levelplot <-
    function(x, y, z, wx, wy, zcol, col.regions, subscripts,
             at = mean(z), labels = NULL, contour = TRUE, region = TRUE,
             col = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)

    ## z not really needed here, but probably would be for contourplot
    if (any(subscripts)) {
        if (region) {
            for (i in seq(along = col.regions)) {
                ok <- (zcol[subscripts]==i)
                if (any(ok))
                    grid.rect(x = x[subscripts][ok],
                              y = y[subscripts][ok],
                              width = wx[subscripts][ok],
                              height = wy[subscripts][ok],
                              default.units = "native",
                              gp = gpar(fill=col.regions[i], col = NULL))
            }
        }
        if (contour) {
            add.line <- trellis.par.get("add.line")
            ux <- as.double(sort(unique(x[subscripts])))
            uy <- as.double(sort(unique(y[subscripts])))
            ord <- order(x[subscripts], y[subscripts])
            m <- z[subscripts][ord] + 10e-12
            for (i in seq(along = at)) {
                val <- .Call("cont", m, ux, uy, as.double(at[i]),
                             length(ux), length(uy), PACKAGE="lattice")
                if (is.null(labels))
                    lsegments(val[[1]], val[[2]], val[[3]], val[[4]],
                              col = col, lty = lty, lwd = lwd)
                else {
                    if (length(val[[1]]) > 3) {
                        ltext(lab = labels$lab[i],
                              x = .5 * (val[[1]][1]+val[[3]][1]),
                              y = .5 * (val[[2]][1]+val[[4]][1]))
                        lsegments(val[[1]][-(1:2)], val[[2]][-(1:2)],
                                  val[[3]][-(1:2)], val[[4]][-(1:2)])
                    }
                }
            }
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
             cuts = 7,
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
                   cuts = cuts, contour = contour,
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
             col.regions = trellis.par.get("regions")$col,
             colorkey = region,
             contour = FALSE,
             cuts = 15,
             labels = TRUE,
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

    #if(!(is.numeric(x) && is.numeric(y) && is.numeric(z)))
    #    warning("x, y and z should be numeric")
    #x <- as.numeric(x)
    #y <- as.numeric(y)
    #z <- as.numeric(z)

    zrng <- extend.limits(range(z[!is.na(z)]))
    if (missing(at))
        at <-
            if (pretty) lpretty(zrng, cuts)
            else seq(zrng[1], zrng[2], length = cuts+2)
    

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab), dots))
                          

    ## Processing the labels argument
    if (is.logical(labels) && !labels) labels <- NULL
    else {
        if (is.logical(labels)) labels <- format(at)
        text <- trellis.par.get("add.text") # something better ?
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1, rot = 0)
        labels <- list(label = if (is.list(labels)) labels[[1]] else labels,
                       col = text$col, rot = text$rot,
                       cex = text$cex, font = text$font)
        if (is.list(labels)) labels[names(labels)] <- labels
        if (!is.characterOrExpression(labels$label))
            labels$label <- format(at)
    }

    
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

    ## scales <- eval(substitute(scales), data, parent.frame())
    if (is.character (scales)) scales <- list(relation = scales)
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
    cond.max.level <- unlist(lapply(cond, nlevels))

    id.na <- is.na(x)|is.na(y)|is.na(z)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    ## Most levelplot/contourplot specific code here


    ## region
    numcol <- length(at)-1
    numcol.r <- length(col.regions)

    col.regions <-
        if (numcol.r <= numcol)
            rep(col.regions, length = numcol)
        else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]
    
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

    ## Current algo unnecessarily memory intensive ?

    
    
    ## I'm going to create vectors parallel to x y etc which would
    ## give the widths and heights of the rectangles for each point.
    ## My algo works only when the x's and y's are really evaluated
    ## on a grid, that is, there is no numerical error. Splus also
    ## doesn't work (in any meningful way, at least) in such cases,
    ## but behaviour would be dissimilar in that case.

    ux <- sort(unique(x[!is.na(x)]))
    dux <- diff(ux)
    wux <- .5 * (c(dux[1], dux) + c(dux, dux[length(dux)]))
    ##wx <- wux[match(x[!is.na(x)], ux)]
    wx <- wux[match(x, ux)]
    uy <- sort(unique(y[!is.na(y)]))
    duy <- diff(uy)
    wuy <- .5 * (c(duy[1], duy) + c(duy, duy[length(duy)]))
    ##wy <- wuy[match(y[!is.na(y)], uy)]
    wy <- wuy[match(y, uy)]

    zcol <- numeric(length(z))
    for (i in seq(along=col.regions))
        zcol[!id.na & z>=at[i] & z<at[i+1]] <- i

    foo$panel.args.common <-
        c(list(x=x, y=y, z=z, at=at, labels=labels,
               region = region, contour = contour,
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
    cond.current.level <- rep(1, number.of.cond)
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








