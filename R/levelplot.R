


### Copyright 2001-2003  Deepayan Sarkar <deepayan@stat.wisc.edu>
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
    function(x, y, subscripts, ...)
{
    if (is.numeric(x)) {
        x <- as.numeric(x[subscripts])
        ux <- sort(unique(x[!is.na(x)]))
        xlim <-
            if (length(ux) < 2) ux + c(-1, 1)
            else c(3 * ux[1] - ux[2], 3 * ux[length(ux)] - ux[length(ux)-1])/2
    }
    else x <- x[subscripts]
    if (is.numeric(y)) {
        y <- as.numeric(y[subscripts])
        uy <- sort(unique(y[!is.na(y)]))
        ylim <-
            if (length(uy) < 2) uy + c(-1, 1)
            else c(3 * uy[1] - uy[2], 3 * uy[length(uy)] - uy[length(uy)-1])/2
    }
    else y <- y[subscripts]

    list(xlim =
         if (is.numeric(x)) extend.limits(xlim, prop = -0.0614)
         else levels(x),
         ylim = if (is.numeric(y)) extend.limits(ylim, prop = -0.0614)
         else levels(y),
         dx = if (is.numeric(x)) length(ux) else 1,
         dy = if (is.numeric(y)) length(uy) else 1)
}






panel.levelplot <-
    function(x, y, z, zcol,
             subscripts,
             at = mean(z),
             shrink,
             labels = NULL,
             label.style = c("mixed", "flat", "align"),
             contour = TRUE,
             region = TRUE,
             col = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             cex = add.text$cex,
             font = add.text$font,
             col.text = add.text$col,
             ...,
             col.regions)
{
    label.style <- match.arg(label.style)
    x <- as.numeric(x[subscripts])
    y <- as.numeric(y[subscripts])

    fullZrange <- range(as.numeric(z), na.rm = TRUE) # for shrinking
    z <- as.numeric(z[subscripts])
    zcol <- as.numeric(zcol[subscripts])

    ## Do we need a zlim-like argument ?

    shrinkx <- c(1, 1)
    shrinky <- c(1, 1)
    if (!missing(shrink)) {
        if (is.numeric(shrink)) {
            shrinkx <- rep(shrink, length = 2)
            shrinky <- rep(shrink, length = 2)
        }
        else if (is.list(shrink)) {
            shrinkx <- rep(shrink[[1]], length = 2)
            shrinky <- rep(shrink[[1]], length = 2)
            if ("x" %in% names(shrink)) shrinkx <- rep(shrink$x, length = 2)
            if ("y" %in% names(shrink)) shrinky <- rep(shrink$y, length = 2)
        }
        else warning("Invalid shrink, ignored")
    }

    scaleWidth <- function(z, min = .8, max = .8, zl = range(z, na.rm = TRUE)) {
        if (diff(zl) == 0) rep(.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1]) / diff(zl)
    }

    
    if (any(subscripts)) {

        ## sorted unique values of x 
        ux <- sort(unique(x[!is.na(x)]))
        ## actual box boundaries (x axis)
        bx <- c(3 * ux[1] - ux[2],
                ux[-length(ux)] + ux[-1],
                3 * ux[length(ux)] - ux[length(ux)-1]) / 2
        ## dimension of rectangles
        lx <- diff(bx)
        ## centers of rectangles
        cx <- (bx[-1] + bx[-length(bx)])/2

        ## same things for y
        uy <- sort(unique(y[!is.na(y)]))
        by <- c(3 * uy[1] - uy[2],
                uy[-length(uy)] + uy[-1],
                3 * uy[length(uy)] - uy[length(uy)-1]) / 2
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2


        idx <- match(x, ux)
        idy <- match(y, uy)

        if (region) 
            grid.rect(x = cx[idx],
                      y = cy[idy],
                      width = lx[idx] * scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange),
                      height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], fullZrange),
                      default.units = "native",
                      gp = gpar(fill=col.regions[zcol], col = NULL))


        ################################################
#         dux <- diff(ux)
#         wux <- .5 * (c(dux[1], dux) + c(dux, dux[length(dux)]))
#         ##wx <- wux[match(x[!is.na(x)], ux)]
#         wx <- wux[match(x, ux)]
#         uy <- sort(unique(y[!is.na(y)]))
#         duy <- diff(uy)
#         wuy <- .5 * (c(duy[1], duy) + c(duy, duy[length(duy)]))
#         ##wy <- wuy[match(y[!is.na(y)], uy)]
#         wy <- wuy[match(y, uy)]

#         if (region) {
#             for (i in seq(along = col.regions)) {
#                 ok <- (zcol[subscripts]==i)
#                 if (any(ok))
#                     grid.rect(x = x[ok],
#                               y = y[ok],
#                               width = wx[ok],
#                               height = wy[ok],
#                               default.units = "native",
#                               gp = gpar(fill=col.regions[i], col = NULL))
#             }
#         }
        ################################################


        
        if (contour) {


            ## FIXME:

            ## bad hack for when z contains NA's. Including anyway
            ## since the result would be much worse without it (well,
            ## that at least had the advantage of being obviously
            ## broken, as opposed to this which will in certain cases
            ## silently give the wrong result... still, shouldn't be
            ## that bad)

            ## z[is.na(z)] <- min(z, na.rm = TRUE)



            add.line <- trellis.par.get("add.line")
            add.text <- trellis.par.get("add.text")
            ux <- as.double(ux)
            uy <- as.double(uy)
            ord <- order(x, y)
            m <- z[ord] + 10e-12 ## some problems otherwise
            
            for (i in seq(along = at)) {
                val <- .Call("calculateContours", m, ux, uy, as.double(at[i]),
                             length(ux), length(uy), PACKAGE="lattice")
                if (length(val[[1]]) > 3) {
                    if (is.null(labels))
                        lsegments(val[[1]], val[[2]], val[[3]], val[[4]],
                                  col = col, lty = lty, lwd = lwd)
                    else {

                        if (label.style == "flat") {
                            slopes <-
                                (val[[4]] - val[[2]]) /
                                    (val[[3]] - val[[1]])
                            textloc <- which(abs(slopes) == min(abs(slopes)))[1]
                            ##skiploc <- numeric(0)
                            rotangle <- 0
                        }
                        else if (label.style == "align") {
                            rx <- range(ux)
                            ry <- range(uy)
                            depth <- pmin( (val[[1]] + val[[3]] - 2 * rx[1])/diff(rx),
                                          (2 * rx[2] - val[[1]] - val[[3]])/diff(rx),
                                          (val[[2]] + val[[4]] - 2 * ry[1])/diff(ry),
                                          (2 * ry[2] - val[[2]] - val[[4]])/diff(ry))
                            textloc <- which(depth == max(depth))[1]
                            slopes <-
                                (val[[4]][textloc] - val[[2]][textloc]) /
                                    (val[[3]][textloc] - val[[1]][textloc])
                            rotangle <- atan(slopes * diff(rx) / diff(ry)) * 180 / base::pi
                        }
                        else if (label.style == "mixed") {
                            slopes <-
                                (val[[4]] - val[[2]]) /
                                    (val[[3]] - val[[1]])
                            rx <- range(ux)
                            ry <- range(uy)
                            depth <- pmin( (val[[1]] + val[[3]] - 2 * rx[1])/diff(rx),
                                          (2 * rx[2] - val[[1]] - val[[3]])/diff(rx),
                                          (val[[2]] + val[[4]] - 2 * ry[1])/diff(ry),
                                          (2 * ry[2] - val[[2]] - val[[4]])/diff(ry))

                            textloc <- which(abs(slopes) == min(abs(slopes), na.rm = TRUE))[1]
                            rotangle <- 0

                            if ( depth[textloc] < .05 ) {
                                textloc <- which(depth == max(depth))[1]
                                rotangle <- atan(slopes[textloc] * diff(rx) / diff(ry)) * 180 / base::pi
                            }
                        }
                        else stop("Invalid label.style")

                        lsegments(val[[1]], val[[2]],
                                  val[[3]], val[[4]],
                                  col = col, lty = lty, lwd = lwd)

                        ltext(lab = labels$lab[i], adj = c(.5, 0),
                              srt = rotangle,
                              col = col.text, cex = cex, font = font,
                              x = .5 * (val[[1]][textloc]+val[[3]][textloc]),
                              y = .5 * (val[[2]][textloc]+val[[4]][textloc]))

                    }
                }
            }
        }
    }
}









contourplot <-
    function(formula,
             data = parent.frame(),
             panel = "panel.levelplot",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 7,
             labels = TRUE,
             contour = TRUE,
             pretty = TRUE,
             region = FALSE,
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

    do.call("levelplot",
            c(list(formula = formula,
                   data = data,
                   groups = groups,
                   subset = subset,
                   panel = panel,
                   prepanel = prepanel,
                   strip = strip,
                   labels = labels,
                   cuts = cuts,
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
             contour = FALSE,
             cuts = 15,
             labels = FALSE,
             pretty = FALSE,
             region = TRUE,
             ...,
             colorkey = region,
             col.regions = trellis.par.get("regions")$col,
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
            if (is.matrix(formula)) {
                tmp <- expand.grid(1:nrow(formula), 1:ncol(formula))
                list(left = as.vector(formula),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     left.name = "",
                     right.x.name = "", right.y.name = "")
            }
            else if (is.data.frame(formula)) {
                tmp <- expand.grid(rownames(formula), colnames(formula))
                list(left = as.vector(as.matrix(formula)),
                     right.x = tmp[[1]],
                     right.y = tmp[[2]],
                     condition = NULL,
                     left.name = "",
                     right.x.name = "", right.y.name = "")
            }
            else stop("invalid formula")
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

    ## Step 2: Compute scales.common (excluding limits)

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

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, nlevels))

    id.na <- is.na(x)|is.na(y)  ##|is.na(z)
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
    ## doesn't work (in any meaningful way, at least) in such cases,
    ## but behaviour would be dissimilar in that case.

#     ux <- sort(unique(x[!is.na(x)]))
#     dux <- diff(ux)
#     wux <- .5 * (c(dux[1], dux) + c(dux, dux[length(dux)]))
#     ##wx <- wux[match(x[!is.na(x)], ux)]
#     wx <- wux[match(x, ux)]
#     uy <- sort(unique(y[!is.na(y)]))
#     duy <- diff(uy)

# print(uy)

#     wuy <- .5 * (c(duy[1], duy) + c(duy, duy[length(duy)]))
#     ##wy <- wuy[match(y[!is.na(y)], uy)]
#     wy <- wuy[match(y, uy)]

    zcol <- rep(NA, length(z)) #numeric(length(z))
    for (i in seq(along=col.regions))
        zcol[!id.na & !is.na(z) & z>=at[i] & z<at[i+1]] <- i

    foo$panel.args.common <-
        c(list(x=x, y=y, z=z, at=at, labels=labels,
               region = region, contour = contour,
               zcol=zcol, col.regions=col.regions),
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
                               nplots = nplots,
                               x.axs = foo$x.scales$axs,
                               y.axs = foo$y.scales$axs))

    class(foo) <- "trellis"
    foo
}








