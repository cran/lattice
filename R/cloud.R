




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




prepanel.default.cloud <-
    function(distance, xlim, ylim, zlim, zoom = 1,
             rot.mat = rot.mat, aspect = aspect, ...)
{
    aspect <- rep(aspect, length=2)

    corners <-
        rbind(x = c(-1,1,1,-1,-1,1,1,-1) / 2,
              y = c(-1,-1,-1,-1,1,1,1,1) / 2 * aspect[1],
              z = c(-1,-1,1,1,-1,-1,1,1) / 2 * aspect[2])

    ## box ranges and lengths
    cmin <- lapply(corners, min)
    cmax <- lapply(corners, max)
    clen <- lapply(corners, function(x) diff(range(x)))

    ##if (!missing(R.mat))
    ##  {;} #do something

    corners <- rot.mat %*% corners

    zback <- min(corners[3,])
    zfront <- max(corners[3,])
    za <- (zfront * (1-distance) - zback) / (zfront - zback)
    zb <- distance / (zfront - zback)

    corners[1,] <- (za + zb * corners[3,]) * corners[1,]
    corners[2,] <- (za + zb * corners[3,]) * corners[2,]

    xrng <- range(corners[1,])
    yrng <- range(corners[2,])
    slicelen <- max(diff(xrng), diff(yrng))

    list(xlim = extend.limits(xrng, length = slicelen) / zoom,
         ylim = extend.limits(yrng, length = slicelen) / zoom,
         dx = 1, dy = 1)
}











panel.cloud <-
    function(x, y, z, subscripts, distance, xlim, ylim, zlim,
             subpanel = "panel.xyplot",
             rot.mat = rot.mat, aspect = aspect,
             zcol, col.regions, par.box = NULL,
             ## next few arguments are an attempt to support
             ## scales. The main problem with scales is that it is
             ## difficult to figure out the best way to place the
             ## scales. Here, they would need to be specified
             ## explicitly. Maybe this code can be used later for a
             ## proper implementation
             xlab, ylab, zlab, scales.3d,
             proportion = 0.6, wireframe = FALSE,
             scpos = list(x = 1, y = 8, z = 12), 
             ...)
{
    if (any(subscripts)) {

        par.box.final <- trellis.par.get("box.3d")
        if (!is.null(par.box)) par.box.final[names(par.box)] <- par.box

        subpanel <-
            if (is.character(subpanel)) get(subpanel)
            else eval(subpanel)

        aspect <- rep(aspect, length=2)

        x <- x[subscripts]
        y <- y[subscripts]
        z <- z[subscripts]

        if (wireframe) {
            ord <- order(y, x)
            x <- x[ord]
            y <- y[ord]
            z <- z[ord]
            nx <- length(unique(x))
            ny <- length(unique(y))
            len <- length(z)
            if (nx * ny != len) stop("Incorrect arguments")
            zcol <- rep(zcol, len)

            idtf <- c(rep( c(rep(TRUE, nx-1), FALSE), ny-1 ), rep(FALSE, nx))
            id0 <- (1:(nx*ny))[idtf]
            id1 <- id0 + 1
            id2 <- id1 + nx
            id3 <- id0 + nx
        }


        corners <-
            data.frame(x = c(-1,1,1,-1,-1,1,1,-1) / 2,
                       y = c(-1,-1,-1,-1,1,1,1,1) / 2 * aspect[1],
                       z = c(-1,-1,1,1,-1,-1,1,1) / 2 * aspect[2])

        ## these are box boundaries
        ##pre <- c(1,2,3,4,2,3,4,1,5,6,7,8)
        ##nxt <- c(2,3,4,1,6,7,8,5,6,7,8,5)  old -- as backup

        pre <- c(1,2,4,1,2,3,4,1,5,6,8,5)
        nxt <- c(2,3,3,4,6,7,8,5,6,7,7,8)

        ## SCALES : very beta

        labs <- rbind(x = c(0, corners$x[pre[scpos$y]], corners$x[pre[scpos$z]]),
                      y = c(corners$y[pre[scpos$x]], 0, corners$y[pre[scpos$z]]),
                      z = c(corners$z[pre[scpos$x]], corners$z[pre[scpos$y]], 0))

        labs[,1] <- labs[,1] * (1 + scales.3d$x.scales$distance/3)
        labs[,2] <- labs[,2] * (1 + scales.3d$y.scales$distance/3)
        labs[,3] <- labs[,3] * (1 + scales.3d$z.scales$distance/3)

        axes <- rbind(x = 
                      c(proportion * corners$x[c(pre[scpos$x], nxt[scpos$x])],
                        corners$x[c(pre[scpos$y], nxt[scpos$y])],
                        corners$x[c(pre[scpos$z], nxt[scpos$z])]),
                      y = 
                      c(corners$y[c(pre[scpos$x], nxt[scpos$x])],
                        proportion * corners$y[c(pre[scpos$y], nxt[scpos$y])],
                        corners$y[c(pre[scpos$z], nxt[scpos$z])]),
                      z = 
                      c(corners$z[c(pre[scpos$x], nxt[scpos$x])],
                        corners$z[c(pre[scpos$y], nxt[scpos$y])],
                        proportion * corners$z[c(pre[scpos$z], nxt[scpos$z])]))
            
        axes[,1:2] <- axes[,1:2] * (1 + scales.3d$x.scales$distance/10)
        axes[,3:4] <- axes[,3:4] * (1 + scales.3d$y.scales$distance/10)
        axes[,5:6] <- axes[,5:6] * (1 + scales.3d$z.scales$distance/10)

        x.at <-
            if (is.logical(scales.3d$x.scales$at))
                lpretty(xlim, scales.3d$x.scales$tick.number)
            else scales.3d$x.scales$at
        y.at <- 
            if (is.logical(scales.3d$y.scales$at))
                lpretty(ylim, scales.3d$y.scales$tick.number)
            else scales.3d$y.scales$at
        z.at <- 
            if (is.logical(scales.3d$z.scales$at))
                lpretty(zlim, scales.3d$z.scales$tick.number)
            else scales.3d$z.scales$at
        x.at <- x.at[x.at >= xlim[1] & x.at <= xlim[2]]
        y.at <- y.at[y.at >= ylim[1] & y.at <= ylim[2]]
        z.at <- z.at[z.at >= zlim[1] & z.at <= zlim[2]]
        x.at.lab <-
            if (is.logical(scales.3d$x.scales$labels))
                as.character(x.at)
            else as.character(scales.3d$x.scales$labels)
        y.at.lab <-
            if (is.logical(scales.3d$y.scales$labels))
                as.character(y.at)
            else as.character(scales.3d$y.scales$labels)
        z.at.lab <-
            if (is.logical(scales.3d$z.scales$labels))
                as.character(z.at)
            else as.character(scales.3d$z.scales$labels)

        ## box ranges and lengths
        cmin <- lapply(corners, min)
        cmax <- lapply(corners, max)
        clen <- lapply(corners, function(x) diff(range(x)))

        ##if (!missing(R.mat))
        ##  {;} #do something

        ## transformed data
        tdata <- rbind(x = cmin$x + clen$x * (x-xlim[1])/diff(xlim),
                       y = cmin$y + clen$y * (y-ylim[1])/diff(ylim),
                       z = cmin$z + clen$z * (z-zlim[1])/diff(zlim))

        taxes <- rot.mat %*% axes
        x.at <- cmin$x + clen$x * (x.at-xlim[1])/diff(xlim)
        y.at <- cmin$y + clen$y * (y.at-ylim[1])/diff(ylim)
        z.at <- cmin$z + clen$z * (z.at-zlim[1])/diff(zlim)
        at.len <- length(x.at)
        x.at <- rbind(x = x.at,
                      y = rep(corners$y[pre[scpos$x]], at.len),
                      z = rep(corners$z[pre[scpos$x]], at.len))
        at.len <- length(y.at)
        y.at <- rbind(x = rep(corners$x[pre[scpos$y]], at.len),
                      y = y.at,
                      z = rep(corners$z[pre[scpos$y]], at.len))
        at.len <- length(z.at)
        z.at <- rbind(x = rep(corners$x[pre[scpos$z]], at.len),
                      y = rep(corners$y[pre[scpos$z]], at.len),
                      z = z.at)

        x.at.end <- x.at + scales.3d$x.scales$tck * .05 * labs[,1]
        y.at.end <- y.at + scales.3d$y.scales$tck * .05 * labs[,2]
        z.at.end <- z.at + scales.3d$z.scales$tck * .05 * labs[,3]

        x.labs <- x.at + 2 * scales.3d$x.scales$tck * .05 * labs[,1]
        y.labs <- y.at + 2 * scales.3d$y.scales$tck * .05 * labs[,2]
        z.labs <- z.at + 2 * scales.3d$z.scales$tck * .05 * labs[,3]

        x.at <- rot.mat %*% x.at
        x.labs <- rot.mat %*% x.labs
        x.at.end <- rot.mat %*% x.at.end
        y.at <- rot.mat %*% y.at
        y.labs <- rot.mat %*% y.labs
        y.at.end <- rot.mat %*% y.at.end
        z.at <- rot.mat %*% z.at
        z.labs <- rot.mat %*% z.labs
        z.at.end <- rot.mat %*% z.at.end
        
        tdata <- rot.mat %*% tdata
        corners <- rot.mat %*% t(as.matrix(corners))
        tlabs <- rot.mat %*% labs


        zback <- min(corners[3,])
        zfront <- max(corners[3,])
        za <- (zfront * (1-distance) - zback) / (zfront - zback)
        zb <- distance / (zfront - zback)

        tdata[1,] <- (za + zb * tdata[3,]) * tdata[1,]
        tdata[2,] <- (za + zb * tdata[3,]) * tdata[2,]

        corners[1,] <- (za + zb * corners[3,]) * corners[1,]
        corners[2,] <- (za + zb * corners[3,]) * corners[2,]

        taxes[1,] <- (za + zb * taxes[3,]) * taxes[1,]
        taxes[2,] <- (za + zb * taxes[3,]) * taxes[2,]
        
        x.at[1,] <- (za + zb * x.at[3,]) * x.at[1,]
        x.at[2,] <- (za + zb * x.at[3,]) * x.at[2,]
        x.labs[1,] <- (za + zb * x.labs[3,]) * x.labs[1,]
        x.labs[2,] <- (za + zb * x.labs[3,]) * x.labs[2,]
        x.at.end[1,] <- (za + zb * x.at.end[3,]) * x.at.end[1,]
        x.at.end[2,] <- (za + zb * x.at.end[3,]) * x.at.end[2,]

        y.at[1,] <- (za + zb * y.at[3,]) * y.at[1,]
        y.at[2,] <- (za + zb * y.at[3,]) * y.at[2,]
        y.labs[1,] <- (za + zb * y.labs[3,]) * y.labs[1,]
        y.labs[2,] <- (za + zb * y.labs[3,]) * y.labs[2,]
        y.at.end[1,] <- (za + zb * y.at.end[3,]) * y.at.end[1,]
        y.at.end[2,] <- (za + zb * y.at.end[3,]) * y.at.end[2,]

        z.at[1,] <- (za + zb * z.at[3,]) * z.at[1,]
        z.at[2,] <- (za + zb * z.at[3,]) * z.at[2,]
        z.labs[1,] <- (za + zb * z.labs[3,]) * z.labs[1,]
        z.labs[2,] <- (za + zb * z.labs[3,]) * z.labs[2,]
        z.at.end[1,] <- (za + zb * z.at.end[3,]) * z.at.end[1,]
        z.at.end[2,] <- (za + zb * z.at.end[3,]) * z.at.end[2,]

        
        tlabs[1,] <- (za + zb * tlabs[3,]) * tlabs[1,]
        tlabs[2,] <- (za + zb * tlabs[3,]) * tlabs[2,]


        farthest <- 1
        farval <- corners[3,1]

        for (i in 2:8)
            if (corners[3,i] < farval) {
                farthest <- i
                farval <- corners[3,i]
            }

        mark <- rep(TRUE, 12)
        for (j in 1:12)
            if (pre[j]==farthest || nxt[j]==farthest)
                mark[j] <- FALSE

        lsegments(corners[1, pre[!mark]],
                  corners[2, pre[!mark]],
                  corners[1, nxt[!mark]],
                  corners[2, nxt[!mark]],
                  col = par.box.final$col,
                  lwd = par.box.final$lwd,
                  lty = 2)

        
        if (wireframe) {
            ## This is where the wireframe is actually drawn
            if (TRUE) {
                xx <- tdata[1,]
                yy <- tdata[2,]
                zz <- tdata[3,]
                ord <- order(zz[id0])
                zcol <- zcol[id0][ord]
                px <- cbind(xx[id0][ord], xx[id1][ord], xx[id2][ord], xx[id3][ord])
                py <- cbind(yy[id0][ord], yy[id1][ord], yy[id2][ord], yy[id3][ord])
                for (i in seq(along = ord))
                    grid.polygon(x = px[i,], y = py[i,], default.units = "native",
                                 gp = gpar(fill = col.regions[zcol[i]], col = "black"))
            }
#             else {
#                 xx <- tdata[1,]
#                 yy <- tdata[2,]
#                 zz <- tdata[3,]
#                 ord <- order(zz[id0])

#                 dumx <- unit(c(0,.5,.5,0 ), "native")
#                 dumy <- unit(c(0,0,.5,.5), "native")

#                 grid.Call.graphics("D_quadrilateral",
#                                    xx[id0][ord],
#                                    xx[id1][ord],
#                                    xx[id2][ord],
#                                    xx[id3][ord],
#                                    yy[id0][ord],
#                                    yy[id1][ord],
#                                    yy[id2][ord],
#                                    yy[id3][ord],
#                                    dumx, dumy,
#                                    col.regions[zcol[id0][ord]],
#                                    PACKAGE="grid")
#             }
        }
        else subpanel(x=tdata[1,], y=tdata[2,], subscripts = subscripts, ...)
            
        lsegments(corners[1, pre[mark]],
                  corners[2, pre[mark]],
                  corners[1, nxt[mark]],
                  corners[2, nxt[mark]],
                  col = par.box.final$col,
                  lty = par.box.final$lty,
                  lwd = par.box.final$lwd)

        ## Next part for axes : beta
        
        if (scales.3d$x.scales$draw) {
            if (scales.3d$x.scales$arrows) {
                larrows(x0 = taxes[1, 1], y0 = taxes[2, 1],
                        x1 = taxes[1, 2], y1 = taxes[2, 2],
                        lty = scales.3d$x.scales$lty,
                        lwd = scales.3d$x.scales$lwd,
                        col = scales.3d$x.scales$col)
            }
            else {
                lsegments(x0 = x.at[1,], y0 = x.at[2,], x1 = x.at.end[1,], y1 = x.at.end[2,],
                          lty = scales.3d$x.scales$lty,
                          col = scales.3d$x.scales$col,
                          lwd = scales.3d$x.scales$lwd)
                ltext(x.at.lab, x = x.labs[1,], y = x.labs[2,],
                      cex = scales.3d$x.scales$cex,
                      font = scales.3d$x.scales$font,
                      col = scales.3d$x.scales$col)
            }
        }

        if (scales.3d$y.scales$draw) {
            if (scales.3d$y.scales$arrows) {
                larrows(x0 = taxes[1, 3], y0 = taxes[2, 3],
                        x1 = taxes[1, 4], y1 = taxes[2, 4],
                        lty = scales.3d$y.scales$lty,
                        lwd = scales.3d$y.scales$lwd,
                        col = scales.3d$y.scales$col)
            }
            else {
                lsegments(x0 = y.at[1,], y0 = y.at[2,], x1 = y.at.end[1,], y1 = y.at.end[2,],
                          lty = scales.3d$y.scales$lty,
                          col = scales.3d$y.scales$col,
                          lwd = scales.3d$y.scales$lwd)
                ltext(y.at.lab, x = y.labs[1,], y = y.labs[2,],
                      cex = scales.3d$y.scales$cex,
                      font = scales.3d$y.scales$font,
                      col = scales.3d$y.scales$col)
            }
        }
        if (scales.3d$z.scales$draw) {
            if (scales.3d$z.scales$arrows) {
                larrows(x0 = taxes[1, 5], y0 = taxes[2, 5],
                        x1 = taxes[1, 6], y1 = taxes[2, 6],
                        lty = scales.3d$z.scales$lty,
                        lwd = scales.3d$z.scales$lwd,
                        col = scales.3d$z.scales$col)
            }
            else {
                lsegments(x0 = z.at[1,], y0 = z.at[2,], x1 = z.at.end[1,], y1 = z.at.end[2,],
                          lty = scales.3d$z.scales$lty,
                          col = scales.3d$x.scales$col,
                          lwd = scales.3d$z.scales$lwd)
                ltext(z.at.lab, x = z.labs[1,], y = z.labs[2,],
                      cex = scales.3d$z.scales$cex,
                      font = scales.3d$z.scales$font,
                      col = scales.3d$z.scales$col)
            }
        }


        if (!is.null(xlab)) ltext(xlab$lab, x = tlabs[1, 1], y = tlabs[2, 1],
                                  cex = xlab$cex, rot = xlab$rot,
                                  font = xlab$font, col = xlab$col)
        
        if (!is.null(ylab)) ltext(ylab$lab, x = tlabs[1, 2], y = tlabs[2, 2],
                                  cex = ylab$cex, rot = ylab$rot,
                                  font = ylab$font, col = ylab$col)
                                  
        if (!is.null(zlab)) ltext(zlab$lab, x = tlabs[1, 3], y = tlabs[2, 3],
                                  cex = zlab$cex, rot = zlab$rot,
                                  font = zlab$font, col = zlab$col)
    }
}








panel.wireframe <- function(...)
    panel.cloud(..., wireframe = TRUE)





wireframe <-
    function(formula,
             data = parent.frame(),
             panel = "panel.wireframe",
             prepanel = NULL,
             strip = TRUE,
             groups = NULL,
             cuts = 70,
             pretty = FALSE,
             col.regions = trellis.par.get("regions")$col,
             drape = FALSE,
             colorkey = any(drape),
             ...,
             subset = TRUE)
{
    warning("wireframe can be EXTREMELY slow")
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

    do.call("cloud",
            c(list(formula = formula, data = data,
                   groups = groups, subset = subset,
                   panel = panel, prepanel = prepanel, strip = strip,
                   cuts = cuts, 
                   pretty = pretty,
                   col.regions = col.regions,
                   drape = drape,
                   colorkey = colorkey),
              dots))
}























cloud <-
    function(formula,
             data = parent.frame(),
             aspect = c(1,1),
             layout = NULL,
             panel = "panel.cloud",
             subpanel = "panel.xyplot",
             prepanel = NULL,
             scales = NULL,
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = range(x),
             ylab,
             ylim = range(y),
             zlab,
             zlim = range(z),
             distance = .2,
             par.box,
             perspective = TRUE,
             R.mat = diag(4),
             screen = list(z = 40, x = -60),
             zoom = .9,
             at,
             pretty = FALSE,
             drape = FALSE,
             colorkey = any(drape),
             col.regions, cuts = 1,
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
    if (missing(zlab)) zlab <- form$left.name

    if(!(is.numeric(x) && is.numeric(y) && is.numeric(z)))
        warning("x, y and z should be numeric")
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)

    zrng <- extend.limits(range(z[!is.na(z)]))
    if (missing(at))
        at <-
            if (pretty) lpretty(zrng, cuts)
            else seq(zrng[1], zrng[2], length = cuts+2)
    

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = 1,
                          strip = strip,
                          panel = panel,
                          xlab = NULL,
                          ylab = NULL), dots))
                          
    ##-----------------------------------------------------------
    ## xlab, ylab, zlab have special meaning in cloud / wireframe

    if (!is.null(xlab)) {
        text <- trellis.par.get("par.xlab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        xlab <- list(label = xlab[[1]], col = text$col, rot = 0,
                     cex = text$cex, font = text$font)
        if (is.list(xlab)) xlab[names(xlab)] <- xlab
    }
    if (!is.null(ylab)) {
        text <- trellis.par.get("par.ylab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        ylab <- list(label = ylab[[1]], col = text$col,  rot = 0,
                         cex = text$cex, font = text$font)
        if (is.list(ylab)) ylab[names(ylab)] <- ylab
    }
    if (!is.null(zlab)) {
        text <- trellis.par.get("par.zlab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        zlab <- list(label = zlab[[1]], col = text$col, rot = 0,
                         cex = text$cex, font = text$font)
        if (is.list(zlab)) zlab[names(zlab)] <- zlab
    }
    ##-----------------------------------------------------------------





    
    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()
    foo$fontsize.normal <- 10
    foo$fontsize.small <- 8

    ## This is for cases like xlab/ylab = list(cex=2)
    if (is.list(xlab) && !is.character(xlab$label))
        xlab$label <- form$right.x.name
    if (is.list(ylab) && !is.character(ylab$label))
        ylab$label <- form$right.y.name
    if (is.list(zlab) && !is.character(zlab$label))
        zlab$label <- form$left.name

    ## Step 2: Compute scales.common (leaving out limits for now)

    foo <- c(foo,
             do.call("construct.scales", list(draw=FALSE)))

    ## scales has to be interpreted differently. Nothing needs to be
    ## done for the ususal scales, but need a scales for panel.cloud
    ## Splus probably doesn't allow x-y-z-specific scales, but I see
    ## no reason not to allow that (will not allow limits, though)


    scales.default <-
        list(cex = .8, col = "black", lty = 1,
             lwd = 1, tck = 1, distance = c(1, 1, 1),
             arrows = TRUE)
    if (!is.null(scales)) scales.default[names(scales)] <- scales
    scales.3d <- do.call("construct.3d.scales", scales.default)

    ## Step 3: Decide if limits were specified in call:
    ## Here, always FALSE (in the 2d panel sense)
    have.xlim <- FALSE
    have.ylim <- FALSE

    ## Step 4: Decide if log scales are being used: !!!

    have.xlog <- !is.logical(scales.3d$x.scales$log) || scales.3d$x.scales$log
    have.ylog <- !is.logical(scales.3d$y.scales$log) || scales.3d$y.scales$log
    have.zlog <- !is.logical(scales.3d$z.scales$log) || scales.3d$z.scales$log
    if (have.xlog) {
        xlog <- scales.3d$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        ylog <- scales.3d$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        ylim <- log(ylim, ybase)
    }
    if (have.zlog) {
        zlog <- scales.3d$z.scales$log
        zbase <-
            if (is.logical(zlog)) 10
            else if (is.numeric(zlog)) zlog
            else if (zlog == "e") exp(1)

        z <- log(z, zbase)
        zlim <- log(zlim, zbase)
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

    ## FIXME: R.mat ignored
    ## calculate rotation matrix:
    rot.mat <- diag(3)
    screen.names <- names(screen)
    screen <- lapply(screen, "*", pi/180)

    for(i in seq(along=screen.names)) {
        th <- screen[[i]]
        cth <- cos(th)
        sth <- sin(th)
        tmp.mat <- 
            (if (screen.names[i]=="x")
             matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 3)
            else if (screen.names[i]=="y")
             matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 3)
            else if (screen.names[i]=="z")
             matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 3))
        rot.mat <- tmp.mat %*% rot.mat
    }


    if (drape) {
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
        zcol <- numeric(length(z))
        for (i in seq(along=col.regions))
            zcol[!id.na & z>=at[i] & z<at[i+1]] <- i
    }
    else {
        col.regions <- trellis.par.get("background")$col
        zcol <- 1
    }









    ## maybe *lim = NULL with relation = "free" ? 
    foo$panel.args.common <-
        c(list(x=x, y=y, z=z, rot.mat = rot.mat, zoom = zoom,
               subpanel = subpanel,
               xlim = xlim, ylim = ylim, zlim = zlim,
               xlab = xlab, ylab = ylab, zlab = zlab,
               aspect = aspect,
               distance = distance,
               scales.3d = scales.3d,
               zcol = zcol, col.regions = col.regions),
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
             limits.and.aspect(prepanel.default.cloud,
                               prepanel = prepanel,
                               have.xlim = have.xlim, xlim = xlim, 
                               have.ylim = have.ylim, ylim = ylim, 
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = 1,
                               nplots = nplots))

    class(foo) <- "trellis"
    foo
}








