




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
             rot.mat = rot.mat, aspect = aspect, ...)
{
    if (any(subscripts)) {

        subpanel <-
            if (is.character(subpanel)) get(subpanel)
            else eval(subpanel)

        aspect <- rep(aspect, length=2)

        x <- x[subscripts]
        y <- y[subscripts]
        z <- z[subscripts]

        corners <-
            data.frame(x = c(-1,1,1,-1,-1,1,1,-1) / 2,
                       y = c(-1,-1,-1,-1,1,1,1,1) / 2 * aspect[1],
                       z = c(-1,-1,1,1,-1,-1,1,1) / 2 * aspect[2])

        ## these are box boundaries
        pre <- c(1,2,3,4,2,3,4,1,5,6,7,8)
        nxt <- c(2,3,4,1,6,7,8,5,6,7,8,5)

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

        tdata <- rot.mat %*% tdata
        corners <- rot.mat %*% t(as.matrix(corners))

        zback <- min(corners[3,])
        zfront <- max(corners[3,])
        za <- (zfront * (1-distance) - zback) / (zfront - zback)
        zb <- distance / (zfront - zback)

        tdata[1,] <- (za + zb * tdata[3,]) * tdata[1,]
        tdata[2,] <- (za + zb * tdata[3,]) * tdata[2,]

        corners[1,] <- (za + zb * corners[3,]) * corners[1,]
        corners[2,] <- (za + zb * corners[3,]) * corners[2,]

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

        lsegments(corners[1,pre[!mark]],
                  corners[2,pre[!mark]],
                  corners[1,nxt[!mark]],
                  corners[2,nxt[!mark]], lty = 2)

        subpanel(x=tdata[1,], y=tdata[2,], subscripts = subscripts, ...)

        lsegments(corners[1,pre[mark]],
                  corners[2,pre[mark]],
                  corners[1,nxt[mark]],
                  corners[2,nxt[mark]])
    }
}








cloud <-
    function(formula,
             data = parent.frame(),
             aspect = c(1,1),
             layout = NULL,
             panel = "panel.cloud",
             subpanel = "panel.xyplot",
             prepanel = NULL,
             scales = list(cex=.5, lty = 1, lwd = 1, col = "black",
               distance = rep(1,3), arrows = TRUE),
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
             zoom = 1,
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

    form <- latticeParseFormula(formula, data, dim = 3)

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
                   c(list(aspect = 1,
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

    foo <- c(foo, 
             do.call("construct.scales", list(draw=FALSE)))


    ## Step 3: Decide if limits were specified in call:
    ## Here, always FALSE (in the 2d panel sense)
    have.xlim <- FALSE
    have.ylim <- FALSE

    ## Step 4: Decide if log scales are being used:  !!!

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

    ## maybe *lim = NULL with relation = "free" ? 
    foo$panel.args.common <-
        c(list(x=x, y=y, z=z, rot.mat = rot.mat, zoom = zoom,
               subpanel = subpanel,
               xlim = xlim, ylim = ylim, zlim = zlim,  
               aspect = aspect, distance = distance),
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








