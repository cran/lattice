




### Copyright 2001-2002  Deepayan Sarkar <deepayan@stat.wisc.edu>
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








## Plan: to make things more modular than they are now. As a first
## step, get a function that does a 3d transformation. Probably a good
## idea to do things in terms of homogeneous coordinates


ltransform3dMatrix <- function(screen, R.mat = diag(4)) {

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
    rot.mat <- cbind(rot.mat, c(0,0,0))
    rot.mat <- rbind(rot.mat, c(0,0,0,1))
    if (!missing(R.mat)) rot.mat <- rot.mat %*% R.mat
    rot.mat
}





ltransform3dto3d <- function(x, R.mat, za = 1 , zb = 0, zmin, zmax, dist) {
    tdata <- R.mat %*% rbind(x, 1)
    tdata[1,] <- tdata[1,]/tdata[4,]
    tdata[2,] <- tdata[2,]/tdata[4,]
    tdata[3,] <- tdata[3,]/tdata[4,]
    if (!missing(za) && !missing(zb)) {
        #print(zmin)
        #print(zmax)
        #print(dist)

        tdata[4,] <- tdata[3,] ## dummy, so that changes is tdata[3,]
        ## do not affect subsequent calculations of tdata[1:2,]

        if (dist != 0)
        {
            perp.dist.sq <- tdata[1,]^2 + tdata[2,]^2
            orig.v2z <- (zmax-zmin)/dist + zmin - tdata[3,]
            new.v2z <- sqrt(perp.dist.sq + orig.v2z^2)
            tdata[3,] <- tdata[3,] - new.v2z + orig.v2z
        }

        tdata[1,] <- (za + zb * tdata[4,]) * tdata[1,]
        tdata[2,] <- (za + zb * tdata[4,]) * tdata[2,]

    }
    tdata[1:3, ]
}







prepanel.default.cloud <-
    function(distance, xlim, ylim, zlim, zoom = 1,
             rot.mat = rot.mat, aspect = aspect, ...)
{
    aspect <- rep(aspect, length=2)
    corners <-
        rbind(x = c(-1,1,1,-1,-1,1,1,-1) / 2,
              y = c(-1,-1,-1,-1,1,1,1,1) / 2 * aspect[1],
              z = c(-1,-1,1,1,-1,-1,1,1) / 2 * aspect[2])
    corners <- ltransform3dto3d(corners, rot.mat)
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



            

panel.3dscatter.old <-
    function(x, y, z, rot.mat = diag(4), za, zb,  zback,
             zfront, distance, groups = NULL,
             subpanel = if (is.null(groups)) "panel.xyplot"
             else "panel.superpose",
             ...)
{
    subpanel <-
        if (is.character(subpanel)) get(subpanel)
        else eval(subpanel)
    m <- ltransform3dto3d(rbind(x, y, z), rot.mat, za, zb,  zback, zfront, distance)
    subpanel(x = m[1,], y = m[2,], groups = groups, ...)
}



panel.3dscatter <-
    function(x, y, z, rot.mat = diag(4), za, zb,
             zback, zfront, distance,
             zlim, zero,
             groups = NULL, subscripts = TRUE,
             type = 'p',
             col,
             ## eventually make all these cloud.3d$col etc
             col.point = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
             col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
             lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
             lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
             cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
             pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch,
             ...)
{
    ##cloud.3d <- list(col=1, cex=1, lty=1, lwd=1, pch=1)
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col)) {
        col.point <- col
        col.lines <- col
    }
    n <- length(x)
    if (n > 0)
    {
        if (is.null(groups))
        {
            col.point <- rep(col.point, length = n)
            col.line <- rep(col.line, length = n)
            lty <- rep(lty, length = n)
            lwd <- rep(lwd, length = n)
            cex <- rep(cex, length = n)
            pch <- rep(pch, length = n)
        }
        else {
            groups <- as.numeric(groups[subscripts])
            nvals <- length(unique(groups))
            col.point <- rep(col.point, length = nvals)[groups]
            col.line <- rep(col.line, length = nvals)[groups]
            lty <- rep(lty, length = nvals)[groups]
            lwd <- rep(lwd, length = nvals)[groups]
            cex <- rep(cex, length = nvals)[groups]
            pch <- rep(pch, length = nvals)[groups]
        }
        m <- ltransform3dto3d(rbind(x, y, z), rot.mat, za, zb, zback, zfront, distance)
        ord <- sort.list(m[3,])
        if (type == 'p')
            lpoints(x = m[1,ord], y = m[2,ord],
                    col = col.point[ord],
                    pch = pch[ord],
                    cex = cex[ord])
        ##cex = cex[ord] * (za + zb * m[3,ord])) - doesn't seem to work
        else if (type == 'h') {
            zero <-
                if (zero < zlim[1]) zlim[1]
                else if (zero > zlim[2]) zlim[2]
                else zero
            ##print(zero)
            other.end <- ltransform3dto3d(rbind(x, y, zero), rot.mat, za, zb, zback, zfront, distance)
            lsegments(m[1,ord], m[2,ord],
                      other.end[1,ord], other.end[2,ord],
                      col = col.line[ord],
                      lty = lty[ord],
                      lwd = lwd[ord])
        }
        else {
            warning(paste("type =", type, "not implemented, consider using 'panel.3d.cloud = panel.3dscatter.old'"))
        }
    }
}




####################################################################
##          Interface to New Experimental C code                  ##
####################################################################



palette.shade <- function(cosangle, height, saturation = .3, ...) {
    hsv(h = height,
        s = saturation,
        v = cosangle)
}



panel.3dwire <- 
    function(x, y, z, rot.mat = diag(4), za, zb,
             minz = 0, maxz = 1,
             col.at, col.regions,
             shade = FALSE,
             shade.colors = palette.shade,
             light.source = c(1, 0, 0),
             col = "black",
             col.groups = superpose.line$col,
             ...)
{
    
    ## x, y, z are in a special form here (compared to most other
    ## places in lattice). x and y are short ascending, describing the
    ## grid, and z is the corresponding z values in the order (x1,y1),
    ## (x1,y2), ... . length(z) == length(x) * length(y). Sometimes, z
    ## might be a matrix, which indicates multiple surfaces. Above
    ## description true for each column in that case.

    lenz <- maxz - minz
    ngroups <- if (is.matrix(z)) ncol(z) else 1
    superpose.line <- trellis.par.get("superpose.line")
    col.groups <- rep(col.groups, ngroups)
    light.source <- light.source/sqrt(sum(light.source * light.source))

    shade.colors <-
        if (is.character(shade.colors)) get(shade.colors)
        else eval(shade.colors)
    
    wirePolygon <-
        if (shade)
            function(xx, yy, misc) {
                ## xx, yy : coordinates of quadrilateral
                grid.polygon(x = xx, y = yy,
                             default.units = "native",
                             gp =
                             gpar(fill = 
                                  shade.colors(misc[1],
                                               (misc[2] - minz)/lenz), 
                                  col = "transparent"))
            }
        else if (length(col.regions) > 1)
            function(xx, yy, misc) {
                grid.polygon(x = xx, y = yy,
                             default.units = "native",
                             gp =
                             gpar(fill =
                                  col.regions[(seq(along = col.at)[col.at > misc[2]])[1] - 1 ],
                                  col = col))
            }
        else if (ngroups == 1)
            function(xx, yy, misc) {
                grid.polygon(x = xx, y = yy,
                             default.units = "native",
                             gp =
                             gpar(fill = col.regions[1],
                                  col = col))
            }
        else
            function(xx, yy, misc) {
                grid.polygon(x = xx, y = yy,
                             default.units = "native",
                             gp =
                             gpar(fill = col.groups[1 + as.integer(misc[3])],
                                  col = col))

            }


    #print(x)
    #print(y)
    #print(z)

    
    .Call("wireframePanelCalculations",
          as.double(x),
          as.double(y),
          as.double(z),
          as.double(rot.mat),
          as.double(za),
          as.double(zb),
          length(x),
          length(y),
          as.integer(ngroups),
          as.double(light.source),
          environment(),
          PACKAGE="lattice")
          
}
      





# panel.3dwire.old <- 
#     function(x, y, z, rot.mat = diag(4), za, zb, zcol,
#              ...)
# {

#     ## x, y, z are in a special form here (compared to most other
#     ## places in lattice). x and y are short ascending, describing the
#     ## grid, and z is the corresponding z values in the order (x1,y1),
#     ## (x1,y2), ... . length(z) == length(x) * length(y). Sometimes, z
#     ## might be a matrix, which indicates multiple surfaces. Above
#     ## description true for each column in that case.

#     grid <- rbind(t(as.matrix(expand.grid(yy = y, xx = x)))[2:1,], z)
#     grid <- ltransform3dto3d(grid, rot.mat, za, zb)

#     nx <- length(x)
#     ny <- length(y)

#     ordvec <- (1: ((nx -1 ) * ny)  )[- (1:(nx - 1)) * ny]
#     ordvec <- ordvec[order(pmax(grid[3, ordvec],
#                                 grid[3, ordvec + ny],
#                                 grid[3, ordvec + ny + 1],
#                                 grid[3, ordvec + 1] ))]
    
#     ##zcol <- zcol[id0][ord]

#     for (i in ordvec)
#         grid.polygon(x = grid[1, c(i, i + ny, i + ny + 1, i + 1)],
#                      y = grid[2, c(i, i + ny, i + ny + 1, i + 1)],
#                      default.units = "native",
#                      gp = gpar(fill = "white", col = "black"))
# }
      







panel.cloud <-
    function(x, y, z, subscripts,
             groups = NULL,
             distance, xlim, ylim, zlim,
             panel.3d.cloud = "panel.3dscatter",
             panel.3d.wireframe = "panel.3dwire",
             rot.mat, aspect,
             par.box = NULL,
             ## next few arguments are an attempt to support
             ## scales. The main problem with scales is that it is
             ## difficult to figure out the best way to place the
             ## scales. Here, they would need to be specified
             ## explicitly. Maybe this code can be used later for a
             ## proper implementation
             xlab, ylab, zlab, scales.3d,
             proportion = 0.6, wireframe = FALSE,
             scpos,
             ...,
             col.at,
             col.regions)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)

    if (any(subscripts)) { ## otherwise nothing to draw (not even box ?)

        par.box.final <- trellis.par.get("box.3d")
        if (!is.null(par.box)) par.box.final[names(par.box)] <- par.box

        aspect <- rep(aspect, length=2)

        x <- x[subscripts]
        y <- y[subscripts]
        z <- z[subscripts]
              
        corners <-
            data.frame(x = c(-1, 1, 1,-1,-1, 1, 1,-1) / 2,
                       y = c(-1,-1,-1,-1, 1, 1, 1, 1) / 2 * aspect[1],
                       z = c(-1,-1, 1, 1,-1,-1, 1, 1) / 2 * aspect[2])

        zlim.scaled <- range(corners$z) ## needed in panel.3dscatter for type = 'h'
        ## denotes z range of bounding box 
        
        ## center of bounding box:
        box.center <- matrix(unlist(lapply(corners, mean)), 3, 1)
        
        ## these are box boundaries:
        pre <- c(1,2,4,1,2,3,4,1,5,6,8,5)
        nxt <- c(2,3,3,4,6,7,8,5,6,7,7,8)

        ## The corners are defined in terms of coordinates in 3-D
        ## space as above. The actual choice of coordinates ideally
        ## should not affect anything, but I haven't checked. Box
        ## boundaries are defined as pairs of corners. The numbers of
        ## the corners and boundaries are helpful in keeping track of
        ## things, and are described in the diagram below.


        ## 1, 2, ..., 8 are the corners, L-1, ..., L-12 the boundaries        
        ##                          
        ##                                   L-11                  
        ##                           8------------------------7    
        ##                         / |                       / |    
        ##                        /  |                      /  |    
        ##                    L-7/   |L-12              L-6/   |    
        ##                      /    |                    /    |    
        ##                     /     |                   /     |    
        ##                    /      |        L-3       /      |L-10 
        ##                   4-------------------------3       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |                 |       |
        ##                   |       |    L-9          |       |
        ##                L-4|       5-----------------|-------6 
        ##                   |      /                  |      / 
        ##                   |     /                   |     /  
        ##                   |    /                 L-2|    /L-5
        ##                   |   /                     |   / 
        ##                   |  /L-8                   |  / 
        ##                   | /                       | /
        ##                   |/                        |/
        ##                   1-------------------------2
        ##                (0,0,0)          L-1           
        ##                                            
        ##
        ## Also the 6 FACES are defined in terms of corners (lines)
        ## as follows:
        ##
        ## F-1 : 1,2,3,4 (1,2,3,4)
        ## F-2 : 2,6,7,3 (5,10,6,2)
        ## F-3 : 6,5,8,7 (9,12,11,10)
        ## F-4 : 5,1,4,8 (8,4,7,12)
        ## F-5 : 1,2,6,5 (1,5,9,8)
        ## F-6 : 4,3,7,8 (3,6,11,7)

        face.corners <- list(c(1,2,3,4),
                             c(2,6,7,3),
                             c(6,5,8,7),
                             c(5,1,4,8),
                             c(1,2,6,5),
                             c(4,3,7,8))

        face.lines <- list(c(1,2,3,4),
                           c(5,10,6,2),
                           c(9,12,11,10),
                           c(8,4,7,12),
                           c(1,5,9,8),
                           c(3,6,11,7))
        
        ## SCALES : very beta

        tmp <- ltransform3dto3d(t(as.matrix(corners)), rot.mat)
        farthest <- 1  ## used later also
        farval <- tmp[3,1]

        for (i in 2:8)
            if (tmp[3,i] < farval) {
                farthest <- i
                farval <- tmp[3,i]
            }

        ## not foolproof, need to revisit this later
        scale.position <-
            if (farthest == 1) list(x = 3, y = 7, z = 2)
            else if (farthest == 2) list(x = 9, y = 8, z = 10)
            else if (farthest == 3) list(x = 11, y = 7, z = 10)
            else if (farthest == 4) list(x = 11, y = 6, z = 2)
            else if (farthest == 5) list(x = 1, y = 5, z = 4)
            else if (farthest == 6) list(x = 1, y = 8, z = 12)
            else if (farthest == 7) list(x = 3, y = 7, z = 2)
            else if (farthest == 8) list(x = 3, y = 6, z = 10)

        ##original:
            #if (farthest == 1) list(x = 9, y = 5, z = 2)
            #else if (farthest == 2) list(x = 9, y = 8, z = 10)
            #else if (farthest == 3) list(x = 11, y = 7, z = 10)
            #else if (farthest == 4) list(x = 11, y = 6, z = 2)
            #else if (farthest == 5) list(x = 1, y = 5, z = 4)
            #else if (farthest == 6) list(x = 1, y = 8, z = 12)
            #else if (farthest == 7) list(x = 3, y = 7, z = 2)
            #else if (farthest == 8) list(x = 3, y = 6, z = 10)
        if (!missing(scpos))
            scale.position[names(scpos)] <- scpos

        scpos <- scale.position

        
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



        ## FIXME: The following should be split into blocks of
        ## is.characterOrExpression(xlim)...

        x.at <-
            if (is.logical(scales.3d$x.scales$at)) 
                if (is.characterOrExpression(xlim)) seq(along = xlim)
                else lpretty(xlim, scales.3d$x.scales$tick.number)
            else scales.3d$x.scales$at
        y.at <- 
            if (is.logical(scales.3d$y.scales$at)) 
                if (is.characterOrExpression(ylim)) seq(along = ylim)
                else lpretty(ylim, scales.3d$y.scales$tick.number)
            else scales.3d$y.scales$at
        z.at <- 
            if (is.logical(scales.3d$z.scales$at)) 
                if (is.characterOrExpression(zlim)) seq(along = zlim)
                else lpretty(zlim, scales.3d$z.scales$tick.number)
            else scales.3d$z.scales$at
        x.at.lab <-
            if (is.logical(scales.3d$x.scales$labels))
                if (is.characterOrExpression(xlim)) xlim
                else as.character(x.at)
            else as.character(scales.3d$x.scales$labels)
        y.at.lab <-
            if (is.logical(scales.3d$y.scales$labels))
                if (is.characterOrExpression(ylim)) ylim
                else as.character(y.at)
            else as.character(scales.3d$y.scales$labels)
        z.at.lab <-
            if (is.logical(scales.3d$z.scales$labels))
                if (is.characterOrExpression(zlim)) zlim
                else as.character(z.at)
            else as.character(scales.3d$z.scales$labels)
        if (is.characterOrExpression(xlim)) {
            xlim <- c(0, length(xlim) + 1)
        }
        if (is.characterOrExpression(ylim)) {
            ylim <- c(0, length(ylim) + 1)
        }
        if (is.characterOrExpression(zlim)) {
            zlim <- c(0, length(zlim) + 1)
        }
        x.at <- x.at[x.at >= xlim[1] & x.at <= xlim[2]]
        y.at <- y.at[y.at >= ylim[1] & y.at <= ylim[2]]
        z.at <- z.at[z.at >= zlim[1] & z.at <= zlim[2]]

        x.at.lab <- x.at.lab[x.at >= xlim[1] & x.at <= xlim[2]]
        y.at.lab <- y.at.lab[y.at >= ylim[1] & y.at <= ylim[2]]
        z.at.lab <- z.at.lab[z.at >= zlim[1] & z.at <= zlim[2]]

        

        ## box ranges and lengths
        cmin <- lapply(corners, min)
        cmax <- lapply(corners, max)
        clen <- lapply(corners, function(x) diff(range(x)))


        ## scaled (to bounding box) data
        x <- cmin$x + clen$x * (x-xlim[1])/diff(xlim)
        y <- cmin$y + clen$y * (y-ylim[1])/diff(ylim)
        z <- cmin$z + clen$z * (z-zlim[1])/diff(zlim)
        col.at <- cmin$z + clen$z * (col.at - zlim[1])/diff(zlim)

        zero.scaled <- cmin$z - clen$z * zlim[1]/diff(zlim)
        ## needed in panel.3dscatter for type = 'h'


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

        ## Things necessary for perspective
        tmp <- ltransform3dto3d(t(as.matrix(corners)), rot.mat)
        zback <- min(tmp[3,])
        zfront <- max(tmp[3,])
        za <- (zfront * (1-distance) - zback) / (zfront - zback)
        zb <- distance / (zfront - zback)

        corners <- ltransform3dto3d(t(as.matrix(corners)), rot.mat, za, zb, zback, zfront, distance)

        taxes <- ltransform3dto3d(axes, rot.mat, za, zb, zback, zfront, distance)
        x.at <- ltransform3dto3d(x.at, rot.mat, za, zb, zback, zfront, distance)
        x.labs <- ltransform3dto3d(x.labs, rot.mat, za, zb, zback, zfront, distance)
        x.at.end <- ltransform3dto3d(x.at.end, rot.mat, za, zb, zback, zfront, distance)

        y.at <- ltransform3dto3d(y.at, rot.mat, za, zb, zback, zfront, distance)
        y.labs <- ltransform3dto3d(y.labs, rot.mat, za, zb, zback, zfront, distance)
        y.at.end <- ltransform3dto3d(y.at.end, rot.mat, za, zb, zback, zfront, distance)

        z.at <- ltransform3dto3d(z.at, rot.mat, za, zb, zback, zfront, distance)
        z.labs <- ltransform3dto3d(z.labs, rot.mat, za, zb, zback, zfront, distance)
        z.at.end <- ltransform3dto3d(z.at.end, rot.mat, za, zb, zback, zfront, distance)

        tlabs <- ltransform3dto3d(labs, rot.mat, za, zb, zback, zfront, distance)

        box.center <- ltransform3dto3d(box.center, rot.mat, za, zb, zback, zfront, distance)

        ## Shall now determine which bounding lines should be 'hidden'
        ## (by the data, and hence need to be drawn before the data),
        ## and which should be 'visible'. Will actually consider each
        ## face (one at a time), determine if it is 'visible' (had the
        ## bounding cube been opaque), and if so, mark the lines
        ## forming that face as 'visible'

        ## The logical vector 'mark' will correspond to the 12 lines
        ## (indexing explained in the diagram above). mark = TRUE will
        ## mean that the line will be drawn AFTER the data is
        ## drawn. Start off with all fark = FALSE.

        ## The idea is that for visible faces, the z-value of the
        ## center of the face will be greater than the z-value of the
        ## center of the whole box

        ##print(box.center)
        mark <- rep(FALSE, 12)
        box.center.z <- box.center[3]

        for (face in 1:6)
            if (mean(corners[3, face.corners[[face]] ]) > box.center.z) ## i.e., face visible
                mark[1:12 %in% face.lines[[face]] ] <- TRUE

        #for (j in 1:12)
        #    if (pre[j]==farthest || nxt[j]==farthest)
        #        mark[j] <- FALSE

        ## This draws the 'back' of the box, i.e., the portion that
        ## should be hidden by the data. This doesn't work properly in
        ## the case where the whole 'back rectangle' is 'contained'
        ## within the 'front rectangle'.

        lsegments(corners[1, pre[!mark]],
                  corners[2, pre[!mark]],
                  corners[1, nxt[!mark]],
                  corners[2, nxt[!mark]],
                  col = par.box.final$col,
                  lwd = par.box.final$lwd,
                  lty = 2)


        ## The following portion of code is responsible for drawing
        ## the part of the plot driven by the data. The modus operandi
        ## will be different for cloud and wireframe, since they have
        ## essentially different purpose. For cloud, the data is
        ## unstructured, and x, y and z are all passed to the
        ## panel.3d.cloud function. For wireframe, on the other hand,
        ## x and y must form a regular grid, which sort(unique(<x|y>))
        ## is enough to describe (o.w., very real memory problems
        ## possible). z would then have to be supplied in a very
        ## particular order. All this is fine, but a problem arises if
        ## we want to allow groups -- multiple surfaces. One option is
        ## to supply a matrix (nx * ny by no.of.groups) for z. This is
        ## OK, but it precludes the posibility of supplying x and y as
        ## only their unique values from the very beginning. Let's do
        ## it this way for now.


        if (wireframe) {
            panel.3d.wireframe <- 
                if (is.character(panel.3d.wireframe)) get(panel.3d.wireframe)
                else eval(panel.3d.wireframe)

            if (is.null(groups)) {
                ord <- order(x, y)
                tmp <- z[ord]

                nx <- length(unique(x))
                ny <- length(unique(y))
                len <- length(z)
                if (nx * ny != len) stop("Incorrect arguments")
            }
            else {
                vals <- sort(unique(groups))
                nvals <- length(vals)
                tmp <- numeric(0)

                for (i in seq(along=vals)) {
                    id <- (groups[subscripts] == vals[i])
                    if (any(id)) {
                        ord <- order(x[id], y[id])
                        tmp <- cbind(tmp, z[id][ord])
                    }
                }

            }
            x <- sort(unique(x))
            y <- sort(unique(y))
            z <- NULL ## hopefully becomes garbage, collected if necessary


            panel.3d.wireframe(x = x, y = y, z = tmp,
                               rot.mat = rot.mat,
                               za = za, zb = zb,
                               minz = cmin$z,
                               maxz = cmax$z,
                               col.at = col.at,
                               col.regions = col.regions,
                               ...)
        }
        else {
            panel.3d.cloud <- 
                if (is.character(panel.3d.cloud)) get(panel.3d.cloud)
                else eval(panel.3d.cloud)
            panel.3d.cloud(x = x, y = y, z = z,
                           rot.mat = rot.mat,
                           za=za, zb=zb, zback, zfront, distance,
                           zlim = zlim.scaled,
                           zero = zero.scaled,
                           subscripts = subscripts,
                           groups = groups,
                           ...)
        }





        ## This draws the front of the bounding box

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
             drape = FALSE,
             ...,
             col.regions = trellis.par.get("regions")$col,
             colorkey = any(drape),
             subset = TRUE)
{
    ##warning("wireframe can be EXTREMELY slow")
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
             allow.multiple = FALSE,
             outer = FALSE,
             auto.key = FALSE,
             aspect = c(1,1),
             layout = NULL,
             panel = "panel.cloud",
             prepanel = NULL,
             scales = NULL,
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim = if (is.factor(x)) levels(x) else range(x),
             ylab,
             ylim = if (is.factor(y)) levels(y) else range(y),
             zlab,
             zlim = if (is.factor(z)) levels(z) else range(z),
             distance = .2,
             perspective = TRUE,
             R.mat = diag(4),
             screen = list(z = 40, x = -60),
             zoom = .8,
             at,
             pretty = FALSE,
             drape = FALSE,
             ...,
             colorkey = any(drape),
             col.regions, cuts = 1,
             subscripts = TRUE,
             subset = TRUE)
{

    ##dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    formula <- eval(substitute(formula), data, parent.frame())
    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data, dim = 3, subset = subset,
                                groups = groups, multiple = allow.multiple,
                                outer = outer, subscripts = TRUE)
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
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }
    
    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name
    if (missing(zlab)) zlab <- form$left.name

    ##if(!(is.numeric(x) && is.numeric(y) && is.numeric(z)))
    ##    warning("x, y and z should be numeric")
    ##x <- as.numeric(x)
    ##y <- as.numeric(y)
    ##z <- as.numeric(z)

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
        xlab <-
            list(label =
                 if (is.characterOrExpression(xlab)) xlab
                 else xlab[[1]],
                 col = text$col, rot = 0,
                 cex = text$cex, font = text$font)
        if (is.list(xlab)) xlab[names(xlab)] <- xlab
    }
    if (!is.null(ylab)) {
        text <- trellis.par.get("par.ylab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        ylab <-
            list(label =
                 if (is.characterOrExpression(ylab)) ylab
                 else ylab[[1]],
                 col = text$col,  rot = 0,
                 cex = text$cex, font = text$font)
        if (is.list(ylab)) ylab[names(ylab)] <- ylab
    }
    if (!is.null(zlab)) {
        text <- trellis.par.get("par.zlab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        zlab <-
            list(label =
                 if (is.characterOrExpression(zlab)) zlab
                 else zlab[[1]],
                 col = text$col, rot = 0,
                 cex = text$cex, font = text$font)
        if (is.list(zlab)) zlab[names(zlab)] <- zlab
    }
    ##-----------------------------------------------------------------


    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()

    ## This is for cases like xlab/ylab = list(cex=2)
    if (is.list(xlab) && !is.characterOrExpression(xlab$label))
        xlab$label <- form$right.x.name
    if (is.list(ylab) && !is.characterOrExpression(ylab$label))
        ylab$label <- form$right.y.name
    if (is.list(zlab) && !is.characterOrExpression(zlab$label))
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

    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)|is.na(y)|is.na(z)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args





    ## calculate rotation matrix:


#     rot.mat <- diag(3)
#     screen.names <- names(screen)
#     screen <- lapply(screen, "*", pi/180)

#     for(i in seq(along=screen.names)) {
#         th <- screen[[i]]
#         cth <- cos(th)
#         sth <- sin(th)
#         tmp.mat <- 
#             (if (screen.names[i]=="x")
#              matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 3)
#             else if (screen.names[i]=="y")
#              matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 3)
#             else if (screen.names[i]=="z")
#              matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 3))
#         rot.mat <- tmp.mat %*% rot.mat
#     }


    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)

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
    }
    else {
        col.regions <- trellis.par.get("background")$col
    }


    ## maybe *lim = NULL with relation = "free" ? 
    foo$panel.args.common <-
        c(list(x=x, y=y, z=z, rot.mat = rot.mat, zoom = zoom,
               xlim = xlim, ylim = ylim, zlim = zlim,
               xlab = xlab, ylab = ylab, zlab = zlab,
               aspect = aspect,
               distance = if (perspective) distance else 0,
               scales.3d = scales.3d,
               col.at = at, col.regions = col.regions),
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

    if (is.null(foo$key) && !is.null(groups) &&
        (is.list(auto.key) || (is.logical(auto.key) && auto.key)))
        foo$key <- do.call("simpleKey",
                           c(list(levels(as.factor(groups))),
                             if (is.list(auto.key)) auto.key else list()))

    class(foo) <- "trellis"
    foo
}








