


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





## Need global variable to handle more in print.trellis
.lattice.print.more <- FALSE

## starting with these settings:
## trellis.settings must be available as a global variable

trellis.settings <-   # color settings
    list(background=list(col="#889088"),
         add.line=list(col="black", lty=1, lwd=1),
         add.text=list(cex=1, col="black", font=1),
         bar.fill=list(col="#00fcf8"),
         box.dot=list(col="black", cex=1, font=1, pch=16),
         box.rectangle=list(col="#00fcf8", lty=1, lwd=1),
         box.umbrella=list(col="#00fcf8", lty=2, lwd=1),
         dot.line=list(col="#a8a8a8", lty=1, lwd=2),
         dot.symbol=list(cex=0.8, col="#00fcf8", font=1, pch=16),
         plot.line=list(col="#00fcf8", lty=1, lwd=1),
         plot.symbol=list(cex=0.8, col="#00fcf8", font=1, pch=1),
         reference.line=list(col="#a8a8a8", lty=1, lwd=2),   
         
         strip.background=list(col=c("#f8d088","#c8fcc8","#c0fcf8",
                               "#a8e0f8","#f8c0f8","#f88c88","#f8fcc0")),
         strip.shingle=list(col=c("#f87c00", "#00fc00", "#00fcf8",
                            "#007cf8","#f800f8","#f80000","#f8fc00")),
         
         
         superpose.line=list(col=c("#00fcf8","#f800f8","#00fc00",
                             "#f87c00","#007cf8","#f8fc00","#f80000"),
         lty=c(1,1,1,1,1,1,1),lwd=c(1,1,1,1,1,1,1)),
         regions = list(col = rev(cm.colors(100))),
         superpose.symbol=list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
         col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8",
         "#f8fc00","#f80000"),
         font=c(5,5,5,5,5,5,5),
         pch=c(1,1,1,1,1,1,1)),
         axis.line=list(line=0, col="black", lty=1, lwd=1))


lpretty <- function(x, ...) { 
    eps <- 1e-10
    ##{    print(x); pretty(x, ...) }
    ##    .Call("L_pretty", range(x))
    at <- pretty(x, ...)
    at <- ifelse(abs(at)<eps, 0, at)
}

trellis.par.get <-
    function(name)
{
    if (name %in% names(trellis.settings))
        trellis.settings[[name]]
}


trellis.par.set <-
    function(name, value)
{
    if (name %in% names(trellis.settings))
        trellis.settings[[name]] <<- value
}


trellis.device <-
    function(device = getOption("device"),
             color=TRUE,
             # Paul 30/05/01
             # Only use grey background for colour x11 device
             # NOTE that this check may need modifying as more
             # devices are added beyond just "x11" and "postscript"
             bg =
             if (color && (
                           if (is.character(device)) device != "postscript"
                           else deparse(substitute(device)) != "postscript"
                           )) "#889088"
             else "#f8fcf8",
             new = TRUE, ...)
{

    # Paul 30/05/01
    # Changed "device" to "device.call" so that the device name and
    # the device function cannot be confused
    if (is.character(device)) {
        device.call <- get(device)
    }
    else device.call <- device
    
    if (new)
        device.call(...)

    ## if (exists(".grid.started") && !.grid.started) grid.start()
    
    ##    par(bg = bg) # drop this when ...

    if (color)
    {
        # color settings
        trellis.par.set("background", list(col=bg))
        trellis.par.set("add.line", list(col="black", lty=1, lwd=1))
        trellis.par.set("add.text", list(cex=1, col="black", font=1))
        trellis.par.set("bar.fill", list(col="#00fcf8"))
        trellis.par.set("box.dot", list(col="black", cex=1, font=1, pch=16))
        trellis.par.set("box.rectangle", list(col="#00fcf8", lty=1, lwd=1))
        trellis.par.set("box.umbrella", list(col="#00fcf8", lty=2, lwd=1))
        trellis.par.set("dot.line", list(col="#a8a8a8", lty=1, lwd=2))
        trellis.par.set("dot.symbol", list(cex=0.8, col="#00fcf8", font=1, pch=16))
        trellis.par.set("plot.line", list(col="#00fcf8", lty=1, lwd=1))
        trellis.par.set("plot.symbol", list(cex=0.8, col="#00fcf8", font=1, pch=1))
        trellis.par.set("regions", list(col = rev(cm.colors(100))))
        trellis.par.set("reference.line", list(col="#a8a8a8", lty=1, lwd=2))   
        trellis.par.set("strip.background", list(col=c("#f8d088","#c8fcc8","#c0fcf8",
                                                "#a8e0f8","#f8c0f8","#f88c88","#f8fcc0")))
        trellis.par.set("strip.shingle", list(col=c("#f87c00", "#00fc00", "#00fcf8",
                                             "#007cf8","#f800f8","#f80000","#f8fc00")))
        trellis.par.set("superpose.line", list(col=c("#00fcf8","#f800f8","#00fc00",
                                               "#f87c00","#007cf8","#f8fc00","#f80000"),
                                               lty=c(1,1,1,1,1,1,1), lwd=c(1,1,1,1,1,1,1)))
        trellis.par.set("superpose.symbol", list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
                                                 col=c("#00fcf8","#f800f8","#00fc00","#f87c00",
                                                 "#007cf8","#f8fc00","#f80000"),
                                                 font=c(5,5,5,5,5,5,5),
                                                 pch = c(1,1,1,1,1,1,1)))
        trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
    }
    else
    {
        
        ## black and white settings
        trellis.par.set("background", list(col=bg))
        trellis.par.set("add.line", list(col="black", lty=1, lwd=1))
        trellis.par.set("add.text", list(cex=1, col="black", font=1))
        trellis.par.set("bar.fill", list(col="#e8f0e8"))
        trellis.par.set("box.dot", list(col="black", cex=1, font=1, pch=16))
        trellis.par.set("box.rectangle", list(col="black", lty=1, lwd=1))
        trellis.par.set("box.umbrella", list(col="black", lty=1, lwd=1))
        trellis.par.set("dot.line", list(col="#e4e0e4", lty=1, lwd=2))
        trellis.par.set("dot.symbol", list(cex=0.8, col="black", font=1, pch=16))
        trellis.par.set("plot.line", list(col="black", lty=1, lwd=1))
        trellis.par.set("plot.symbol", list(cex=0.8, col="black", font=1, pch=1))
        trellis.par.set("regions", list(col = gray(0:99/99)))
        trellis.par.set("reference.line", list(col="#e4e0e4", lty=1, lwd=2))
        trellis.par.set("strip.background", list(col="#e8f0e8"))
        trellis.par.set("strip.shingle", list(col="#a8b0a8"))
        trellis.par.set("superpose.line", list(col=rep("black", 7),
                                               lty=c(1,2,3,4,5,6,7),
                                               lwd=c(1,1,1,1,1,1,1)))
        trellis.par.set("superpose.symbol", list(cex=c(0.85,0.85,0.85,0.85,0.85,0.85,0.85),
                                                 col=rep("black", 7),
                                                 font=c(1,1,1,1,1,1,1),
                                                 pch=c("\001","+",">","s","w","#","{")))
        trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
        
    }

    .grid.started <<- FALSE
}




oneway <-
    function(formula, data, location = mean,
             spread = function(x) sqrt(var(x)))
{

    if(missing(data)) data <- sys.frame(sys.parent())
    form <- latticeParseFormula(formula, data)
    y <- form$left
    x <- form$right

    if (!is.shingle(x)) x <- as.factor(x)

    is.f.x <- is.factor(x)

    num.l.x <- 
        if (is.f.x) length(levels(x)) 
        else nrows(x$int)

    foo <- list()
    
    if (is.f.x) {
        foo$location <-
            if (is.function(location)) as.vector(tapply(X=y, INDEX=list(x), FUN = location))
            else rep(location, num.l.x)

        foo$spread <- 
            if (is.function(spread)) as.vector(tapply(X=y, INDEX=list(x), FUN = spread))
            else rep(spread, num.l.x)

        foo$fitted.values <- numeric(length(y))
        sc <- numeric(length(y))

        for (i in seq(along = y)){
            foo$fitted.values[i] <- foo$location[as.numeric(x)[i]]
            sc[i] <- foo$spread[as.numeric(x)[i]]
        }
        foo$residuals <- y - foo$fitted.values
        foo$scaled.residuals <- foo$residuals/sc
    }
    else stop("x must be (coercible to be) a factor")

    foo
    
}






do.breaks  <- function(endpoints, nint)
{
    if (length(endpoints)!=2) stop("error")
    endpoints[1] + diff(endpoints) * 0:nint / nint
}




levels <- function(x)
    UseMethod("levels")

levels.default <- function(x)
    attr(x, "levels")

levels.shingle <- function(x)
    x$intervals

numlevels <- function(x)
{
    if (is.shingle(x)) nrow(levels(x))
    else nlevels(x)
}


is.na.shingle <- function(x)
    is.na(x$x)

## This converts character to factor, numeric to shingle, and
## in addition, takes subsets
as.factorOrShingle <- function(x, subset = TRUE, drop = FALSE)
{
    if (is.character(x)) as.factor(x)[subset, drop = drop]
    else if (is.numeric(x)) as.shingle(x)[subset, drop = drop]
    else x[subset, drop = drop]
}


## defunt:
ldrop.levels <- function(x, subset)
    x[subset, drop = TRUE]


"[.shingle" <-
    function(x, subset, drop = FALSE)
{
    x$x <- x$x[subset]
    if (drop) {
        dl <- logical(nrow(x$int))
        for (i in seq(along=dl))
            dl[i] <- any( x$x >= x$int[i,1] & x$x <= x$int[i,2]  )
        x$intervals <- x$intervals[dl,]
    }
    x
}






Rows <- function(x, which)
{
    for (i in seq(along = x)) x[[i]] <- x[[i]][which]
    x
}
## S-Plus trellis function needed for nlme.







equal.count <-
  function(x, ...)
{
    ans <- list(x=x,
                intervals=co.intervals(x,...))
    class(ans) <- "shingle"
    ans
}



shingle <-
    function(x, intervals=sort(unique(x)))
{

    if (ncol(as.matrix(intervals))==1)
        intervals <- cbind(intervals, intervals)

    ans <- list(x=x,
                intervals=intervals)
    class(ans) <- "shingle"
    ans
}



levels.shingle <-
    function(x)
{
    if (!is.shingle(x)) stop("x must be shingle")
    x$intervals
}


is.shingle <-
  function(x) inherits(x, "shingle")






as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)




strip.default <-
    function(x, level, style = 1,#if(is.factor(x)) 1 else 3,
             strip.names = is.matrix(x), name="",
             bg, fg, par.strip.text = trellis.par.get("add.text"))
{
    if(is.matrix(x)) {
        grid.rect(gp = gpar(fill=bg))
        t <- range(x)
        r <- (range(x[level,])-t[1])/diff(t)
        grid.rect(x = unit(r%*%c(.5,.5),"npc"), width = unit(diff(r),"npc"),
                  gp = gpar(col=fg, fill=fg))
        if(strip.names) grid.text(label = name,
                                  gp = gpar(col = par.strip.text$col,
                                  fontsize = par.strip.text$cex *
                                  current.viewport()$gp$fontsize))
        
        grid.rect()
    }
    else {
        num <- length(levels(x))
        if (style == 1) {
            grid.rect(gp = gpar(fill=bg))
            grid.text(label = paste(if(strip.names)
                      paste(name,":") else "", x[level]),
                      gp = gpar(col = par.strip.text$col,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if (style == 2) {
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill=fg, col = NULL))
            grid.text(label=x,
                      x = (2*1:num-1)/(2*num),
                      gp = gpar(col = par.strip.text$col,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style==3){
            grid.rect(gp = gpar(fill=bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill=fg, col = NULL))
            grid.text(label = paste(if(strip.names)
                      paste(name,":") else "", x[level]),
                      gp = gpar(col = par.strip.text$col, 
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style>=4){
            grid.rect(gp = gpar(fill=bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(col=NULL, fill=fg))
            grid.text(label=x,
                      x = (2* 1:num - 1)/(2*num),   #using default.units
                      gp = gpar(col = par.strip.text$col, 
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
    }
}





lsegments <-
    function(x0, y0, x1, y1, 
             col = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd, ...)

{
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length = ml)
    x1 <- rep(x1, length = ml)
    y0 <- rep(y0, length = ml)
    y1 <- rep(y1, length = ml)
    for (i in seq(along=x0))
        grid.lines(x = c(x0[i], x1[i]),
                   y = c(y0[i], y1[i]),
                   gp = gpar(lty=lty, col=col, lwd=lwd),
                   default.units="native")
}


larrows <-
    function(...) 
{
    warning("larrows not corectly implemented yet")
    lsegments(...)
}



ltext <-
    function(x, y = NULL, labels = seq(along = x),
             col = add.text$col,
             cex = add.text$cex,
             srt = 0,
             adj = .5, ...)
{
    add.text <- trellis.par.get("add.text")
    xy <- xy.coords(x, y)
    grid.text(label = as.character(labels), x = xy$x, y = xy$y,
              gp = gpar(col = col), rot = srt, 
              just = c(if (adj == 0) "left"
              else if (adj == 1) "right" else "centre", "centre"),
              ##size = unit(cex * 2.5, "mm"),
              default.units = "native")
}





llines <-
    function(x, y = NULL, type = "l",
             col = plot.line$col,
             lty = plot.line$lty,
             lwd = plot.line$lwd, ...)
{
    plot.line <- trellis.par.get("plot.line")
    lplot.xy(xy.coords(x, y), type = type,
             col = col, lty = lty, lwd = lwd, ...)
}




lpoints <-
    function(x, y = NULL, type = "p",
             col = plot.symbol$col,
             pch = plot.symbol$pch,
             cex = plot.symbol$cex, ...)
{
    plot.symbol <- trellis.par.get("plot.symbol")
    lplot.xy(xy.coords(x, y), type = type,
             col = col, pch = pch, cex = cex, ...)
}






lplot.xy <-
    function(xy, type, pch = 1, lty = 1, col = 1, cex = 1, lwd = 1, ...)
{
    x <- xy$x
    y <- xy$y

    if (type %in% c("l", "o", "b", "c"))
        grid.lines(x=x, y=y, gp = gpar(lty=lty, col=col, lwd=lwd),
                   default.units="native")
    
    if (type %in% c("p", "o", "b", "c"))
        grid.points(x = x, y = y, size = unit(cex * 2.5, "mm"),
                    gp = gpar(col = col, cex = cex),
                    pch = pch, 
                    default.units="native")

    if (type %in% c("s", "S")) {
        ord <- order(x)
        n <- length(x)
        xx <- numeric(2*n-1)
        yy <- numeric(2*n-1)

        xx[2*1:n-1] <- x[ord]
        yy[2*1:n-1] <- y[ord]
        xx[2*1:(n-1)] <- x[ord][if (type=="s") -1 else -n]
        yy[2*1:(n-1)] <- y[ord][if (type=="s") -n else -1]
        grid.lines(x=xx, y=yy,
                   gp = gpar(lty=lty, col=col, lwd=lwd),
                   default.units="native")
    }

    if (type == "h") {

        ylim <- current.viewport()$yscale

        zero <-
            if (ylim[1] > 0) ylim[1]
            else if (ylim[2] < 0) ylim[2]
            else 0

        print(zero)
        print(x)
        for (i in seq(along=x))
            grid.lines(x=rep(x[i],2), y=c(y[i], zero),
                       gp = gpar(lty=lty, col=col, lwd=lwd),
                       default.units="native")
    }
}







