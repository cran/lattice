

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

## the foll functions don't do much error checking yet



panel.tmd <- function(...) {
    panel.abline(h=0)
    panel.xyplot(...)
}




panel.abline <-
    function(a, b = NULL, h=numeric(0), v=numeric(0),
             col=add.line$col, lty=add.line$lty, ...)
{
    add.line <- trellis.par.get("add.line")
    
    if (!missing(a)) {
        if (inherits(a,"lm")) coef <- a$coef
        else coef <- c(a,b)
        
        if (coef[2]==0) h <- c(h, coef[1])
        else {
            xx <- current.viewport()$xscale
            yy <- current.viewport()$yscale
            
            x <- numeric(0)
            y <- numeric(0)
            ll <- function(i, j, k, l)
                (yy[j]-coef[1]-coef[2]*xx[i]) *
                    (yy[l]-coef[1]-coef[2]*xx[k])
            
            if (ll(1,1,2,1)<=0) {
                y <- c(y, yy[1])
                x <- c(x, (yy[1]-coef[1])/coef[2])
            }
            
            if (ll(2,1,2,2)<=0) {
                x <- c(x, xx[2])
                y <- c(y, coef[1] + coef[2] * xx[2])
            }
            
            if (ll(2,2,1,2)<=0) {
                y <- c(y, yy[2])
                x <- c(x, (yy[2]-coef[1])/coef[2])
            }
            
            if (ll(1,2,1,1)<=0) {
                x <- c(x, xx[1])
                y <- c(y, coef[1] + coef[2] * xx[1])
            }
            
            if (length(x)>0)
                grid.lines(x=x, y = y, default.units="native",
                           gp = gpar(col=col, lty=lty))
        }
    }
    
    
    for(i in seq(along=h))
        grid.lines(y=rep(h[i],2), default.units="native", gp = gpar(col = col, lty = lty))

    for(i in seq(along=v))
        grid.lines(x=rep(v[i],2), default.units="native", gp = gpar(col = col, lty = lty))
    
}








panel.fill <-
    function(col="grey",...)
{
    grid.rect(fill=col)
}












panel.grid <-
    function(h=3, v=3, col=reference.line$col, lty=reference.line$lty, ...)
{
    reference.line <- trellis.par.get("reference.line")

    if (h>0)
        for(i in 1:h)
            grid.lines(y=rep(i/(h+1),2), gp = gpar(col = col, lty = lty), default.units="npc")

    if (v>0)
        for(i in 1:v)
            grid.lines(x=rep(i/(v+1),2), gp = gpar(col = col, lty = lty), default.units="npc")


    ## Cheating here a bit for h=-1, v=-1. Can't think of any neat way to
    ## get the actual `at' values of the panel (Can pass it in though)

    if (h<0)
    {
##        at <- .Call("L_pretty", lcurrent.viewport()$yscale)
        scale <- current.viewport()$yscale
        at <- lpretty(scale)
        at <- at[at>scale[1] & at < scale[2]]
        for(i in seq(along=at))
            grid.lines(y=rep(at[i],2), default.units="native", gp = gpar(col = col, lty = lty))
    }

    
    if (v<0)
    {
##        at <- .Call("L_pretty", lcurrent.viewport()$xscale)
        scale <- current.viewport()$xscale
        at <- lpretty(scale)
        at <- at[at>scale[1] & at < scale[2]]
        for(i in seq(along=at))
            grid.lines(x=rep(at[i],2), default.units="native", gp = gpar(col = col, lty = lty))
    }


}





panel.lmline <-
    function(x, y, ...) if (length(x)>0) panel.abline(lm(y ~ x), ...) 



prepanel.lmline <-
    function(x, y, ...)
{
    if (length(x)>0) {
        coef <- lm(y~x)$coef
        tem <- coef[1] + coef[2] * range(x)
        list(xlim=range(x), ylim=range(y,tem), 
             dx=diff(range(x)), dy=diff(tem))         
    }
    else list(xlim=c(0,1), ylim=c(0,1), dx=1, dy=1)
    ## if length(x)==0 this function shouldn't be called at all
    ## during the setup phase (preparing limits and aspects.
    ## The only case I can think of when a prepanel fn with
    ## length 0 arguments can be called is when relation!= same
    ## in which case it might be called to calculate axis
    ## limits on a panel by panel basis. In that case, some
    ## value of xlim and ylim must be returned, but the exact
    ## value wouldn't matter (except, pehaps for the fact that
    ## it might mess up the axis labelling)
}





## the foll is actually a wrapper around the R fn lowess - arguments
## won't match with trellis

panel.loess <-
    function(x, y, span=2/3, iter=3, col=add.line$col, lty=add.line$lty, ...)
{
    if (length(x)>0) {
        add.line <- trellis.par.get("add.line")
        
        smooth <- lowess(x,y, f=span, iter=iter)
        grid.lines(x=smooth$x, y=smooth$y, default.units="native", gp = gpar(col = col, lty = lty))
    }
}


prepanel.loess <-
    function(x, y, span=2/3, iter=3, ...)
{
    if (length(x)>0) {
        smooth <- lowess(x,y, f=span, iter=iter)
        list(xlim = range(x,smooth$x),
             ylim = range(y,smooth$y),
             dx = diff(smooth$x),
             dy = diff(smooth$y))
    }
    else list(xlim=c(0,1), ylim=c(0,1), dx=1, dy=1)
}






panel.xyplot <-
    function(x, y, type="p",
             pch = plot.symbol$pch,
             col = if(type == "l") plot.line$col else plot.symbol$col,
             lty = plot.line$lty, cex = plot.symbol$cex, ...)
{
    if (length(x)>0) {
        plot.symbol <- trellis.par.get("plot.symbol")
        plot.line <- trellis.par.get("plot.line")
        if(type=="p"||type=="b")
            grid.points(x = x, y = y, size = unit(cex * 2.5, "mm"),
                        gp = gpar(col = col, cex = cex),
                        pch=pch, 
                        default.units="native")
        if(type=="l"||type=="b")
            grid.lines(x=x, y=y, gp = gpar(lty=lty, col=col),
                                 default.units="native")
    }
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
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(stats$stats[4:5],"native"),
                           y=unit(c(.5,.5), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(rep(stats$stats[1],2),"native"),
                           y=unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
                           lty = box.umbrella$lty))
                
                grid.lines(x = unit(rep(stats$stats[5],2),"native"),
                           y=unit(c(0,1), "npc"),
                           gp = gpar(col = box.umbrella$col,
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
                
                pop.viewport(current.viewport())
                
            }
        }
    
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

    panel.abline(h=1:levels.y, col=dot.line$col, lty=dot.line$lty)

    panel.xyplot(x = x, y = y, col = col, pch = pch, ...)

}






panel.stripplot <-
    function(x, y, jitter.data = F, factor = 0.5,
             pch=plot.symbol$pch, col=plot.symbol$col, ...)
{

    plot.symbol <- trellis.par.get("plot.symbol")
    y.jitter  <-
        if (jitter.data) jitter(y, factor = factor)
        else y
    panel.xyplot(x = x, y = y.jitter, pch = pch, col = col, ...)
    
}











panel.parallel <- function(z, subscripts,
                           col=superpose.line$col,
                           lty=superpose.line$lty, ...)
{
    superpose.line <- trellis.par.get("superpose.line")
    reference.line <- trellis.par.get("reference.line")

    n.r <- ncol(z)
    n.c <- length(subscripts)
    col <- rep(col, length=n.c)
    lty <- rep(lty, length=n.c)

    llim <- numeric(n.r)
    ulim <- numeric(n.r)
    dif <- numeric(n.r)
    if(n.r>0)
        for(i in 1:n.r) {
            grid.lines(x = c(0,1), y = c(i,i),
                       default.units = "native",
                       gp = gpar(col = reference.line$col,
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
                   gp = gpar(col=col[i], lty=lty[i]),
                   default.units="native")
    }
    
}









panel.superpose <-
    function(x, y, subscripts, groups, type="p",
             col = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex, 
             lty = superpose.line$lty,
             ...)
{

    if (length(x)>0) {

        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        
        if (is.factor(x)) x <- as.numeric(x)
        if (is.factor(y)) y <- as.numeric(y)
        
        if (is.shingle(x) || is.shingle(y))
            stop("sorry, panel.superpose does not allow shingles")
        
        vals <- sort(unique(groups))
        nvals <- length(vals)
        col <- rep(col, length=nvals)
        pch <- rep(pch, length=nvals)
        lty <- rep(lty, length=nvals)
        cex <- rep(cex, length=nvals)
        lcol <- rep(superpose.line$col, length=nvals)

        for (i in seq(along=vals))
        {
            id <- (groups[subscripts] == vals[i])
            if (any(id))
            {

                if(type=="p" || type=="b")
                    grid.points(x=x[id], y=y[id],
                                size = unit(cex[i] * 2.5, "mm"),
                                pch = pch[i], 
                                gp = gpar(col = col[i],
                                cex = cex[i]),
                                default.units = "native")
                if(type=="l"||type=="b")
                    grid.lines(x=x[id], y=y[id],
                               gp = gpar(lty=lty[i], col=lcol[i]),
                               default.units="native")
            }
        }
    }
}




panel.histogram <- function(x,
                            breaks, 
                            type = "density",
                            col = bar.fill$col,
                            ...)
{
    if (length(x)>0) {
        bar.fill  <- trellis.par.get("bar.fill")
        
        if (is.null(breaks)) {
            
            nint <- round(log2(length(x)) + 1)
            breaks <- quantile(x, 0:nint/nint)
            
        }
        
        h <- hist(x, breaks = breaks, plot = F, ...)
        y <-
            if (type == "count") h$counts
            else if (type == "percent") 100 * h$counts/length(x)
            else h$intensities
        
        grid.lines(x = c(0.05, 0.95),
                   y = unit(c(0,0),"native"),
                   default.units = "npc")
        
        nb <- length(breaks)
        if (nb != (length(y)+1)) warning("something is probably wrong")
        
        if (nb>1) {
            for(i in 1:(nb-1))
                if (y[i]>0) {
                    grid.rect(gp = gpar(fill = col),
                              x = breaks[i],
                              y = 0,
                              height = y[i],
                              width = breaks[i+1]-breaks[i],
                              just = c("left", "bottom"),
                              default.units = "native")
                }
        }
    }
}


panel.densityplot <- function(x, n = 50, ...) {
    
    h <- density(x, n = n, ...)
    lim <- current.viewport()$xscale
    id <- (h$x>=lim[1] & h$x<=lim[2])
    panel.xyplot(x = h$x[id], y = h$y[id], type = "l", ...)

}




panel.mathdensity <-
    function(dmath = dnorm,
             args = list(mean = 0, sd = 1),
             n = 50,
             col = reference.line$col, ...)
{

    reference.line <- trellis.par.get("reference.line")
    x <- do.breaks(endpoints = current.viewport()$xscale,
                   nint = n)
    y <- do.call("dmath", c(list(x = x),args))
    panel.xyplot(x = x, y = y, type = "l", col = col)
    
}


panel.qq <-
    function(x, y, f.value = ppoints, ...)
{

    uy  <- unique(y)
    panel.abline(a=0, b=1,
                 col = trellis.par.get("reference.line")$col)
    if (length(uy)==2) {
        x.val <- x[y==1]
        y.val <- x[y==2]
        n <- max(length(x.val), length(y.val))
        p  <- f.value(n)
        panel.xyplot(x = quantile(x = x.val, probs = p),
                     y = quantile(x = y.val, probs = p),
                     ...)
    }
    else warning("There has to be exactly 2 levels of y")
    
}


panel.qqmath <-
    function(...) panel.xyplot(...)

panel.splom <-
    function(...) panel.xyplot(...)


panel.qqmathline <-
    function(y, distribution, ...)
{
    yy <- quantile(y, c(0.25, 0.75))
    xx <- distribution(c(0.25, 0.75))
    r <- diff(yy)/diff(xx)
    panel.abline(c( yy[1]-xx[1]*r , r), ...)
}

prepanel.qqmathline <-
    function(x, distribution, ...)
{
    yy <- quantile(x, c(0.25, 0.75))
    xx <- distribution(c(0.25, 0.75))
    n <- length(x)
    list(ylim = range(x), xlim = range(distribution(c(1,n)/(n+1))),
         dx = diff(xx), dy = diff(yy))
}



panel.splom <-
    function(...) panel.xyplot(...)

