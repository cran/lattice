


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



## starting with these settings:
## trellis.settings must be available as a global variable

trellis.settings <-   # color settings
    list(background=list(col="white"),
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
         lty=c(1,1,1,1,1,1,1), lwd=c(1,1,1,1,1,1,1)),
         regions = list(col = rev(cm.colors(100))),
         superpose.symbol=list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
         col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8",
         "#f8fc00","#f80000"),
         font=c(1,1,1,1,1,1,1),
         pch=c("o","o","o","o","o","o","o")),
         axis.line=list(line=0, col="black", lty=1, lwd=1),
         par.xlab.text = list(cex = 1, col = "black", font = 1),
         par.ylab.text = list(cex = 1, col = "black", font = 1),
         par.main.text = list(cex = 1.2, col = "black", font = 2),
         par.sub.text = list(cex = 1, col = "black", font = 2))


lpretty <- function(x, ...) { 
    eps <- 1e-10
    ##{    print(x); pretty(x, ...) }
    ##    .Call("L_pretty", range(x))
    at <- pretty(x, ...)
    at <- ifelse(abs(at-round(at, 3))<eps, round(at, 3), at)
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
             color = TRUE,
             bg =
             if (color && (
                           if (is.character(device)) device != "postscript"
                           else deparse(substitute(device)) != "postscript"
                           )) "white"
             else "#f8fcf8",
             new = TRUE,
             first = FALSE,
             ...)
{

    # Paul 30/05/01
    # Changed "device" to "device.call" so that the device name and
    # the device function cannot be confused
    if (is.character(device)) {
        device.call <- get(device)
    }
    else device.call <- device
    
    if (new) {
        device.call(...)
        .lattice.print.more <<- first
    }
    
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
                                                 font=c(1,1,1,1,1,1,1),
                                                 pch=c("o","o","o","o","o","o","o")))
        trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
        trellis.par.set("par.xlab.text" , list(cex = 1, col = "black", font = 1))
        trellis.par.set("par.ylab.text" , list(cex = 1, col = "black", font = 1))
        trellis.par.set("par.main.text" , list(cex = 1.2, col = "black", font = 2))
        trellis.par.set("par.sub.text" , list(cex = 1, col = "black", font = 2))
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
        trellis.par.set("par.xlab.text" , list(cex = 1, col = "black", font = 1))
        trellis.par.set("par.ylab.text" , list(cex = 1, col = "black", font = 1))
        trellis.par.set("par.main.text" , list(cex = 1.2, col = "black", font = 2))
        trellis.par.set("par.sub.text" , list(cex = 1, col = "black", font = 2))
        
    }
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

    num.l.x <- nlevels(x) 

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



## This converts character to factor, numeric to shingle, and
## in addition, takes subsets
as.factorOrShingle <- function(x, subset = TRUE, drop = FALSE)
{
    if (is.character(x)) as.factor(x)[subset, drop = drop]
    else if (is.numeric(x)) as.shingle(x)[subset, drop = drop]
    else x[subset, drop = drop]
}


## defunct:
ldrop.levels <- function(x, subset)
    x[subset, drop = TRUE]




lset <-
    function(theme = c("col.bg", "white.bg", "bw"), file) {

        if (!missing(file)) source(file)
        else if (theme[1] == "col.bg") {
            trellis.par.set("background", list(col="#f8fcf8"))
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
                                                     font=c(1,1,1,1,1,1,1),
                                                     pch=c("o","o","o","o","o","o","o")))
            trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
            trellis.par.set("par.xlab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.ylab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.main.text" , list(cex = 1.2, col = "black", font = 2))
            trellis.par.set("par.sub.text" , list(cex = 1, col = "black", font = 2))
        }
        else if (theme[1] == "white.bg") {
            trellis.par.set("background", list(col="white"))
            trellis.par.set("add.line", list(col="black", lty=1, lwd=1))
            trellis.par.set("add.text", list(cex=1, col="black", font=1))
            trellis.par.set("bar.fill", list(col="turquoise"))
            trellis.par.set("box.dot", list(col="black", cex=1, font=1, pch=16))
            trellis.par.set("box.rectangle", list(col="darkgreen", lty=1, lwd=1))
            trellis.par.set("box.umbrella", list(col="darkgreen", lty=2, lwd=1))
            trellis.par.set("dot.line", list(col="#e8e8e8", lty=1, lwd=2))
            trellis.par.set("dot.symbol", list(cex=0.8, col="darkgreen", font=1, pch=16))
            trellis.par.set("plot.line", list(col="darkgreen", lty=1, lwd=1))
            trellis.par.set("plot.symbol", list(cex=0.8, col="darkgreen", font=1, pch=1))
            trellis.par.set("regions", list(col = terrain.colors(100)))
            trellis.par.set("reference.line", list(col="#e8e8e8", lty=1, lwd=2))   
            trellis.par.set("strip.background", list(col=c("#f8d088","#c8fcc8","#c0fcf8",
                                                     "#a8e0f8","#f8c0f8","#f88c88","#f8fcc0")))
            trellis.par.set("strip.shingle", list(col=c("#f87c00", "#00fc00", "#00fcf8",
                                                  "#007cf8","#f800f8","#f80000","#f8fc00")))
            trellis.par.set("superpose.line", list(col = c("darkgreen","red","royalblue",
                                                   "brown","orange","turquoise", "orchid"),
                                                   lty=c(1,1,1,1,1,1,1), lwd=c(1,1,1,1,1,1,1)))
            trellis.par.set("superpose.symbol", list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
                                                     col = c("darkgreen","red","royalblue",
                                                     "brown","orange","turquoise", "orchid"),
                                                     font=c(1,1,1,1,1,1,1),
                                                     pch=c("o","o","o","o","o","o","o")))
            trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
            trellis.par.set("par.xlab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.ylab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.main.text" , list(cex = 1.2, col = "black", font = 2))
            trellis.par.set("par.sub.text" , list(cex = 1, col = "black", font = 2))
        }
        else if (theme[1] == "bw") {
            trellis.par.set("background", list(col=bg))
            trellis.par.set("add.line", list(col="black", lty=1, lwd=1))
            trellis.par.set("add.text", list(cex=1, col="black", font=1))
            trellis.par.set("bar.fill", list(col="#e8e8e8"))
            trellis.par.set("box.dot", list(col="black", cex=1, font=1, pch=16))
            trellis.par.set("box.rectangle", list(col="black", lty=1, lwd=1))
            trellis.par.set("box.umbrella", list(col="black", lty=1, lwd=1))
            trellis.par.set("dot.line", list(col="#e4e4e4", lty=1, lwd=2))
            trellis.par.set("dot.symbol", list(cex=0.8, col="black", font=1, pch=16))
            trellis.par.set("plot.line", list(col="black", lty=1, lwd=1))
            trellis.par.set("plot.symbol", list(cex=0.8, col="black", font=1, pch=1))
            trellis.par.set("regions", list(col = gray(0:99/99)))
            trellis.par.set("reference.line", list(col="#e4e4e4", lty=1, lwd=2))
            trellis.par.set("strip.background", list(col="#e8e8e8"))
            trellis.par.set("strip.shingle", list(col="#a8a8a8"))
            trellis.par.set("superpose.line", list(col=rep("black", 7),
                                                   lty=c(1,2,3,4,5,6,7),
                                                   lwd=c(1,1,1,1,1,1,1)))
            trellis.par.set("superpose.symbol", list(cex=c(0.85,0.85,0.85,0.85,0.85,0.85,0.85),
                                                     col=rep("black", 7),
                                                     font=c(1,1,1,1,1,1,1),
                                                     pch=c("\001","+",">","s","w","#","{")))
            trellis.par.set("axis.line", list(line=0, col="black", lty=1, lwd=1))
            trellis.par.set("par.xlab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.ylab.text" , list(cex = 1, col = "black", font = 1))
            trellis.par.set("par.main.text" , list(cex = 1.2, col = "black", font = 2))
            trellis.par.set("par.sub.text" , list(cex = 1, col = "black", font = 2))
        }
    }




lset.textfont <-
    function(xlab, ylab, main, sub, add) {
        if (!missing(xlab)) {
            text <- trellis.par.get("par.xlab.text")
            text$font <- xlab
            trellis.par.set("par.xlab.text", text)
        }
        if (!missing(ylab)) {
            text <- trellis.par.get("par.ylab.text")
            text$font <- ylab
            trellis.par.set("par.ylab.text", text)
        }
        if (!missing(main)) {
            text <- trellis.par.get("par.main.text")
            text$font <- main
            trellis.par.set("par.main.text", text)
        }
        if (!missing(sub)) {
            text <- trellis.par.get("par.sub.text")
            text$font <- sub
            trellis.par.set("par.sub.text", text)
        }
        if (!missing(add)) {
            text <- trellis.par.get("add.text")
            text$font <- sub
            trellis.par.set("add.text", text)
        }
    }




lset.textcol <-
    function(xlab, ylab, main, sub, add) {
        if (!missing(xlab)) {
            text <- trellis.par.get("par.xlab.text")
            text$col <- xlab
            trellis.par.set("par.xlab.text", text)
        }
        if (!missing(ylab)) {
            text <- trellis.par.get("par.ylab.text")
            text$col <- ylab
            trellis.par.set("par.ylab.text", text)
        }
        if (!missing(main)) {
            text <- trellis.par.get("par.main.text")
            text$col <- main
            trellis.par.set("par.main.text", text)
        }
        if (!missing(sub)) {
            text <- trellis.par.get("par.sub.text")
            text$col <- sub
            trellis.par.set("par.sub.text", text)
        }
        if (!missing(add)) {
            text <- trellis.par.get("add.text")
            text$col <- sub
            trellis.par.set("add.text", text)
        }
    }



lset.textcex <-
    function(xlab, ylab, main, sub, add) {
        if (!missing(xlab)) {
            text <- trellis.par.get("par.xlab.text")
            text$cex <- xlab
            trellis.par.set("par.xlab.text", text)
        }
        if (!missing(ylab)) {
            text <- trellis.par.get("par.ylab.text")
            text$cex <- ylab
            trellis.par.set("par.ylab.text", text)
        }
        if (!missing(main)) {
            text <- trellis.par.get("par.main.text")
            text$cex <- main
            trellis.par.set("par.main.text", text)
        }
        if (!missing(sub)) {
            text <- trellis.par.get("par.sub.text")
            text$cex <- sub
            trellis.par.set("par.sub.text", text)
        }
        if (!missing(add)) {
            text <- trellis.par.get("add.text")
            text$cex <- sub
            trellis.par.set("add.text", text)
        }
    }













"[.shingle" <-
    function(x, subset, drop = FALSE)
{
    if (!is.shingle(x)) stop("x must be a shingle")
    ans <- as.numeric(x)[subset]
    attr(ans, "levels") <- levels(x)
    class(attr(ans, "levels")) <- "shingleLevel"
    if (drop) {
        xlvs <- levels(ans)
        dl <- logical(nlevels(ans))
        for (i in seq(along=dl))
            dl[i] <- any( ans >= xlvs[[i]][1] & ans <= xlvs[[i]][2] )
        attr(ans, "levels") <- xlvs[dl]
        class(attr(ans, "levels")) <- "shingleLevel"
    }
    class(ans) <- "shingle"
    ans
}






Rows <- function(x, which)
{
    for (i in seq(along = x)) x[[i]] <- x[[i]][which]
    x
}
## S-Plus trellis function needed for nlme.



make.list.from.intervals <- function(x)
{
    if (ncol(x)!=2) stop("x must be matrix with 2 columns")
    if (nrow(x)<1) stop("x must be matrix with at least 1 row")
    ans <- as.list(1:nrow(x))
    for (i in 1:nrow(x))
        ans[[i]] <- x[i,]
    ans
}



equal.count <-
  function(x, ...)
{
    attr(x, "levels") <- make.list.from.intervals(co.intervals(x,...))
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}



shingle <-
    function(x, intervals=sort(unique(x)))
{
    if (ncol(as.matrix(intervals))==1)
        intervals <- cbind(intervals, intervals)
    else if (ncol(as.matrix(intervals)) > 2)
        stop("bad value of 'intervals'")
    attr(x, "levels") <- make.list.from.intervals(intervals)
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


is.shingle <-
    function(x) inherits(x, "shingle")


as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)



summary.shingle <- function(object, ...) print.shingle(object, ...)


print.shingleLevel <-
    function(x, ...) {
        print(do.call("rbind", x))
        invisible(x)
    }

print.shingle <- function(x, ...) {
    cat("\nData:\n")
    print(as.numeric(x))
    l <- levels(x)
    n <- nlevels(x)
    if (n<1) cat("\nno intervals\n")
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x>=l[[i]][1] & x<=l[[i]][2]])
        }
        cat("\nIntervals:\n")
        print(int)
        olap <- numeric(n-1)
        if (n>2)
            for (i in 1:(n-1))
                olap[i] <- length(x[ x>=l[[i]][1] & x<=l[[i]][2] & x>=l[[i+1]][1] & x<=l[[i+1]][2]   ])
        cat("\nOvrlap between adjacent intervals:\n")
        print(olap)
    }
    invisible(x)
}






show.settings <- function() {

    n.row <- 13
    n.col <- 9

    heights.x <- rep(1, n.row)
    heights.units <- rep("lines", n.row)
    heights.units[c(2, 5, 8, 11)] <- "null"

    widths.x <- rep(1, n.row)
    widths.units <- rep("lines", n.row)
    widths.units[c(2, 4, 6, 8)] <- "null"

    page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                               widths = unit(widths.x, widths.units),
                               heights = unit(heights.x, heights.units))

    if (!.lattice.print.more) grid.newpage()
    .lattice.print.more <<- FALSE
    grid.rect(gp = gpar(fill = trellis.par.get("background")$col,
              col = "transparent"))
    push.viewport(viewport(layout = page.layout))



    superpose.symbol <- trellis.par.get("superpose.symbol")
    len <- length(superpose.symbol$col)
    push.viewport(viewport(layout.pos.row = 2,
                           layout.pos.col = 2,
                           yscale = c(0,len+1),
                           xscale = c(0,len+1)))
    for (i in 1:len) {
        lpoints(y = rep(i, len), x = 1:len,
                col = superpose.symbol$col[i],
                font = superpose.symbol$font[i],
                cex = superpose.symbol$cex[i],
                pch = superpose.symbol$pch[i])
    }
    pop.viewport()
    grid.text(lab = "superpose.symbol",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 3, layout.pos.col = 2))


    superpose.line <- trellis.par.get("superpose.line")
    len <- length(superpose.line$col)
    push.viewport(viewport(layout.pos.row = 2,
                           layout.pos.col = 4,
                           yscale = c(0,len+1),
                           xscale = c(0,1)))
    for (i in 1:len) {
        llines(y = rep(i, 2), x = c(0,1),
               col = superpose.line$col[i],
               lty = superpose.line$lty[i],
               lwd = superpose.line$lwd[i])
    }
    pop.viewport()
    grid.text(lab = "superpose.line",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 3, layout.pos.col = 4))



    strip.background <- trellis.par.get("strip.background")
    len <- length(strip.background$col)
    push.viewport(viewport(layout.pos.row = 2,
                           layout.pos.col = 6,
                           yscale = c(0,len+1),
                           xscale = c(0,1)))
    for (i in 1:len) {
        grid.rect(y = unit(i, "native"), h = unit(.5, "native"),
                  gp = gpar(fill = strip.background$col[i]))
    }
    pop.viewport()
    grid.text(lab = "strip.background",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 3, layout.pos.col = 6))


    strip.shingle <- trellis.par.get("strip.shingle")
    len <- length(strip.shingle$col)
    push.viewport(viewport(layout.pos.row = 2,
                           layout.pos.col = 8,
                           yscale = c(0,len+1),
                           xscale = c(0,1)))
    for (i in 1:len) {
        grid.rect(y = unit(i, "native"), h = unit(.5, "native"),
                  gp = gpar(fill = strip.shingle$col[i]))
    }
    pop.viewport()
    grid.text(lab = "strip.shingle",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 3, layout.pos.col = 8))



    push.viewport(viewport(layout.pos.row = 5,
                           layout.pos.col = 2,
                           yscale = extend.limits(c(0,6)),
                           xscale = c(0,6)))
    x <- c(1,2,3,4,5)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    panel.abline(h=1:5, col=dot.line$col,
                 lty=dot.line$lty, lwd=dot.line$lwd)
    panel.xyplot(x = x, y = x, col = dot.symbol$col, pch = dot.symbol$pch)
    grid.rect()
    pop.viewport()
    grid.text(lab = "dot.[symbol, line]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 6, layout.pos.col = 2))


    push.viewport(viewport(layout.pos.row = 5,
                           layout.pos.col = 4,
                           yscale = c(-1.5,1.5),
                           xscale = c(0,6)))
    x <- c(1,2,3,4,5)
    panel.bwplot(x = x, y = rep(0,5))
    grid.rect()
    pop.viewport()
    grid.text(lab = "box.[dot, rectangle, umbrella]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 6, layout.pos.col = 4))


    add.text <- trellis.par.get("add.text")
    add.line <- trellis.par.get("add.line")
    push.viewport(viewport(layout.pos.row = 5,
                           layout.pos.col = 6,
                           yscale = c(-1,1),
                           xscale = c(0,1)))
    x <- seq(.1, .9, length = 50)
    y <- .9 * sin(.1+11*x)
    llines(x = x, y = y, type = "l", col = add.line$col,
           lty = add.line$lty, lwd = add.line$lwd)
    ltext(lab = c("Hello", "World"),
          x = c(.25, .75), y = c(-.5, .5),
          col = add.text$col, cex = add.text$cex, font = add.text$font)
    grid.rect()
    pop.viewport()
    grid.text(lab = "add.[line, text]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 6, layout.pos.col = 6))




    push.viewport(viewport(layout.pos.row = 5,
                           layout.pos.col = 8,
                           yscale = c(0,4),
                           xscale = c(0,4)))
    panel.grid()
    grid.rect()
    pop.viewport()
    grid.text(lab = "reference.line",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 6, layout.pos.col = 8))



    push.viewport(viewport(layout.pos.row = 8,
                           layout.pos.col = 2,
                           yscale = c(-1.1,1.1),
                           xscale = c(-.1,1.1)))
    x <- seq(.1, .9, length = 20)
    y <- .9 * sin(.1+11*x)
    panel.xyplot(x = x+.05, y = y+.1, type = "l")
    panel.xyplot(x = x-.05, y = y-.1)
    grid.rect()
    pop.viewport()
    grid.text(lab = "plot.[symbol, line]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 9, layout.pos.col = 2))




    bar.fill <- trellis.par.get("bar.fill")

    push.viewport(viewport(layout.pos.row = 8,
                           layout.pos.col = 4,
                           yscale = extend.limits(c(0,6)),
                           xscale = extend.limits(c(1,10))))
    grid.rect(x = c(3.5, 4.5, 5.5, 6.5, 7.5), w = rep(5,5),
              y = c(1,2,3,4,5), h = rep(.5, ,5),
              default.units = "native",
              gp = gpar(fill = bar.fill$col))
    grid.rect()
    pop.viewport()
    grid.text(lab = "plot.shingle[bar.fill]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 9, layout.pos.col = 4))


    push.viewport(viewport(layout.pos.row = 8,
                           layout.pos.col = 6,
                           yscale = extend.limits(c(0,7)),
                           xscale = extend.limits(c(0,7))))
    grid.rect(y = c(.5, 1, 1.5, 2, 2.5, 3, 3.5), w = rep(1,7),
              x = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5), h = 1:7,
              default.units = "native",
              gp = gpar(fill = bar.fill$col))
    grid.rect()
    pop.viewport()
    grid.text(lab = "histogram[bar.fill]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 9, layout.pos.col = 6))



    push.viewport(viewport(layout.pos.row = 8,
                           layout.pos.col = 8,
                           yscale = extend.limits(c(0,6)),
                           xscale = c(0,7)))
    grid.rect(x = rev(c(.5, 1, 1.5, 2, 2.5, 3)), h = rep(.5, 6),
              y = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), w = 6:1,
              default.units = "native",
              gp = gpar(fill = bar.fill$col))
    grid.rect()
    pop.viewport()
    grid.text(lab = "barchart[bar.fill]",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 9, layout.pos.col = 8))


    regions <- trellis.par.get("regions")
    len <- length(regions$col)
    push.viewport(viewport(layout.pos.row = 11,
                           layout.pos.col = 2,
                           xscale = c(0,len+1)))
    for (i in 1:len)
        grid.rect(x = i, w = 1, default.units = "native",
                  gp = gpar(col = NULL,  fill = regions$col[i]))
    grid.rect()
    pop.viewport()
    grid.text(lab = "regions",
              gp = gpar(fontsize = 8),
              vp = viewport(layout.pos.row = 12, layout.pos.col = 2))





    
    invisible()
}










strip.default <-
    function(which.given,
             which.panel,
             var.name,
             factor.levels,
             shingle.intervals,
             strip.names = c(FALSE, TRUE),
             style = 1,
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"))
{
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length = 2)
    
    if (is.null(factor.levels)) { # means this is a  shingle, as opposed to a factor
        if (is.null(shingle.intervals)) stop("both factor.levels and shingle.intervals cannot be NULL")
        strip.names <- strip.names[2]
        grid.rect(gp = gpar(fill=bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,])-t[1])/diff(t)
        grid.rect(x = unit(r%*%c(.5,.5),"npc"), width = unit(diff(r),"npc"),
                  gp = gpar(col=fg, fill=fg))
        if (strip.names) grid.text(label = name,
                                   gp = gpar(col = par.strip.text$col,
                                   font = par.strip.text$font,
                                   fontsize = par.strip.text$cex *
                                   current.viewport()$gp$fontsize))
        
        grid.rect()
    }
    else if (is.null(shingle.intervals)) { # factor
        strip.names <- strip.names[1]
        x <- factor.levels
        num <- length(x)
        if (style == 1) {
            grid.rect(gp = gpar(fill=bg))
            grid.text(label = paste(if(strip.names)
                      paste(name,": ") else "", x[level], sep = ""),
                      gp = gpar(col = par.strip.text$col,
                      font = par.strip.text$font,
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
                      font = par.strip.text$font,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if (style == 3){
            grid.rect(gp = gpar(fill=bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill=fg, col = NULL))
            grid.text(label = paste(if(strip.names)
                      paste(name,": ") else "", x[level], sep = ""),
                      gp = gpar(col = par.strip.text$col, 
                      font = par.strip.text$font,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style == 4){
            grid.rect(gp = gpar(fill=bg))
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(col=NULL, fill=fg))
            grid.text(label=x,
                      x = (2* 1:num - 1)/(2*num),   #using default.units
                      gp = gpar(col = par.strip.text$col, 
                      font = par.strip.text$font,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style >= 5){
            grid.rect(gp = gpar(fill=bg))
            grid.text(label=x[level],
                      x = (2* level - 1)/(2*num),   #using default.units
                      gp = gpar(col = par.strip.text$col, 
                      font = par.strip.text$font,
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
             font = 1,
             adj = .5, ...)
{
    add.text <- trellis.par.get("add.text")
    xy <- xy.coords(x, y)
    grid.text(label = as.character(labels), x = xy$x, y = xy$y,
              gp = gpar(col = col, font = font,
              fontsize = cex * 10),
              just = c(if (adj == 0) "left"
              else if (adj == 1) "right" else "centre", "centre"),
              rot = srt,
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
    function(xy, type, pch = 1, lty = 1, col = 1, cex = 1, lwd = 1, font = 1, ...)
{
    x <- xy$x
    y <- xy$y

    if (type %in% c("l", "o", "b", "c"))
        grid.lines(x=x, y=y, gp = gpar(lty=lty, col=col, lwd=lwd),
                   default.units="native")
    
    if (type %in% c("p", "o", "b", "c"))
        if (is.character(pch))
            grid.text(lab = rep(pch, length = length(x)),
                      x = x, y = y,
                      gp = gpar(col = col, fontsize = cex * 10),
                      default.units="native")
        else
            grid.points(x = x, y = y, size = unit(cex * 2.5, "mm"),
                        gp = gpar(col = col),
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

        ##print(zero) ?
        ##print(x) 
        for (i in seq(along=x))
            grid.lines(x=rep(x[i],2), y=c(y[i], zero),
                       gp = gpar(lty=lty, col=col, lwd=lwd),
                       default.units="native")
    }
}







