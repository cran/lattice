

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
.lattice.print.more <- F

## starting with these settings: (trellis.settings must be available as a global variable

trellis.settings <-   # color settings
    list(add.line=list(col="black", lty=1, lwd=1),
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
         
         
         superpose.line=list(col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8","#f8fc00","#f80000"), lty=c(1,1,1,1,1,1,1),lwd=c(1,1,1,1,1,1,1)),
         
         superpose.symbol=list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
         col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8","#f8fc00","#f80000"),
         font=c(5,5,5,5,5,5,5),
         pch=c(1,1,1,1,1,1,1)),
         
         axis.line=list(line=0, col="black", lty=1, lwd=1))


## this might be another way to implement this
##
##funlist <- local({
##    pars <-
##        list()
##    
##    list(pars.set = function(name, value) {
##        pars[[name]] <<- value  # Note the use of <<-
##    },
##         pars.get = function(name){
##             pars[[name]]
##         })
##})
##trellis.par.set <- funlist$pars.set
##trellis.par.get <- funlist$pars.get
##rm(funlist)



lpretty <- function(x, ...)
    .Call("L_pretty", range(x))



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
    function(device="x11",
             color=TRUE,
             bg=if(color)"#889088" else "#f8fcf8",
             new = TRUE, ...)
{

    if (is.character(device)) {
        device <- eval(as.name(device))
    }

    if (new) {
        device(...)
        grid.start()
    }
    par(bg=bg)
    
    if (color)
    {

        trellis.settings <<-   # color settings
            list(add.line=list(col="black", lty=1, lwd=1),
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
                 superpose.line=list(
                 col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8","#f8fc00","#f80000"),
                 lty=c(1,1,1,1,1,1,1),lwd=c(1,1,1,1,1,1,1)),
                 
                 superpose.symbol=list(cex=c(0.8,0.8,0.8,0.8,0.8,0.8,0.8),
                 col=c("#00fcf8","#f800f8","#00fc00","#f87c00","#007cf8","#f8fc00","#f80000"),
                 font=c(5,5,5,5,5,5,5),
                 pch = c(1,1,1,1,1,1,1)),
                 
                 axis.line=list(line=0, col="black", lty=1, lwd=1))
        
    }
    else
    {

        trellis.settings <<-           # black and white settings
            list(add.line=list(col="black", lty=1, lwd=1),
                 add.text=list(cex=1, col="black", font=1),
                 bar.fill=list(col="#e8f0e8"),
                 box.dot=list(col="black", cex=1, font=1, pch=16),
                 box.rectangle=list(col="black", lty=1, lwd=1),
                 box.umbrella=list(col="black", lty=1, lwd=1),
                 dot.line=list(col="#e4e0e4", lty=1, lwd=2),
                 dot.symbol=list(cex=0.8, col="black", font=1, pch=16),
                 plot.line=list(col="black", lty=1, lwd=1),
                 plot.symbol=list(cex=0.8, col="black", font=1, pch=1),
                 reference.line=list(col="#e4e0e4", lty=1, lwd=2),   
                 strip.background=list(col="#e8f0e8"),
                 strip.shingle=list(col="#a8b0a8"),
                 superpose.line=list(col=rep("black", 7),
                 lty=c(1,2,3,4,5,6,7),lwd=c(1,1,1,1,1,1,1)),
                 
                 superpose.symbol=list(cex=c(0.85,0.85,0.85,0.85,0.85,0.85,0.85),
                 col=rep("black", 7),
                 font=c(1,1,1,1,1,1,1),
                 pch=c("\001","+",">","s","w","#","{")),
                 axis.line=list(line=0, col="black", lty=1, lwd=1))
             

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





banking <- function(dx, dy)
{
    if(length(dx)!=length(dy)) stop("Non matching lengths")
    id <- dx!=0 & dy!=0
    if (any(id)) {
        r  <- abs(dx[id]/dy[id])
        median(r)
    }
    else 1
}


do.breaks  <- function(endpoints, nint)
{
    if (length(endpoints)!=2) stop("error")
    endpoints[1] + diff(endpoints) * 0:nint / nint
}



cupdate <- function(index, maxim)
{
    ##  This function is used to handle arbitrary number of
    ## conditioning variables : every time it is called, it
    ## increments the "current" level of the conditioning
    ## variables suitably, i.e., it tries to increment the
    ## level of the 1st conditining variable (the one which
    ## varies fastest along panel order) and if it happens
    ## to be at its maximum (last) value, it sets it to the
    ## first value AND increments the "current" level of the
    ## 2nd (next) conditioning variable recursively.

    ## This is an internal function, not to be documented
    ## for the high level user.
    
    if(length(index)!=length(maxim)||length(maxim)<=0)
        stop("Inappropriate arguments")
    index[1] <- index[1] + 1
    if(index[1]>maxim[1] && length(maxim)>1)
        c(1,cupdate(index[-1],maxim[-1]))
    else index
}


ldrop.levels <- function(x, subset) {
    ## These functions are used to handle the subset argument
    ## by dropping levels of factors or shingles when necessary
    UseMethod("ldrop.levels")
}

ldrop.levels.factor <- function(x, subset)
    x[subset, drop = TRUE]

ldrop.levels.shingle <- function(x, subset)
{
    x$x <- x$x[subset]
    dl <- logical(nrow(x$int))
    for (i in seq(along=dl))
        dl[i] <- any( x$x >= x$int[i,1] & x$x <= x$int[i,2]  )
    x$intervals <- x$intervals[dl,]
    x
}

ldrop.levels.default <- function(x, subset)
    x[subset]







Rows <- function(x, which)
{
    for (i in seq(along = x)) x[[i]] <- x[[i]][which]
    x
}
## S-Plus trellis function needed for nlme.





latticeParseFormula <-
    function(model, data)
{
    parseCond <-
        function(model)
        {
            model <- eval(parse(text = paste("~", deparse(model))))[[2]]
            model.vars <- list()
            while (length(model) == 3 && model[[1]] == as.name("*")) {
                model.vars <- c(model.vars, model[[3]])
                model <- model[[2]]
            }
            rev(c(model.vars, model))
        }
    if (!inherits(model, "formula"))
        stop("model must be a formula object")
    ans <- list(left = NULL, right = NULL, condition = NULL,
                left.name = character(0), right.name = character(0))
    if (length(model) == 3) {
        ans$left <- eval(model[[2]], data)
        ans$left.name <- deparse(model[[2]])
    }
    model <- model[[length(model)]]
    if (length(model) == 3 && model[[1]] == as.name("|")) {
        model.vars <- parseCond(model[[3]])
        ans$condition <- vector("list", length(model.vars))
        names(ans$condition) <- sapply(model.vars, deparse)
        for (i in seq(along = model.vars))
            ans$condition[[i]] <- eval(model.vars[[i]], data)
        model <- model[[2]]
    }
    ans$right <- eval(model, data)
    ans$right.name <- deparse(model)
    ans
}








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





is.shingle <-
  function(x) inherits(x, "shingle")






as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)







strip.default <-
    function(x, level, style = 1,#if(is.factor(x)) 1 else 3,
             strip.names = is.shingle(x), name="",
             bg, fg, par.strip.text = trellis.par.get("add.text"))
{
    if(is.shingle(x)) {
        grid.rect(gp = gpar(fill=bg))
        t <- range(x$intervals)
        r <- (range(x$intervals[level,])-t[1])/diff(t)
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
                      paste(name,":") else "",levels(x)[level]),
                      gp = gpar(col = par.strip.text$col,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if (style == 2) {
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill=fg, col = NULL))
            grid.text(label=levels(x),
                      x = (2*1:num-1)/(2*num),
                      gp = gpar(col = par.strip.text$col,
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style==3){
            grid.rect(fill=bg)
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(fill=fg, col = NULL))
            grid.text(label = paste(if(strip.names)
                      paste(name,":") else "",levels(x)[level]),
                      gp = gpar(col = par.strip.text$col, 
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
        else if(style>=4){
            grid.rect(fill=bg)
            grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                      width = unit(1/num, "npc"),
                      gp = gpar(col=NULL, fill=fg))
            grid.text(label=levels(x),
                      x = (2* 1:num - 1)/(2*num),   #using default.units
                      gp = gpar(col = par.strip.text$col, 
                      fontsize = par.strip.text$cex *
                      current.viewport()$gp$fontsize))
            grid.rect()
        }
    }
}








extend.limits <-
    function(lim, length=1, prop = 0.07) {
        if(length(lim)==2) {
            if (lim[1]>lim[2]) stop("Improper value of limit")
            if(!missing(length) && !missing(prop))
                stop("length and prop cannot both be specified")
            if(length <= 0) stop("length must be positive")
            if(!missing(length)) prop <- (length-diff(lim))/(2*diff(lim))
            if(lim[1]==lim[2]) lim + 0.5*c(-length,length)
            else {
                d <- diff(lim)
                lim + prop*d*c(-1,1)
            }
        }
        else {
            warning("improper value of limit in extend.limits")
            c(0,1)
        }
    }









print.trellis <-
    function(foo, position, split, more = F)
{

    new <- T
    if(.lattice.print.more) new <- F
    .lattice.print.more <<- more
    usual  <- (missing(position) & missing(split))
    if (!new && usual)
        warning("more is relevant only when split/position is specified")
    
    if (!missing(position)) {
        if (length(position)!=4) stop("Incorrect value of position")
        if (new) grid.newpage()

        push.viewport(viewport(x=position[1], y=position[2],
                               width=position[3]-position[1],
                               height=position[4]-position[2],
                               just=c("left","bottom")))
        
        if (!missing(split)) {
            if (length(split)!=4) stop("Incorrect value of split")

            push.viewport(viewport(layout = grid.layout(nrow=split[4],
                                   ncol = split[3])))
            push.viewport(viewport(layout.pos.row = split[2],
                                   layout.pos.col = split[1]))
        }
    }
    
    
    else if (!missing(split)) {
        
        if (length(split)!=4) stop("Incorrect value of split")
        if (new) grid.newpage()
        push.viewport(viewport(layout = grid.layout(nrow=split[4],
                               ncol = split[3])))
        push.viewport(viewport(layout.pos.row = split[2],
                               layout.pos.col = split[1]))
    }
    
    
    panel <- foo$panel # doing this to use "panel" in do.call
    axis.line <- trellis.par.get("axis.line")
    number.of.cond <- length(foo$cond)
    
    panel.width <- 1
    panel.height <- foo$aspect.ratio
    layout.respect <- !foo$aspect.fill
    

    if(number.of.cond<1 || foo$x.relation.same) {
        
        xaxis.col <-
            if (is.logical(foo$x.scales$col)) axis.line$col
            else foo$x.scales$col
        
        xaxis.cex <-
            foo$x.scales$cex * foo$fontsize.small / foo$fontsize.normal
        
        xaxis.rot <-
            if (is.logical(foo$x.scales$rot)) 0
            else foo$x.scales$rot
        
    }
    else {
        
        i <- which(!foo$skip)[1]
        
        xaxis.col <-
            if (is.logical(foo$x.scales[[i]]$col)) axis.line$col
            else foo$x.scales[[i]]$col
        
        xaxis.cex <-
            foo$x.scales[[i]]$cex * foo$fontsize.small / foo$fontsize.normal
        
        xaxis.rot <-
            if (is.logical(foo$x.scales[[i]]$rot)) 0
            else foo$x.scales[[i]]$rot
    }
    

    
    
    if(number.of.cond<1 || foo$y.relation.same) {
        
        yaxis.col <-
            if (is.logical(foo$y.scales$col)) axis.line$col
            else foo$y.scales$col
        
        yaxis.cex <-
            foo$y.scales$cex * foo$fontsize.small / foo$fontsize.normal
        
        yaxis.rot <-
            if (is.logical(foo$y.scales$rot)) 0
            else foo$y.scales$rot
        
    }
    else {
        
        i <- which(!foo$skip)[1]
        
        yaxis.col <-
            if (is.logical(foo$y.scales[[i]]$col)) axis.line$col
            else foo$y.scales[[i]]$col
        
        yaxis.cex <-
            foo$y.scales[[i]]$cex * foo$fontsize.small / foo$fontsize.normal
        
        
        yaxis.rot <-
            if (!is.logical(foo$y.scales[[i]]$rot)) foo$y.scales[[i]]$rot
            else if (is.logical(foo$y.scales[[i]]$labels)) 90
            else 0
        
    }
    
    
    
    if(number.of.cond>0)
    {
        strip.col.default.bg <-
            rep(trellis.par.get("strip.background")$col,length=number.of.cond)
        strip.col.default.fg <-
            rep(trellis.par.get("strip.shingle")$col,length=number.of.cond)
        cond.current.level <- rep(1,number.of.cond)
        cond.max.level <- integer(number.of.cond)
        for(i in 1:number.of.cond) {
            cond.max.level[i] <-
                if (is.shingle(foo$cond[[i]])) nrow(foo$cond[[i]]$intervals)
                else length(levels(foo$cond[[i]]))
        }
        
        
        if(foo$layout[1]==0) { # using device dimensions to
            ddim <- par("din") # calculate default layout
            device.aspect <- ddim[2]/ddim[1]
            panel.aspect <- if(layout.respect) panel.height else 1

            m <- round(sqrt(foo$layout[2] * device.aspect/panel.aspect))
            n <- ceiling(foo$layout[2]/m)
            foo$layout[1] <- n
            foo$layout[2] <- m
        }

        cols.per.page <- foo$layout[1]
        rows.per.page <- foo$layout[2]
        number.of.pages <- foo$layout[3]
        
        if(cols.per.page>1)
            foo$x.between <- rep(foo$x.between, length = cols.per.page-1)
        if(rows.per.page>1) 
            foo$y.between <- rep(foo$y.between, length = rows.per.page-1)

        foo$x.alternating <- rep(foo$x.alternating, length = cols.per.page)
        foo$y.alternating <- rep(foo$y.alternating, length = rows.per.page)
        
    }


    ## The following is needed even when there is no conditioning
    ## Towards that, relation.same must be defined even in that
    ## case (and has to be TRUE), even though it serves no other purpose
    ## Of course, this can be avoided by putting this code in a conditional
    ## loop later.
    if(foo$x.relation.same) {
        
        if(is.logical(foo$x.scales$at)) 
            foo$x.scales$at <-
                lpretty(foo$x.scales$limits,
                       n = foo$x.scales$tick.number)
        
        if(is.logical(foo$x.scales$labels)) 
            foo$x.scales$labels <- as.character(foo$x.scales$at)
        
    }    
    if(foo$y.relation.same) {
        
        if(is.logical(foo$y.scales$at)) 
            foo$y.scales$at <-
                lpretty(foo$y.scales$limits,
                       n = foo$y.scales$tick.number)
        
        if(is.logical(foo$y.scales$labels)) 
            foo$y.scales$labels <- as.character(foo$y.scales$at)
        
    }
    
    

    have.main <- !is.null(foo$main) & is.character(foo$main$label)
    have.sub <- !is.null(foo$sub) & is.character(foo$sub$label)
    have.xlab <- !is.null(foo$xlab) & is.character(foo$xlab$label)
    have.ylab <- !is.null(foo$ylab) & is.character(foo$ylab$label)
    
    ## Shall calculate the per page layout now:
    if (number.of.cond>0)
    {
        n.row <- rows.per.page * (number.of.cond + 2) + (rows.per.page-1) + 7
        ##       ^^^^^^^^^^^      ^^^^^^^^^^^^^^^^      ^^^^^^^^^^^^^^^   ^^^
        ##          panels         rows per panel           between     main, key-top, x-ticks-top
        ##                 (1 for axes when relation!=same)             x-ticks-bot, xlab, key-bot, sub

        n.col <- 2 * cols.per.page + (cols.per.page-1) + 5 # similar, without main and sub

        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- as.list(1:n.row)
                                        # not used now, but maybe needed if
                                        # rot = 90 for y-labels is supported
        
        widths.x <- rep(1, n.col)
        widths.units <- rep("lines", n.col)
        widths.data <- as.list(1:n.col) # this is required because allocating widths 
                                        # for y-axis annotation is more complicated

        ## fine tuning heights:
        
        heights.x[number.of.cond + 4 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
            panel.height # for the panels
        heights.units[number.of.cond + 4 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
            "null" # for the panels

        heights.x[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <- 0
        ## This is for the x-axis labels just below each panel if relation!="same"

        if(rows.per.page>1)
            heights.x[number.of.cond + 6 + (
                                            (if (foo$as.table) 1:(rows.per.page-1)
                                            else (rows.per.page-1):1)
                                            - 1)*(number.of.cond+3)] <-
                                                foo$y.between


        

        heights.x[1] <- if (have.main) 2 * foo$main$cex else 1
        heights.x[n.row] <- if (have.sub) 2 * foo$sub$cex else 1

        heights.x[2] <- 0 # for the key, currently unimplemented
        heights.x[n.row-1] <- 0 # key


        if (foo$x.draw) {

            if (foo$x.relation.same) {

                if (xaxis.rot == 0) {
                    
                    if(any(foo$x.alternating==1)) 
                        heights.x[3] <- 3 * xaxis.cex
                    
                    if (any(foo$x.alternating==2)) 
                        heights.x[n.row-3] <- 3 * xaxis.cex
                    
                }
                else {
                    
                    which.name <- "-"
                    for (ss in foo$x.scales$labels)
                        if (nchar(ss) > nchar(which.name)) which.name <- ss
                    if(any(foo$x.alternating==1)) {
                        heights.x[3] <- xaxis.cex  # cos(rot) ?
                        heights.units[3] <- "strwidth"
                        heights.data[[3]] <- paste(which.name, "--")
                    }
                    if (any(foo$y.alternating==2)) {
                        heights.x[n.row-3] <- xaxis.cex
                        heights.units[n.row-3] <- "strwidth"
                        heights.data[[n.row-3]] <- paste(which.name, "--")
                    }
                }
            }
            else { # relation != same


                if (xaxis.rot == 0) {

                    heights.x[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <- 
                        2 * xaxis.cex


                }
                else {
                    which.name <- "-"
                    for (sc in foo$x.scales)
                        if (is.list(sc)) {
                            
                            labs <-
                                if(is.logical(sc$labels))
                                    as.character(lpretty(sc$limits, n=sc$tick.number))
                                else sc$labels
                            
                            for (ss in labs)
                                if (nchar(ss) > nchar(which.name)) which.name <- ss
                        }
                    
                    heights.x[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                        yaxis.cex
                    heights.units[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                        "strwidth"
                    heights.data[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                        paste(which.name, "--")
                    
                }
                
            }
            
        }
        
        
        
        heights.x[n.row-2] <- if (have.xlab) 2 * foo$xlab$cex else 1
        widths.x[2] <- if (have.ylab) 3 * foo$ylab$cex else 1


         # this is if strip=F -- strips not to be drawn
        for(crr in 1:number.of.cond)
            heights.x[number.of.cond + 4 + (1:rows.per.page - 1)*(number.of.cond+3) -crr] <-
                if(is.logical(foo$strip)) 0  # which means strip = F, strips not to be drawn
                else foo$par.strip.text$cex

        ## fine tuning widths:
        
        widths.x[(1:cols.per.page - 1)*3 + 5] <-
            panel.width # for the panels
        widths.units[(1:cols.per.page - 1)*3 + 5] <-
            "null" # for the panels


        widths.x[(1:cols.per.page - 1)*3 + 4] <- 0
        ## For y-axes to the left of each panel when relation != "same"

        
        if(cols.per.page>1)
            widths.x[(1:(cols.per.page-1) - 1)*3 + 6] <-
                foo$x.between
        
        widths.x[1] <- 0 #for the key, currently unimplemented
        widths.x[n.col] <- 1 # key - right
        
        

        ## next part of the code decides how much space to leave for y-labels

        if (foo$y.draw) {
            
            if (foo$y.relation.same) {

                if (yaxis.rot == 90) {
                    
                    if(any(foo$y.alternating==1)) 
                        widths.x[3] <- 3 * yaxis.cex
                    
                    if (any(foo$y.alternating==2)) 
                        widths.x[n.col-1] <- 3 * yaxis.cex
                    
                }
                else {
                    
                    which.name <- "-"
                    for (ss in foo$y.scales$labels)
                        if (nchar(ss) > nchar(which.name)) which.name <- ss
                    if(any(foo$y.alternating==1)) {
                        widths.x[3] <- yaxis.cex  # cos(rot) ?
                        widths.units[3] <- "strwidth"
                        widths.data[[3]] <- paste(which.name, "--")
                    }
                    if (any(foo$y.alternating==2)) {
                        widths.x[n.col-1] <- yaxis.cex
                        widths.units[n.col-1] <- "strwidth"
                        widths.data[[n.col-1]] <- paste(which.name, "--")
                    }
                }
            }
            else { # relation != same
                if (yaxis.rot == 90) {
                    widths.x[(1:cols.per.page - 1)*3 + 4] <- 2 * yaxis.cex
                }
                else {
                    which.name <- "-"
                    for (sc in foo$y.scales)
                        if (is.list(sc)) {
                            
                            labs <-
                                if(is.logical(sc$labels))
                                    as.character(lpretty(sc$limits, n=sc$tick.number))
                                else sc$labels
                            
                            for (ss in labs)
                                if (nchar(ss) > nchar(which.name)) which.name <- ss
                        }
                    
                    widths.x[(1:cols.per.page - 1)*3 + 4] <- yaxis.cex
                    widths.units[(1:cols.per.page - 1)*3 + 4] <- "strwidth"
                    widths.data[(1:cols.per.page - 1)*3 + 4] <- paste(which.name, "--")
                    
                }

            }
            
        }
        
    }
    else
    {
        n.row <- 8
        ##  main, key-top, x-ticks-top, panel
        ##  x-ticks-bot, xlab, key-bot, sub
        
        n.col <- 6
        
        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- as.list(1:n.row)
        
        widths.x <- rep(1, n.col)
        widths.units <- rep("lines", n.col)
        widths.data <- as.list(1:n.col)  # this is required because allocating widths 
                                        # for y-axis annotation is more complicated



        ## fine tuning heights:
        
        heights.x[4] <- panel.height # for the panel
        heights.units[4] <- "null" # for the panel

        heights.x[1] <- if (have.main) 2 * foo$main$cex else 1
        heights.x[n.row] <- if (have.sub) 2 * foo$sub$cex else 1

        heights.x[2] <- 0 #for the key, currently unimplemented
        heights.x[n.row-1] <- 0 # key


        if (foo$x.draw) {
            if (any(foo$x.alternating==2)) heights.x[3] <- 3 # else 1 : x-ticks and labels at top
            if (any(foo$x.alternating==1)) heights.x[n.row-3] <- 3 # else 1 : x-ticks and labels at top
        }


        heights.x[n.row-2] <- if (!is.null(foo$xlab)) 2 * foo$xlab$cex else 1
        widths.x[2] <- if (!is.null(foo$ylab)) 3 * foo$ylab$cex else 1

        
        ## fine tuning widths:
        
        widths.x[4] <- panel.width # for the panel
        widths.units[4] <- "null" # for the panel

        widths.x[1] <- 0 #for the key, currently unimplemented
        widths.x[n.col] <- 0 # key


        if (foo$y.draw) {
            which.name <- "-"
            for (s in foo$y.scales$label)
                if (nchar(s)>nchar(which.name)) which.name <- s

            if(any(foo$y.alternating==1)) {
                widths.x[3] <- 1.0 * foo$fontsize.small/foo$fontsize.normal
                widths.units[3] <- "strwidth"
                widths.data[[3]] <- paste(which.name, "--")
            }
            if (any(foo$y.alternating==2)) {
                widths.x[n.col-1] <- 1.0 * foo$fontsize.small/foo$fontsize.normal
                widths.units[n.col-1] <- "strwidth"
                widths.data[[n.col-1]] <- paste(which.name, "--")
            }
        }
        
    }

    ## Constructing the layout:

    page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                               widths = unit(widths.x, widths.units, data=widths.data),
                               heights = unit(heights.x, heights.units, data=heights.data),
                               respect = layout.respect)
    

    if(number.of.cond<1) {
        
        if(usual) grid.newpage()
        
        push.viewport(viewport(layout = page.layout,
                               gp = gpar(fontsize = foo$fontsize.normal)))

        if (have.main)
            grid.text(label = foo$main$label,
                      gp = gpar(col = foo$main$col,
                      fontsize = foo$fontsize.normal * foo$main$cex),
                      vp = viewport(layout.pos.row = 1))

        if (have.sub)
            grid.text(label = foo$sub$label,
                      gp = gpar(col = foo$sub$col,
                      fontsize = foo$fontsize.normal * foo$sub$cex),
                      vp = viewport(layout.pos.row = n.row))

        if (have.xlab)
            grid.text(label = foo$xlab$label,
                      gp = gpar(col = foo$xlab$col,
                      fontsize = foo$fontsize.normal * foo$xlab$cex), 
                      vp = viewport(layout.pos.row = n.row - 2))
                                    
        
        if (have.ylab)
            grid.text(label = foo$ylab$label, rot = 90, 
                      gp = gpar(col = foo$ylab$col,
                      fontsize = foo$fontsize.normal * foo$ylab$cex),
                      vp = viewport(layout.pos.col = 2))
        
        
        push.viewport(viewport(layout.pos.row = 4,
                               layout.pos.col = 4,
                               xscale = foo$x.scales$limits,
                               yscale=foo$y.scales$limits))
        
        
        do.call("panel" , c(foo$panel.args, foo$panel.args.common))
        
        
        ## Y-axis to the left
        if (foo$y.draw) {
            axs <- foo$y.scales
            for(tt in seq(along=axs$at))
                if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                {
                    
                    grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                               x = unit(c(0,-.25), "lines"),
                               gp = gpar(col = yaxis.col))
                    
                    if (foo$y.alternating==1)
                        grid.text(label = axs$label[tt],
                                  x = unit(-.5, "lines"),
                                  y = unit(axs$at[tt], "native"),
                                  just = c("right", "centre"),
                                  rot = yaxis.rot, 
                                  gp = gpar(col = yaxis.col, 
                                  fontsize = foo$fontsize.normal * yaxis.cex))
                }
            ## Y-axis to the right
            axs <- foo$y.scales
            for(tt in seq(along=axs$at))
                if (axs$at[tt]>axs$limits[1] && axs$at[tt]<axs$limits[2])
                {
                    
                    grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                               x = unit(c(1,1),"npc")+unit(c(0,.25),"lines"),
                               gp = gpar(col = yaxis.col))
                    
                    if (foo$y.alternating==2)
                        grid.text(label = axs$label[tt],
                                  x = unit(1,"npc") + unit(.5, "lines"),
                                  y = unit(axs$at[tt], "native"),
                                  just = c("left", "centre"),
                                  rot = yaxis.rot, 
                                  gp = gpar(col = yaxis.col,
                                  fontsize = foo$fontsize.normal * yaxis.cex))
                    
                }
        }
        
        
        if (foo$x.draw) {
            ## X-axis to the bottom
            axs <- foo$x.scales
            for(tt in seq(along=axs$at))
                if (axs$at[tt]>axs$limits[1] && axs$at[tt]<axs$limits[2])
                {
                    
                    grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                               y = unit(c(0,-.25), "lines"),
                               gp = gpar(col = xaxis.col))
                    
                    if (foo$x.alternating==1)
                        grid.text(label = axs$label[tt],
                                  y = unit(-.5, "lines"),
                                  x = unit(axs$at[tt], "native"),
                                  just = c("centre", "right"),
                                  rot = xaxis.rot, 
                                  gp = gpar(col = xaxis.col,
                                  fontsize = foo$fontsize.normal * xaxis.cex))
                    
                }
            ## X-axis at top
            axs <- foo$x.scales
            for(tt in seq(along=axs$at))
                if (axs$at[tt]>axs$limits[1] && axs$at[tt]<axs$limits[2])
                {
                    
                    grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                               y = unit(c(1,1),"npc")+unit(c(0,.25),"lines"),
                               gp = gpar(col = xaxis.col))
                    
                    if (foo$x.alternating==2)
                        grid.text(label = axs$label[tt],
                                  y = unit(1,"npc") + unit(.5, "lines"),
                                  x = unit(axs$at[tt], "native"),
                                  just = c("centre", "left"),
                                  rot = xaxis.rot, 
                                  gp = gpar(col = xaxis.col,
                                  fontsize = foo$fontsize.normal * xaxis.cex))
                }
        }
        grid.rect()
        
        
        
    } else { # when there are conditioning variables
        
        cond.current.level <- rep(1,number.of.cond)
        panel.number <- 1
        
        for(page.number in 1:number.of.pages)
            if(!any(cond.max.level-cond.current.level<0)) {
                
                if(usual) grid.newpage()
                
                push.viewport(viewport(layout = page.layout,
                                       gp = gpar(fontsize =
                                       foo$fontsize.normal)))


                if (have.main)
                    grid.text(label = foo$main$label,
                              gp = gpar(col = foo$main$col,
                              fontsize = foo$fontsize.normal * foo$main$cex),
                              vp = viewport(layout.pos.row = 1))
                
                
                if (have.sub)
                    grid.text(label = foo$sub$label,
                              gp = gpar(col = foo$sub$col,
                              fontsize = foo$fontsize.normal * foo$sub$cex),
                              vp = viewport(layout.pos.row = n.row))
                
                
                if (have.xlab)
                    grid.text(label = foo$xlab$label,
                              gp = gpar(col = foo$xlab$col,
                              fontsize = foo$fontsize.normal * foo$xlab$cex), 
                              vp = viewport(layout.pos.row = n.row - 2))
                
                
                if (have.ylab)
                    grid.text(label = foo$ylab$label, rot = 90,
                              gp = gpar(col = foo$ylab$col,
                              fontsize = foo$fontsize.normal * foo$ylab$cex),
                              vp = viewport(layout.pos.col = 2))
                
                
                for(row in 1:rows.per.page)
                    for(column in 1:cols.per.page)
                        
                        if(!any(cond.max.level-cond.current.level<0)) {
                            
                            if(is.logical(foo$panel.args[[panel.number]]))
                                ## corr to skip = T
                                panel.number <- panel.number + 1
                            
                            else {
                                
                                actual.row <- if (foo$as.table)
                                    (rows.per.page-row+1) else row
                                ## this gives the row position from the bottom
                                pos.row <- (rows.per.page-actual.row) *
                                    (number.of.cond+3)+4+number.of.cond
                                pos.col <- (column-1)*3 + 5
                                
                                xscale <-
                                    if(foo$x.relation.same)
                                        foo$x.scales$limits
                                    else foo$x.scales[[panel.number]]$limits
                                yscale <- 
                                    if(foo$y.relation.same)
                                        foo$y.scales$limits
                                    else foo$y.scales[[panel.number]]$limits
                                
                                
                                push.viewport(viewport(layout.pos.row = pos.row,
                                                       layout.pos.col = pos.col,
                                                       xscale = xscale,
                                                       yscale = yscale,
                                                       gp = gpar(fontsize =
                                                       foo$fontsize.normal)))
                                
                                do.call("panel", c(foo$panel.args[[panel.number]],
                                                   foo$panel.args.common))
                                

                                ## next few lines deal with drawing axes
                                ## as appropriate
                                ## when relation != same, axes drawn for
                                ## each panel:
                                
                                ## X-axis
                                if(!foo$x.relation.same && foo$x.draw) {
                                    
                                    axs <- foo$x.scales[[panel.number]]
                                    if (is.logical(axs$at)) axs$at <-
                                        lpretty(xscale, n=axs$tick.number)
                                    if (is.logical(axs$labels)) axs$labels <-
                                        as.character(axs$at)
                                    for(tt in seq(along=axs$at)) 
                                        if (axs$at[tt]>=xscale[1] && axs$at[tt]<=xscale[2])
                                        { 
                                            grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                                                       y = unit(c(0,-.25), "lines"),
                                                       gp = gpar(col = xaxis.col))
                                            
                                            grid.text(label = axs$labels[tt],
                                                      y = unit(-0.5, "lines"),
                                                      x = unit(axs$at[tt], "native"),
                                                      just = if (xaxis.rot == 0) c("centre", "right")
                                                      else c("right", "centre"),
                                                      rot = xaxis.rot, 
                                                      gp = gpar(col = xaxis.col,
                                                      fontsize = foo$fontsize.normal * xaxis.cex))
                                            
                                            
                                        }
                                }
                                ## Y-axis
                                if(!foo$y.relation.same && foo$y.draw) {
                                    
                                    axs <- foo$y.scales[[panel.number]]
                                    if (is.logical(axs$at)) axs$at <- lpretty(yscale, n=axs$tick.number)
                                    if (is.logical(axs$labels)) axs$labels <- as.character(axs$at)
                                    for(tt in seq(along=axs$at))
                                        if (axs$at[tt]>=yscale[1] && axs$at[tt]<=yscale[2])
                                        {
                                            
                                            grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                                                       x = unit(c(0,-.25), "lines"),
                                                       gp = gpar(col = yaxis.col))
                                            
                                            grid.text(label = axs$label[tt],
                                                      x = unit(-.5, "lines"),
                                                      y = unit(axs$at[tt], "native"),
                                                      just = if (yaxis.rot == 90) c("centre", "left")
                                                      else c("right", "centre"),
                                                      rot = yaxis.rot, 
                                                      gp = gpar(col = yaxis.col,
                                                      fontsize = foo$fontsize.normal * yaxis.cex))
                                            
                                        }
                                    
                                }
                                
                                
                                ## When relation = same, axes drawn based on value of alternating
                                if (foo$y.relation.same && foo$y.draw) {
                                    
                                    ## Y-axis to the left
                                    if (column == 1) {
                                        axs <- foo$y.scales
                                        for(tt in seq(along=axs$at))
                                            if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                            {
                                                
                                                grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                                                           x = unit(c(0,-.25), "lines"),
                                                           gp = gpar(col = yaxis.col))
                                                
                                                if (foo$y.alternating[actual.row]==1)
                                                    grid.text(label = axs$label[tt],
                                                              x = unit(-.5, "lines"),
                                                              y = unit(axs$at[tt], "native"),
                                                              just = if (yaxis.rot != 90) c("right", "centre")
                                                              else c("centre", "left"),
                                                              rot = yaxis.rot, 
                                                              gp =gpar(col = yaxis.col,
                                                              fontsize = foo$fontsize.normal * yaxis.cex))
                                                
                                            }
                                    }
                                    
                                    ## Y-axis to the right
                                    if (column == cols.per.page) {
                                        axs <- foo$y.scales
                                        for(tt in seq(along=axs$at))
                                            if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                            {
                                                
                                                grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                                                           x = unit(c(1,1), "npc") + unit(c(0,.25), "lines"),
                                                           gp = gpar(col = yaxis.col))
                                                
                                                if (foo$y.alternating[actual.row]==2)
                                                    grid.text(label = axs$label[tt],
                                                              x = unit(1,"npc") + unit(.5, "lines"),
                                                              y = unit(axs$at[tt], "native"),
                                                              just = if (yaxis.rot != 90) c("left", "centre")
                                                              else c("centre", "right"),
                                                              rot = yaxis.rot, 
                                                              gp = gpar(col = yaxis.col,
                                                              fontsize = foo$fontsize.normal * yaxis.cex))
                                                          
                                            }
                                    }
                                    
                                }
                                
                                
                                
                                ## X-axis to the bottom
                                if (foo$x.relation.same && foo$x.draw) {
                                    if (actual.row == 1) {
                                        axs <- foo$x.scales
                                        for(tt in seq(along=axs$at))
                                            if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                            {
                                                
                                                grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                                                           y = unit(c(0,-.25), "lines"),
                                                           gp = gpar(col = xaxis.col))
                                                
                                                if (foo$x.alternating[column]==1)
                                                    grid.text(label = axs$label[tt],
                                                              y = unit(-.5, "lines"),
                                                              x = unit(axs$at[tt], "native"),
                                                              just = if (xaxis.rot == 0) c("centre", "right")
                                                              else c("right", "centre"),
                                                              rot = xaxis.rot, 
                                                              gp = gpar(col = xaxis.col,
                                                              fontsize = foo$fontsize.normal * xaxis.cex))
                                                
                                            }
                                    }
                                    ## X-axis at the top is a bit more complicated as
                                    ## it has to be drawn on top of the strips.
                                    
                                }
                                
                                
                                
                                grid.rect()
                                
                                pop.viewport(current.viewport())
                                
                                if(!is.logical(foo$strip)) # logical ==> FALSE
                                    for(i in 1:number.of.cond)
                                    {
                                        push.viewport(viewport(layout.pos.row = pos.row-i,
                                                               layout.pos.col = pos.col,
                                                               gp = gpar(fontsize = foo$fontsize.normal)))
                                        
                                        grid.rect()
                                        
                                        foo$strip(foo$cond[[i]],
                                                  level = cond.current.level[i],
                                                  name = names(foo$cond)[i],
                                                  bg = strip.col.default.bg[i],
                                                  fg = strip.col.default.fg[i],
                                                  par.strip.text = foo$par.strip.text)
                                        
                                        pop.viewport(current.viewport())
                                        
                                    }
                                
                                
                                ## X-axis at top
                                if (foo$x.relation.same && foo$x.draw)
                                    if (actual.row == rows.per.page) {
                                        axs <- foo$x.scales
                                        push.viewport(viewport(layout.pos.row = pos.row -
                                                               if (is.logical(foo$strip)) 0 else number.of.cond,
                                                               layout.pos.col = pos.col,
                                                               xscale = xscale))
                                        
                                        
                                        for(tt in seq(along=axs$at))
                                            if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                            {
                                                
                                                grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                                                           y = unit(c(1,1), "npc") + unit(c(0,.25), "lines"),
                                                           gp = gpar(col = xaxis.col))
                                                
                                                if (foo$x.alternating[column]==2)
                                                    grid.text(label = axs$label[tt],
                                                              y = unit(1,"npc") + unit(.5, "lines"),
                                                              x = unit(axs$at[tt], "native"),
                                                              just = if (xaxis.rot == 0) c("centre","left")
                                                              else c("left", "centre"),
                                                              rot = xaxis.rot, 
                                                              gp = gpar(col = xaxis.col,
                                                              fontsize = foo$fontsize.normal * xaxis.cex))
                                                
                                            }
                                        
                                        pop.viewport(current.viewport())
                                    }
                                
                                
                                
                                
                                cond.current.level <- cupdate(cond.current.level,
                                                              cond.max.level)
                                panel.number <- panel.number + 1
                                
                            }
                            
                        }
                if(!is.null(foo$page)) foo$page(page.number)
                
            }
        
    }
    


    if (!missing(position)) {

        if (!missing(split)) {
            pop.viewport(current.viewport())
        }


        pop.viewport(current.viewport())
        pop.viewport(current.viewport())
        pop.viewport(current.viewport())
    }

    else if (!missing(split)) {
        pop.viewport(current.viewport())
        pop.viewport(current.viewport())
        pop.viewport(current.viewport())
    }
    
    
}




