

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


tmd <-
    function(object,
             aspect = "fill",
             as.table = object$as.table,
             between = list(x=object$x.between,y=object$y.between),
             key = object$key,
             layout = object$layout,
             main = object$main,
             page = object$page,
             panel = panel.tmd,
             par.strip.text, 
             prepanel,
             scales,
             strip = object$strip,
             sub = object$sub,
             xlab = "mean",
             xlim,
             ylab = "difference",
             ylim,
             ...)
{

    if (is.logical(strip) && strip) strip <- strip.default

    if(!missing(between)) {
        if (!("x" %in% names(between))) between <- list(x=0, y=between$y)
        if (!("y" %in% names(between))) between <- list(y=0, x=between$x)
    }

    foo <- list(fname = "tmd",
                aspect.fill = (aspect=="fill"),
                aspect.ratio = 1,
                as.table = as.table,
                cond = object$cond,
                key = key,
                layout=layout,
                page = page,
                panel = panel,
                panel.args = object$panel.args,
                panel.args.common = object$panel.args.commmon,
                par.strip.text = trellis.par.get("add.text"),
                strip = strip,
                main = NULL,
                sub = NULL,
                xlab = NULL,
                ylab = NULL,
                x.draw = TRUE,
                y.draw = TRUE,
                x.scales = NULL,
                y.scales = NULL,
                x.between = between$x,
                y.between = between$y,
                x.relation.same = TRUE,  # Note: needed even when
                y.relation.same = TRUE,  # number.of.cond == 0
                x.alternating = c(1,2),
                y.alternating = c(1,2),
                fontsize.normal = 10,
                fontsize.small = 8)


    if(!missing(par.strip.text))
        foo$par.strip.text[names(par.strip.text)] <- par.strip.text

    if (!missing(main)) {
        foo$main <- list(label = main[[1]], col = "black", cex = 1, font = "unimplemented")
        if (is.list(main)) foo$main[names(main)] <- main
    }
    
    if (!missing(sub)) {
        foo$sub <- list(label = sub[[1]], col = "black", cex = 1, font = "unimplemented")
        if (is.list(sub)) foo$sub[names(sub)] <- sub
    }
    
    if(!is.null(xlab)) {
        foo$xlab <- list(label = xlab[[1]], col = "black", cex = 1, font = "unimplemented")
        if (is.list(xlab)) foo$xlab[names(xlab)] <- xlab
        if (!is.character(foo$xlab$label)) foo$xlab$label <- form$right.name
    }
    
    if(!is.null(ylab)) {
        foo$ylab <- list(label = ylab[[1]], col = "black", cex = 1, font = "unimplemented")
        if (is.list(ylab)) foo$ylab[names(ylab)] <- ylab
        if (!is.character(foo$ylab$label)) foo$ylab$label <- form$left.name
    }
    

    

    xlim.specified <-
        (!missing(xlim) ||
         ( !missing(scales) &&
          ("limits" %in% names(scales) ||
           ("x" %in% names(scales) && "limits" %in% names(scales$x)))))
    

    ylim.specified <-
        (!missing(ylim) ||
         ( !missing(scales) &&
          ("limits" %in% names(scales) ||
           ("y" %in% names(scales) && "limits" %in% names(scales$y)))))
    
    
    if(missing(xlim)) xlim <- numeric(0)
    if(missing(ylim)) ylim <- numeric(0)
    
    
    scales.x <- list(relation = "same",
                     draw = TRUE,
                     alternating = c(1,2),
                     limits = xlim,
                     tck = 1, # factor affecting length of ticks 
                     cex = 1,
                     tick.number = 5,
                     rot = F,
                     at = F, labels = F, col = F, log = F)
    
    
    scales.y <- list(relation = "same",
                     draw = TRUE,
                     alternating = c(1,2),
                     limits = ylim,
                     tck = 1,
                     cex = 1,
                     tick.number = 5,
                     rot = F,  
                     at = F, labels = F, col = F, log = F) 
    
    if(!missing(scales)) {
        scales.x[names(scales)] <- scales
        scales.y[names(scales)] <- scales
        if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
    }
    
    
    ## Need to go through the panels before the actual plotting
    
    xlim.l <- if (length(xlim)==2) xlim[1] else Inf
    xlim.u <- if (length(xlim)==2) xlim[2] else -Inf
    ylim.l <- if (length(ylim)==2) ylim[1] else Inf
    ylim.u <- if (length(ylim)==2) ylim[2] else -Inf
    x.slice.length <- 0
    y.slice.length <- 0
    
    
    aspr <- numeric(0)
    count <- 1
    
    if("x" %in% names(foo$panel.args)) { # which means no conditioning
        x <- (foo$panel.args$x+foo$panel.args$y)/2
        y <- foo$panel.args$y-foo$panel.args$x
        ## will stop if any errors, not putting any more handlers
            
        foo$panel.args$x <- x
        foo$panel.args$y <- y
        
        if(length(x)>0) {
                    
            ord <- order(x)
            tem <- list(xlim = range(x),
                        ylim = range(y),
                        dx = diff(x[ord]),
                        dy = diff(y[ord]))
            
            if(!missing(prepanel)) {
                tem1 <- prepanel(x=x,y=y)
                tem[names(tem1)] <- tem1
                }
            
            
            if(missing(xlim)) xlim <- range(xlim, x)
            if(missing(ylim)) ylim <- range(ylim, y)
            xlim.l <- min(xlim.l, tem$xlim[1])
            xlim.u <- max(xlim.u, tem$xlim[2])
            ylim.l <- min(ylim.l, tem$ylim[1])
            ylim.u <- max(ylim.u, tem$ylim[2])
            x.slice.length <- max(x.slice.length, diff(tem$xlim))
            y.slice.length <- max(y.slice.length, diff(tem$ylim))
            
            aspr <- c(aspr, banking(tem$dx, tem$dy))
            }
        
    }
    else for (p in foo$panel.args)
        if (is.logical(p)) # which means skip = T for this panel
            count <- count + 1 
        else {
            
            x <- (p$x+p$y)/2
            y <- p$y-p$x       # will stop if any errors, not putting any more handlers

            foo$panel.args[[count]]$x <- x
            foo$panel.args[[count]]$y <- y

            if(length(x)>0) {
                
                ord <- order(x)
                tem <- list(xlim = range(x),
                            ylim = range(y),
                            dx = diff(x[ord]),
                            dy = diff(y[ord]))
                
                if(!missing(prepanel)) {
                    tem1 <- prepanel(x=x,y=y)
                    tem[names(tem1)] <- tem1
                }

                if(missing(xlim)) xlim <- range(xlim, x)
                if(missing(ylim)) ylim <- range(ylim, y)
                xlim.l <- min(xlim.l, tem$xlim[1])
                xlim.u <- max(xlim.u, tem$xlim[2])
                ylim.l <- min(ylim.l, tem$ylim[1])
                ylim.u <- max(ylim.u, tem$ylim[2])
                x.slice.length <- max(x.slice.length, diff(tem$xlim))
                y.slice.length <- max(y.slice.length, diff(tem$ylim))
                
                aspr <- c(aspr, banking(tem$dx, tem$dy))
            }

            count <- count + 1
            
        }
    

    ## check whether xlim / scales$limits was specified, don't change in that case,
    ## unless, of course,  relation="sliced" or "free"  (see xlim.specified etc above)
    if (scales.x$relation == "same" && !xlim.specified)
        scales.x$limits <- extend.limits(c(xlim.l,xlim.u))
    else x.slice.length <- x.slice.length * 1.14
    ## this is actually irrelevant for relation="free" 
        if (scales.y$relation == "same" && !ylim.specified)
            scales.y$limits <- extend.limits(c(ylim.l,ylim.u))
        else y.slice.length <- y.slice.length * 1.14
   
    foo$x.relation.same <- (scales.x$relation == "same")
    foo$y.relation.same <- (scales.y$relation == "same")
    
    if(foo$x.relation.same)
        foo$x.scales <- list(limits = scales.x$limits,
                             at = scales.x$at,
                             labels = scales.x$labels,
                             tck = scales.x$tck,
                             col = scales.x$col,
                             cex = scales.x$cex,
                             rot = scales.x$rot,
                             tick.number = scales.x$tick.number)
    
    if(foo$y.relation.same)
        foo$y.scales <- list(limits = scales.y$limits,
                             at = scales.y$at,
                             labels = scales.y$labels,
                             tck = scales.y$tck,
                             col = scales.y$col,
                             cex = scales.y$cex,
                             rot = scales.y$rot,
                             tick.number = scales.y$tick.number)
    
    ## aspect.fill <- if (aspect=="fill") TRUE else FALSE (done above)
    foo$aspect.ratio <-
        if (aspect == "fill") 1
        else if (aspect == "xy") median(aspr) * 
            (if (scales.y$relation == "sliced") slice.y.length else diff(scales.y$limits)) / 
                (if (scales.x$relation == "sliced") slice.x.length else diff(scales.x$limits)) 
        else aspect[1]
    
    if (length(layout)!=3) stop("layout, if specified (disouraged), must be of length 3")
    
    foo$x.alternating <- scales.x$alternating
    foo$y.alternating <- scales.y$alternating
    
    if (is.logical(scales.x$alternating))
        scales.x$alternating <- if (scales.x$alternating) c(1,2) else 1
    
    if (is.logical(scales.y$alternating))
        scales.y$alternating <- if (scales.y$alternating) c(1,2) else 1
    
    foo$x.alternating <- scales.x$alternating
    foo$y.alternating <- scales.y$alternating
    foo$x.draw <- scales.x$draw
    foo$y.draw <- scales.y$draw
    


    if(!(foo$x.relation.same && foo$y.relation.same)) {

        foo$x.scales <- as.list(1: (count-1))
        count <- 1
        for (p in foo$panel.args)
            if (is.logical(p)) # which means skip = T for this panel
                count <- count + 1 
            else {
                
            x <- p$x
            y <- p$y
                
            if(length(x)>0) {

                tem <- list(xlim = range(x),
                            ylim = range(y),
                            dx = diff(x),
                            dy = diff(y))
                
                if(!missing(prepanel)) {
                    tem1 <- prepanel(x=x,y=y)
                    tem[names(tem1)] <- tem1
                }
            }
            else tem <- list(xlim = xlim, ylim = ylim)

            if(!foo$x.relation.same) 
                foo$x.scales[[count]] <-
                    list(limits =
                         if (scales.x$relation == "sliced")
                         extend.limits(tem$xlim, length = x.slice.length)
                         else extend.limits(tem$xlim),
                         at = scales.x$at,
                         labels = scales.x$labels,
                         tck = scales.x$tck,
                         col = scales.x$col,
                         cex = scales.x$cex,
                         rot = scales.x$rot,
                         tick.number = scales.x$tick.number)
            
            if(!foo$y.relation.same) 
                foo$y.scales[[count]] <-
                    list(limits =
                         if (scales.y$relation == "sliced")
                         extend.limits(tem$ylim, length = y.slice.length)
                         else extend.limits(tem$ylim),
                         at = scales.y$at,
                         labels = scales.y$labels,
                         tck = scales.y$tck,
                         col = scales.y$col,
                         cex = scales.y$cex,
                         rot = scales.y$rot,
                         tick.number = scales.y$tick.number)
            

        }
        
    }
    
    class(foo) <- "trellis"
    foo
}


