



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


qqmath <-
    function(formula,
             f.value = ppoints,
             distribution = qnorm, 
             aspect = "fill",
             as.table = FALSE,
             between = list(x=0,y=0),
             data,
             groups = NULL,
             key = NULL,
             layout,
             main = NULL,
             page = NULL,
             panel = panel.xyplot,
             par.strip.text,
             prepanel,
             scales,
             skip = FALSE,
             strip = strip.default,
             sub = NULL,
             subscripts = !missing(groups),
             subset,
             xlab,
             xlim,
             ylab,
             ylim,
             ...)
{


    if(all(skip)) stop("can't skip all panels !")

    if (is.logical(strip) && strip) strip <- strip.default

    if(missing(data)) data <- sys.frame(sys.parent())
    form <- latticeParseFormula(formula, data)
    cond <- form$condition
    number.of.cond <- length(cond)
    x <- form$right
    
    if(!missing(groups)) {
        groups <- eval(substitute(groups), data)
    }
    
    if("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if(subscripts) subscr <- seq(along=x)

    if(!missing(subset)) {                          # not tested yet
        subset <- eval(substitute(subset), data)
        x <- ldrop.levels(x,subset)
        if (subscripts) subscr <- subscr[subset]
    }
    if(missing(ylab)) ylab <- form$right.name
    if(missing(xlab)) xlab <- "Theoretical Distribution"
    if (is.shingle(x)) stop("x cannot be a shingle")

    if(!missing(between)) {
        if (!("x" %in% names(between))) between <- list(x=0, y=between$y)
        if (!("y" %in% names(between))) between <- list(y=0, x=between$x)
    }

    id.na <- is.na(x)
    if (!any(!id.na)) stop("nothing to draw")

    foo <- list(formula=formula,
                fname = "qqmath",
                aspect.fill = (aspect=="fill"),
                aspect.ratio = 1,
                as.table = as.table,
                cond = NULL,
                key = key,
                layout=c(1,1,1),
                page = page,
                panel = panel,
                panel.args = NULL,
                panel.args.common = NULL,
                par.strip.text = trellis.par.get("add.text"),
                skip = skip,
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
    

    if(number.of.cond>0)
    {
        
        cond.current.level <- rep(1,number.of.cond)
        cond.max.level <- integer(number.of.cond)
                                        # creating the vector now,
                                        # exact values assigned later.
        for(i in 1:number.of.cond) {
            
            ## Processing the conditioning variables
            ## There are some subtle issues involving the subset argument here.
            
            if (is.character(cond[[i]])) cond[[i]] <- factor(cond[[i]])
            else if (!is.factor(cond[[i]])) cond[[i]] <- as.shingle(cond[[i]])
            
            if(!missing(subset)) cond[[i]] <- ldrop.levels(cond[[i]], subset)
            
            id.na <- id.na | is.na(
                                   if (is.shingle(cond[[i]])) cond[[i]]$x
                                   else cond[[i]]
                                   )
            cond.max.level[i] <-
                if (is.shingle(cond[[i]])) nrow(cond[[i]]$intervals)
                else length(levels(cond[[i]]))
        }

        foo$cond <- cond


        nplots <- 1
        for(i in 1:number.of.cond) nplots <- nplots * cond.max.level[i]


        ## the foll strange logical essentially says whether xlim and ylim
        ## were specified somehow in the call. Needed later.

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

        
        if(missing(ylim)) ylim <-
            range( x[is.finite(x[!id.na])])
        if(missing(xlim)) xlim <- c(0,0)
        
        ## will add 7% by extend.limits() after updating this
        ## if necessary using the prepanel function


        xlim.l <- xlim[1]
        xlim.u <- xlim[2]
        ylim.l <- ylim[1]
        ylim.u <- ylim[2]
        x.slice.length <- 0
        y.slice.length <- 0
        
        aspr <- rep(1,nplots)
        for(count in 1:nplots)
            if(!any(cond.max.level-cond.current.level<0)) # should be always TRUE, safety check
            {
                
                id <- !id.na
                for(i in 1:length(cond)) {
                    id <- id &
                    if (is.shingle(cond[[i]]))
                        ((cond[[i]]$x >=
                          cond[[i]]$intervals[cond.current.level[i], 1])
                         & (cond[[i]]$x <=
                            cond[[i]]$intervals[cond.current.level[i], 2]))
                    else (as.numeric(cond[[i]]) == cond.current.level[i])
                }
                
                
                if(any(id)) {
                    if(!missing(prepanel))
                        tem <- prepanel(x=x[id], distribution = distribution)
                    else tem <- list(xlim = range(distribution(f.value(length(x[id])))),
                                     ylim = range(quantile(x[id], f.value(length(x[id])))),
                                     dx = diff(distribution(f.value(length(x[id])))),
                                     dy = diff(quantile(x[id], f.value(length(x[id])))))
                        
                    xlim.l <- min(xlim.l, tem$xlim[1])
                    xlim.u <- max(xlim.u, tem$xlim[2])
                    ylim.l <- min(ylim.l, tem$ylim[1])
                    ylim.u <- max(ylim.u, tem$ylim[2])

                    x.slice.length <- max(x.slice.length, diff(tem$xlim))
                    y.slice.length <- max(y.slice.length, diff(tem$ylim))

                    aspr[count] <- banking(tem$dx, tem$dy)
                }
                    
                
                cond.current.level <- cupdate(cond.current.level,
                                              cond.max.level)
                
            }
        
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
        
        if(missing(layout)) {
            layout <- c(0,1,1)
            if (number.of.cond==1) layout[2] <- nplots
            else {
                layout[1] <- cond.max.level[1]
                layout[2] <- cond.max.level[2]
            }
            skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
            plots.per.page <- length(skip) - length(skip[skip])
            layout[3] <- 1 + ceiling(nplots/plots.per.page)
        }
        else if (length(layout)==1)
            stop("layout must have at least 2 elements")
        else if (length(layout)==2)
        {
            if(all(layout<1))
                stop("at least one element of layout must be positive")
            else if (layout[2]==0) stop("inadmissible value of layout")

            skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
            plots.per.page <- length(skip) - length(skip[skip])
            layout[3] <- 1 + ceiling(nplots/plots.per.page)
        }
        else if (length(layout)==3) {
            if(layout[1]<0||layout[2]<1||layout[3]<1)
                stop("invalid value for layout")
            
            skip <- rep(skip, length = layout[1] * layout[2])
        }

        plots.per.page <- max(layout[1] * layout[2], layout[2])
        number.of.pages <- layout[3]
        
        foo$layout[1] <- layout[1]
        foo$layout[2] <- layout[2]
        foo$layout[3] <- layout[3]
        foo$x.alternating <- scales.x$alternating
        foo$y.alternating <- scales.y$alternating
        
    }
    else
    {
        ##aspect.fill <- if (aspect=="fill") TRUE else FALSE
        id <- !id.na
        if(missing(xlim))
            xlim <-
                extend.limits(range(distribution(f.value(length(x[id])))))
        
        if(missing(ylim)) ylim <- extend.limits(range(x[is.finite(x) & id]))
        
        scales.x <- list(draw = TRUE,
                         alternating = 1,
                         limits = xlim,
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         rot = F,
                         tick.number = 5,
                         at = F, labels = F, col = F, log = F)
        
        scales.y <- list(draw = TRUE,
                         alternating = 1,
                         limits = ylim,
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         rot = F,
                         tick.number = 5,
                         at = F, labels = F, col = F, log = F)
        
        if(!missing(scales)) {
            scales.x[names(scales)] <- scales
            scales.y[names(scales)] <- scales
            if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
        }
        
        
        if (!missing(prepanel) || aspect == "xy")
        {
            
            id <- !id.na
            
            if(any(id)) {
                if(!missing(prepanel))
                    tem <- prepanel(x=x[id], distribution = distribution)
                else  # banking
                    tem <- list(xlim = range(distribution(f.value(length(x[id])))),
                                ylim = range(quantile(x[id], f.value(length(x[id])))),
                                dx = diff(distribution(f.value(length(x[id])))),
                                dy = diff(quantile(x[id], f.value(length(x[id])))))
                
                scales.x$limits <- range(scales.x$limits, tem$xlim)
                scales.y$limits <- range(scales.y$limits, tem$ylim)
                
                aspr <- banking(tem$dx, tem$dy)
                
            }
            
        }
        

        foo$aspect.ratio <-
            if (aspect == "fill") 1
            else if (aspect == "xy") aspr * diff(scales.y$limits) / diff(scales.x$limits)
            else aspect[1]
        
        foo$x.scales <- list(limits = scales.x$limits,
                             at = scales.x$at,
                             labels = scales.x$labels,
                             tck = scales.x$tck,
                             col = scales.x$col,
                             cex = scales.x$cex,
                             rot = scales.x$rot,
                             tick.number = scales.x$tick.number)

        foo$y.scales <- list(limits = scales.y$limits,
                             at = scales.y$at,
                             labels = scales.y$labels,
                             tck = scales.y$tck,
                             col = scales.y$col,
                             cex = scales.y$cex,
                             rot = scales.y$rot,
                             tick.number = scales.y$tick.number)

    }


    if (is.logical(scales.x$alternating))
        scales.x$alternating <- if (scales.x$alternating) c(1,2) else 1
    
    if (is.logical(scales.y$alternating))
        scales.y$alternating <- if (scales.y$alternating) c(1,2) else 1
    
    foo$x.alternating <- scales.x$alternating
    foo$y.alternating <- scales.y$alternating
    foo$x.draw <- scales.x$draw
    foo$y.draw <- scales.y$draw
    
    
    ## Now, to construct the argument list for each panel

    
    foo$panel.args.common <-
        if(subscripts) list(groups = groups, distribution = distribution, ...)
        else list(distribution = distribution, ...)
    
    if(number.of.cond<1) {

        foo$panel.args <- 
            if(subscripts) list(x = x[id],
                                subscripts=subscr[id])
        
            else list(x = distribution(f.value(length(x[id]))), 
                      y = quantile(x[id], f.value(length(x[id]))))

    }
    
    else {

        npanels <- plots.per.page * layout[3]
                                        # upper bound for number of panels
        foo$panel.args <- list(1:nplots)
        cond.current.level <- rep(1,number.of.cond)

        if (!foo$x.relation.same)           # this means that axes will be different for EACH panel
            foo$x.scales <- list(1:nplots)  # and consequently, foo$x.scales must be a list parallel to
        if (!foo$y.relation.same)           # foo$panel.args.
            foo$y.scales <- list(1:nplots)
        
        panel.number <- 1 # this is a counter for panel number
        
        for(page.number in 1:number.of.pages)
            if(!any(cond.max.level-cond.current.level<0)) {
                for(plot in 1:plots.per.page) {

                    if(skip[plot]) foo$panel.args[[panel.number]] <- FALSE
                    else if(!any(cond.max.level-cond.current.level<0)) {
                        
                        id <- !id.na
                        for(i in 1:length(cond)) {
                            id <- id &
                            if (is.shingle(cond[[i]]))
                                ((cond[[i]]$x >=
                                  cond[[i]]$intervals[cond.current.level[i], 1])
                                 & (cond[[i]]$x <=
                                    cond[[i]]$intervals[cond.current.level[i], 2]))
                            else (as.numeric(cond[[i]]) == cond.current.level[i])
                        }

                        tem <- list(x = distribution(f.value(length(x[id]))), 
                                    y = quantile(x[id], f.value(length(x[id]))))

                        xscale <- 
                            if(scales.x$relation == "same") scales.x$limits
                            else if (scales.x$relation == "sliced")
                            {
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id], distribution = distribution)$xlim
                                                  else scales.x$limits,
                                                  length=x.slice.length)
                                
                                else extend.limits(
                                                   if (any(id)) range(tem$x)
                                                   else scales.x$limits,
                                                   length=x.slice.length)
                                
                            }
                            else #if relation=free
                            { 
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id], distribution = distribution)$xlim
                                                  else scales.x$limits)
                                
                                else extend.limits(
                                                   if (any(id)) range(tem$x)
                                                   else scales.x$limits)
                                
                            }
                        
                        if(!foo$x.relation.same) 
                            foo$x.scales[[panel.number]] <-
                                list(limits = xscale,
                                     at = scales.x$at,
                                     labels = scales.x$labels,
                                     tck = scales.x$tck,
                                     col = scales.x$col,
                                     cex = scales.x$cex,
                                     rot = scales.x$rot,
                                     tick.number = scales.x$tick.number)
                        
                        
                        yscale <- 
                            if(scales.y$relation == "same") scales.y$limits
                            else if (scales.y$relation == "sliced") { # doesn't happen
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id], distribution = distribution)$ylim
                                                  else scales.y$limits,
                                                  length=y.slice.length)
                                
                                else extend.limits(
                                                   if (any(id)) range(tem$y)
                                                   else scales.y$limits,
                                                   length=y.slice.length)
                            }
                            else  #if relation=free
                            {
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id], distribution = distribution)$ylim
                                                  else scales.y$limits)
                                
                                else extend.limits(
                                                   if (any(id)) range(tem$y)
                                                   else scales.y$limits)
                            }
                                  
                                
                        if(!foo$y.relation.same) 
                            foo$y.scales[[panel.number]] <-
                                list(limits = yscale,
                                     at = scales.y$at,
                                     labels = scales.y$labels,
                                     tck = scales.y$tck,
                                     col = scales.y$col,
                                     cex = scales.y$cex,
                                     rot = scales.y$rot,
                                     tick.number = scales.y$tick.number)

                        ##id <- id & (x>=xscale[1]) & (x<=xscale[2]) & (y>=yscale[1]) & (y<=yscale[2])
                        
                        foo$panel.args[[panel.number]] <- 
                            if(subscripts) list(x = tem$x, y = tem$y, 
                                                subscripts=subscr[id])
                            else list(x = tem$x, y = tem$y)
                        
                        cond.current.level <- cupdate(cond.current.level,
                                                      cond.max.level)
                        
                    }
                    
                    panel.number <- panel.number + 1
                    
                }
            }
    }
    
    class(foo) <- "trellis"
    foo
}


