

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


dotplot <-
    function(formula,
             panel = panel.dotplot,
             data = sys.frame(sys.parent()),
             ...)
    bwplot(formula, data=data, panel=panel, box.ratio=1, ...)


barchart <-
    function(formula,
             panel = panel.barchart,
             data = sys.frame(sys.parent()),
             box.ratio = 2,
             ...)
    bwplot(formula, data=data, panel=panel, box.ratio = box.ratio, ...)




stripplot <-
    function(formula,
             panel = panel.stripplot,
             data = sys.frame(sys.parent()),
             jitter = T,
             factor = .5,
             box.ratio = 1,
             ...)
    bwplot(formula, data=data,
           panel=panel, box.ratio = 1,
           jitter.data = jitter, factor = factor,
           ...)


bwplot <-
    function(formula,
             aspect = "fill",
             as.table = FALSE,
             between = list(x=0,y=0),
             box.ratio = 1,
             data,
             groups = NULL,
             key = NULL,
             layout,
             main = NULL,
             page = NULL,
             panel = panel.bwplot,
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
    y <- form$left
    x <- form$right

    if(!missing(groups)) {
        groups <- eval(substitute(groups), data)
    }

    if("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if(subscripts) subscr <- seq(along=x)

    if(!missing(subset)) {   
        subset <- eval(substitute(subset), data)
        x <- x[subset]
        y <- ldrop.levels(y, subset)  # Note: y can be factor, char, num, shingle
        if (subscripts) subscr <- subscr[subset]
    }

    if(!(is.numeric(x))) stop("x must be numeric")
    if (! (is.shingle(y) || is.factor(y)) )
    {
        if (is.numeric(y)) y <- shingle(y)
        else if(is.character(y)) y <- factor(y, levels = unique(y))
        else stop("y must be factor, shingle, character or numeric")
    }

    is.f.y <- is.factor(y)  # used throughout the rest of the code

    if(missing(xlab)) xlab <- form$right.name
    if(missing(ylab)) ylab <-
        if (is.f.y) NULL
        else form$left.name

    if(!missing(between)) {
        if (!("x" %in% names(between))) between <- list(x=0, y=between$y)
        if (!("y" %in% names(between))) between <- list(y=0, x=between$x)
    }

    id.na <- (is.na(x)|
              if (is.f.y) is.na(y) else is.na(y$x))


    foo <- list(formula=formula,
                fname = "bwplot-dtplot-barchart-stripplot",
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

        if (ylim.specified) warning("explicitly specifying y limits is not advisable")

        if (missing(xlim)) xlim <- range(x[is.finite(x[!id.na])])
        temp <- .5*(box.ratio+2)/(box.ratio+1)
        num.l.y <-
            if (is.f.y) length(levels(y))
            else nrow(y$intervals)
        if (missing(ylim)) ylim <- c(1-temp, num.l.y + temp)

        ## will add 7% by extend.limits() after updating this
        ## if necessary using the prepanel function


        scales.x <- list(relation = "same",
                         draw = TRUE,
                         alternating = c(1,2),
                         limits = xlim,
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         tick.number = 5,
                         rot = F,
                         at = F, labels = F, col = F, log = F)
        ## S-Plus documentation does not seem to specify all possible values in
        ## the list scales. These are all I've been able to figure out. `rot'
        ## was added by me, though.
        ## For y-scale, rot defaults to 0, except in the particular case when
        ## relation != "same" AND text labels are specified (S+ behaviour)

        ## I'm not sure if this is the best way to handle scales, but I
        ## couldn't think of a better one.
        
        scales.y <- list(relation = "same",
                         draw = TRUE,
                         alternating = 1,
                         limits = ylim,
                         tck = 1,
                         cex = 1,
                         tick.number = num.l.y,
                         rot = 0, # for these functions (bwplot, dotplot, stripplot, barchart)
                         at = 1:num.l.y,    # y-rot should always be 0
                         labels = if(is.f.y) levels(y) else as.character(1:num.l.y),
                         col = F, log = F)              

        if(!missing(scales)) {
            scales.x[names(scales)] <- scales
            scales.y[names(scales)] <- scales
            if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
        }


        ## Need to go through the panels before the actual plotting if
        ## (1) !missing(prepanel) OR
        ## (2) relation="sliced" OR
        ## (3) aspect="xy"  :

        
        xlim.l <- xlim[1]
        xlim.u <- xlim[2]
        ylim.l <- ylim[1]
        ylim.u <- ylim[2]
        x.slice.length <- 0
        y.slice.length <- 0
        


        aspr <- rep(1,nplots)
        if (!missing(prepanel) || scales.x$relation=="sliced" ||
            scales.y$relation=="sliced" || aspect == "xy")
        {
            warning("are you sure you want to do this ?")
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

                        if (is.f.y) {
                            panel.x <- x[id]
                            panel.y <- as.numeric(y[id])
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            for (k in 1:num.l.y) {
                                tid <- id & (y$x >= y$int[k,1]) & (y$x <= y$int[k,2])
                                panel.x <- c(panel.x, x[tid])
                                panel.y <- c(panel.y, rep(k,length(tid[tid])))
                            }
                        }
                        

                        tem <- list(xlim = range(panel.x),
                                    ylim = range(panel.y),
                                    dx = diff(range(panel.x)),
                                    dy = diff(range(panel.y)))
                        
                        if(!missing(prepanel)) {
                            tem1 <- prepanel(x=x[id],y=y[id])
                            tem[names(tem1)] <- tem1
                        }
                        
                        xlim.l <- min(xlim.l, tem$xlim[1])
                        xlim.u <- max(xlim.u, tem$xlim[2])
                        ## ylim.l <- min(ylim.l, tem$ylim[1])
                        ## ylim.u <- max(ylim.u, tem$ylim[2])
                        x.slice.length <- max(x.slice.length, diff(tem$xlim))
                        ## y.slice.length <- max(y.slice.length, diff(tem$ylim))
                        
                        aspr[count] <- banking(tem$dx, tem$dy)
                    }
                    
                    
                    cond.current.level <- cupdate(cond.current.level,
                                                  cond.max.level)
                    
                }
            
            
        }
        

        ## check whether xlim / scales$limits was specified, don't change in that case,
        ## unless, of course,  relation="sliced" or "free"  (see xlim.specified etc above)
        if (scales.x$relation == "same" && !xlim.specified)
            scales.x$limits <- extend.limits(c(xlim.l,xlim.u))
        else x.slice.length <- x.slice.length * 1.14
        ## this is actually irrelevant for relation="free"
        ## All this doesn't make sense for ylim, so...
        scales.y$limits <- ylim
        

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
            else if (aspect == "xy") {
                warning("behaviour for aspect=xy undefined")
                median(aspr)
            }                           # not banking, just fooling around
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
        if (aspect=="xy") warning("behaviour for aspect=xy undefined")
        
        id <- !id.na
        if(missing(xlim)) xlim <- extend.limits(range(x[is.finite(x[id])]))
        if(missing(ylim)) {
            temp <- .5*(box.ratio+2)/(box.ratio+1)
            num.l.y <- if (is.f.y) length(levels(y)) else nrow(y$intervals)
            ylim <- c(1-temp, num.l.y + temp)
        }
        

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
                         rot = 0,
                         tick.number = 5,
                         at = 1:num.l.y,
                         labels = if (is.f.y) levels(y) else as.character(1:num.l.y),
                         col = F, log = F)
        
        if(!missing(scales)) {
            scales.x[names(scales)] <- scales
            scales.y[names(scales)] <- scales
            if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
        }
        

        if (!missing(prepanel) || aspect == "xy")
        {
            warning("are you sure you want to do this ?")

            id <- !id.na
            if(any(id)) {
                if (is.f.y) {
                    panel.x <- x[id]
                    panel.y <- as.numeric(y[id])
                }
                else {  # shingle
                    panel.x <- numeric(0)
                    panel.y <- numeric(0)
                    for (k in 1:num.l.y) {
                        tid <- id & (y$x >= y$int[k,1]) & (y$x <= y$int[k,2])
                        panel.x <- c(panel.x, x[tid])
                        panel.y <- c(panel.y, rep(k,length(tid[tid])))
                    }
                }


                
                tem <- list(xlim = range(panel.x),
                            ylim = range(panel.y),
                            dx = diff(range(panel.x)),
                            dy = diff(range(panel.y)))
                
                if(!missing(prepanel)) {
                    tem1 <- prepanel(x=x[id],y=y[id])
                    tem[names(tem1)] <- tem1
                }
                    
                ## banking computations should be put here ?
                
                scales.x$limits <- range(scales.x$limits, tem$xlim)
                ##  scales.y$limits <- range(scales.y$limits, tem$ylim)
                ## doesn't make sense for bwplot etc.
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
        if(subscripts) list(groups = groups, box.ratio = box.ratio, ...)
        else list(box.ratio = box.ratio, ...)
    
    if(number.of.cond<1)
        if (any(id)) {

            if (is.f.y) {
                panel.x <- x[id]
                panel.y <- as.numeric(y[id])
                if (subscripts) panel.subscr <- subscr[id]
            }
            else {
                panel.x <- numeric(0)
                panel.y <- numeric(0)
                if (subscripts) panel.subscr <- numeric(0)
                for (k in 1:num.l.y) {
                    tid <- id & (y$x >= y$int[k,1]) & (y$x <= y$int[k,2])
                    panel.x <- c(panel.x, x[tid])
                    panel.y <- c(panel.y, rep(k,length(tid[tid])))
                    if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                }
            }
            
            foo$panel.args <-
                if(subscripts) list(x=panel.x, y=panel.y, 
                                    subscripts=panel.subscr)
                else list(x = panel.x, y = panel.y)
            

        }
        else foo$panel.args <- 
            if(subscripts) list(x=numeric(0),y=numeric(0), subscripts=numeric(0))
            else list(x=numeric(0),y=numeric(0)) 
    
    
    else {

        npanels <- plots.per.page * layout[3]
                                        # upper bound for number of panels
        foo$panel.args <- list(1:nplots)
        cond.current.level <- rep(1,number.of.cond)

        if (!foo$x.relation.same)           # this means that axes will be different 
            foo$x.scales <- list(1:nplots)  # for EACH panel and consequently,
        if (!foo$y.relation.same)           # foo$x.scales must be a list parallel to
            foo$y.scales <- list(1:nplots)  # foo$panel.args.
        
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



                        xscale <- 
                            if(scales.x$relation == "same") scales.x$limits
                            else if (scales.x$relation == "sliced")
                            {
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id],y=y[id])$xlim
                                                  else scales.x$limits,
                                                  length=x.slice.length)
                                
                                else extend.limits(
                                                   if (any(id)) range(x[id])
                                                   else scales.x$limits,
                                                   length=x.slice.length)
                                
                            }
                            else #if relation=free
                            { 
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=x[id],y=y[id])$xlim
                                                  else scales.x$limits)
                                
                                else extend.limits(
                                                   if (any(id)) range(x[id])
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

                        
                        if (is.f.y) {
                            panel.x <- x[id]
                            panel.y <- as.numeric(y[id])
                            if (subscripts) panel.subscr <- subscr[id]
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            if (subscripts) panel.subscr <- numeric(0)
                            for (k in 1:num.l.y) {
                                tid <- id & (y$x >= y$int[k,1]) & (y$x <= y$int[k,2])
                                panel.x <- c(panel.x, x[tid])
                                panel.y <- c(panel.y, rep(k,length(tid[tid])))
                                if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                            }
                        }
                        
                        
                        
                        yscale <- 
                            if(scales.y$relation == "same") scales.y$limits
                            else if (scales.y$relation == "sliced") {
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=panel.x,
                                                                        y=panel.y)$ylim
                                                  else scales.y$limits,
                                                  length=y.slice.length)
                                
                                else extend.limits(
                                                   if (any(id)) range(panel.y)
                                                   else scales.y$limits,
                                                   length=y.slice.length)
                            }
                            else  #if relation=free
                            {
                                if(!missing(prepanel))
                                    extend.limits(
                                                  if (any(id)) prepanel(x=panel.x,
                                                                        y=panel.y)$ylim
                                                  else scales.y$limits)
                                                  
                                else extend.limits(
                                                   if (any(id)) range(panel.y)
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
                        

                        id <- ((panel.x>=xscale[1]) & (panel.x<=xscale[2]) &
                               (panel.y>=yscale[1]) & (panel.y<=yscale[2]))
                        ## This is a new id, the old one is not needed any more

                        foo$panel.args[[panel.number]] <-                             
                            if(subscripts) list(x=panel.x[id], y=panel.y[id],
                                                subscripts=panel.subscr[id])
                            else list(x = panel.x[id], y = panel.y[id])
                        
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
