

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



parallel <-
    function(formula,
             aspect = "fill",
             as.table = FALSE,
             between = list(x=0.5,y=0.5),
             data,
             groups = NULL,
             key = NULL,
             layout,
             main = NULL,
             page = NULL,
             panel = panel.parallel,
             par.strip.text, 
             prepanel,
             scales,
             skip = FALSE,
             strip = strip.default,
             sub = NULL,
             subscripts = !missing(groups),
             subset,
             xlab = NULL,
             xlim,
             ylab = NULL,
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
    subscr <- seq(along=x[,1])

    if(!missing(subset)) {
        subset <- eval(substitute(subset), data)
        x <- x[subset,,drop=F]
        subscr <- subscr[subset]
    }

    if(!missing(between)) {
        if (!("x" %in% names(between))) between <- list(x=0, y=between$y)
        if (!("y" %in% names(between))) between <- list(y=0, x=between$x)
    }

    id.na <- F

    foo <- list(formula=formula,
                fname = "splom",
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
        if (!is.character(foo$xlab$label)) foo$xlab$label <- "Scatter Plot Matrix"
    }
    
    if(!is.null(ylab)) {
        foo$ylab <- list(label = ylab[[1]], col = "black", cex = 1, font = "unimplemented")
        if (is.list(ylab)) foo$ylab[names(ylab)] <- ylab
        if (!is.character(foo$ylab$label)) foo$ylab$label <- NULL
    }


    if(missing(xlim)) xlim <- extend.limits(c(0,1))
    if(missing(ylim)) ylim <- extend.limits(c(1,ncol(x)), prop = 0.03) 
    
    if(number.of.cond>0)
    {

        cond.current.level <- rep(1,number.of.cond)
        cond.max.level <- integer(number.of.cond)
                                        # creating the vector now,
                                        # exact values assigned later.
        for(i in 1:number.of.cond) {

            ## Processing the conditioning variables
            ## There are some issues involving the subset argument here.
            
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

        ## ignoring scales because it doesn't make much sense
        ## (may it makes some sense, but I'll ignore it anyway for now)
        ## (keeping the foll for future work)
        
        scales.x <- list(relation = "same",
                         limits = xlim,
                         draw = TRUE,
                         alternating = c(1,2),
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         tick.number = 5,
                         rot = 0,
                         at = c(0,1),
                         labels = c("Min", "Max"),
                         col = F, log = F)

        
        scales.y <- list(relation = "same",
                         limits = ylim,
                         draw = TRUE,
                         alternating = 1,
                         tck = 1,
                         cex = 1,
                         tick.number = 5,
                         rot = 0,  
                         at = 1:ncol(x),
                         labels = colnames(x),
                         col = F, log = F) 

        if(!missing(scales)) {
            scales.x[names(scales)] <- scales
            scales.y[names(scales)] <- scales
            if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
        }


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

        ## ^^ all probably unnecessary

        ## aspect.fill <- if (aspect=="fill") TRUE else FALSE (done above)
        foo$aspect.ratio <-
            if (aspect == "fill") 1
            else if (aspect == "xy") 1 
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
        foo$aspect.ratio <-
            if (aspect == "fill") 1
            else if (aspect == "xy") 1 
            else aspect[1]
        

        id <- !id.na


        scales.x <- list(draw = TRUE,
                         limits = xlim,
                         alternating = 1,
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         rot = 0,
                         tick.number = 5,
                         at = c(0,1),
                         labels = c("Min", "Max"),
                         col = F, log = F)

        scales.y <- list(draw = TRUE,
                         limits = ylim,
                         alternating = 1,
                         tck = 1, # factor affecting length of ticks 
                         cex = 1,
                         rot = 0,
                         tick.number = 5,
                         at = 1:ncol(x),
                         labels = colnames(x),
                         col = F, log = F)
        
        if(!missing(scales)) {
            scales.x[names(scales)] <- scales
            scales.y[names(scales)] <- scales
            if ("x" %in% names(scales)) scales.x[names(scales$x)] <- scales$x
            if ("y" %in% names(scales)) scales.y[names(scales$y)] <- scales$y
        }


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
    foo$x.draw <- scales.x$draw   ## this is probably the only
    foo$y.draw <- scales.y$draw   ## important component of scales
    
    
    ## Now, to construct the argument list for each panel

    
    foo$panel.args.common <-
        list(z = x, groups = groups, ...)

    
    if(number.of.cond<1) 
        foo$panel.args <- 
            list(subscripts=subscr[id])

    
    else {

        npanels <- plots.per.page * layout[3]
                                        # upper bound for number of panels
        foo$panel.args <- list(1:nplots)
        cond.current.level <- rep(1,number.of.cond)

        if (!foo$x.relation.same)           # this means that axes will be different for EACH panel
            foo$x.scales <- list(1:nplots)  # and consequently, foo$x.scales must be 
        if (!foo$y.relation.same)           # a list parallel to foo$panel.args.
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

                        foo$panel.args[[panel.number]] <- 
                            list(subscripts=subscr[id])

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









