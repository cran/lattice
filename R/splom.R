

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



splom <-
    function(formula,
             aspect = 1,
             as.table = FALSE,
             between = list(x=0.5,y=0.5),
             data,
             groups = NULL,
             key = NULL,
             layout,
             main = NULL,
             page = NULL,
             panel = panel.splom,
             superpanel = panel.pairs,
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

    ## Need to change factors into numerics
    for(i in 1:ncol(x)) if (is.factor(x[,i])) x[,i] <- as.numeric(x[,i])

    
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
                panel = superpanel,
                panel.args = NULL,
                panel.args.common = NULL,
                par.strip.text = trellis.par.get("add.text"),
                skip = skip,
                strip = strip,
                main = NULL,
                sub = NULL,
                xlab = NULL,
                ylab = NULL,
                x.draw = FALSE,
                y.draw = FALSE,
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
        
        scales.common <- list(draw = TRUE,
                              tck = 1, # factor affecting length of ticks 
                              cex = 1,
                              tick.number = 4,
                              rot = c(0,0),
                              at = F,
                              labels = F, col = F, log = F)

        if(!missing(scales)) {
            scales.common[names(scales)] <- scales
            if ("x" %in% names(scales)) warning("x in scales ?")
            if ("y" %in% names(scales)) warning("y in scales ?")
        }


        foo$x.relation.same <- T
        foo$y.relation.same <- T

        foo$x.scales <- list(limits = c(0,1))
        foo$y.scales <- list(limits = c(0,1))

        ## aspect.fill <- if (aspect=="fill") TRUE else FALSE (done above)
        foo$aspect.ratio <-
            if (aspect == "fill") 1
            else if (aspect == "xy") 1 # nonsense
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

    }
    else
    {
        ##aspect.fill <- if (aspect=="fill") TRUE else FALSE
        foo$aspect.ratio <-
            if (aspect == "fill") 1
            else if (aspect == "xy") 1 
            else aspect[1]
        

        id <- !id.na

        scales.common <- list(draw = TRUE,
                              tck = 1, # factor affecting length of ticks 
                              cex = 1,
                              rot = 0,
                              tick.number = 5,
                              at = F,
                              labels = F, col = F, log = F)

        if(!missing(scales)) {
            scales.common[names(scales)] <- scales
            if ("x" %in% names(scales)) warning("x in scales ?")
            if ("y" %in% names(scales)) warning("y in scales ?")
        }


        foo$x.scales <- list(limits = c(0,1))
        foo$y.scales <- list(limits = c(0,1))

    }

    ## Now, to construct the argument list for each panel

    foo$panel.args.common <-
        list(z = x, groups = groups, panel=panel, panel.subscripts = subscripts,
             scales = scales.common, ...)
    
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

















panel.pairs <-
    function(z, panel = panel.splom, groups = NULL,
             panel.subscripts,
             subscripts,
             fontsize.small = 8,
             scales,
             ...)
{

    axis.line <- trellis.par.get("axis.line")
    n.var <- ncol(z)

    if(n.var>0) {
        ## there must be a better way to do the foll:
        lim <- list(1:n.var)
        for(i in 1:n.var)
            lim[[i]] <- extend.limits(range(z[,i]))
        ## should be further complicated by allowing for customization by
        ## prepanel functions --- prepanel(z[i], z[j]) etc
    }
    ## maybe (ideally) this should be affected by scales
    
    
    splom.layout <- grid.layout(nrow=n.var, ncol=n.var)
    
    if(n.var > 0 && any(subscripts)) {
        
        push.viewport(viewport(layout=splom.layout))
        
        for(i in 1:n.var)
            for(j in 1:n.var)
            {
                push.viewport(viewport(layout.pos.row = n.var-i+1,
                                       layout.pos.col = j,
                                       gp = gpar(fontsize = fontsize.small),
                                       xscale = lim[[j]],
                                       yscale = lim[[i]]))
                
                if(i == j)
                {
                    
                    if (!is.null(colnames(z))) grid.text(colnames(z)[i])
                        
                    if (scales$draw) {
                        ## plot axes
                        
                        axls <-
                            if (is.logical(scales$at))
                                lpretty(lim[[i]], n = scales$tick.number)
                            else scales$at
                        if (!is.logical(scales$labels)) warning("labels in scales not supported yet")
                        axls <- axls[axls>lim[[i]][1] & axls <lim[[i]][2]]
                        nal <- length(axls)/2+.5
                        
                        for(tt in seq(along=axls)) {
                            if(tt <= nal) {
                                
                                grid.lines(y = unit(rep(axls[tt],2), "native"),
                                           x = unit(c(1,1),"npc") - unit(c(0,.25), "lines"),
                                           gp = gpar(col = axis.line$col))
                                
                                grid.text(label = as.character(axls[tt]),
                                          x = unit(1,"npc") - unit(.5, "lines"),
                                          y = unit(axls[tt], "native"),
                                          just = c("right", "centre"))
                                
                                grid.lines(x = unit(rep(axls[tt],2), "native"),
                                           y = unit(c(0,.25), "lines"),
                                           gp = gpar(col = axis.line$col))
                                
                                grid.text(label = as.character(axls[tt]),
                                          y = unit(0.5, "lines"),
                                          x = unit(axls[tt], "native"),
                                          just = c("centre", "left"))
                                
                            }
                            if(tt >=nal) {
                                
                                grid.lines(y = unit(rep(axls[tt],2), "native"),
                                           x = unit(c(0,0.25), "lines"),
                                           gp = gpar(col = axis.line$col))
                                
                                grid.text(label = as.character(axls[tt]),
                                          x = unit(0.5, "lines"),
                                          y = unit(axls[tt], "native"),
                                          just = c("left", "centre"))
                                
                                grid.lines(x = unit(rep(axls[tt],2), "native"),
                                           y = unit(c(1,1),"npc") - unit(c(0,.25), "lines"),
                                           gp = gpar(col = axis.line$col))
                                
                                grid.text(label = as.character(axls[tt]),
                                          y = unit(1,"npc") - unit(.5, "lines"),
                                          x = unit(axls[tt], "native"),
                                          just = c("centre", "right"))
                                
                            }
                            
                        }
                        
                    }
                    
                    grid.rect()
                    
                }
                else
                {
                    if(!panel.subscripts)
                        panel(x=z[subscripts, j, drop=T],
                              y=z[subscripts, i, drop=T], ...)
                    
                    else panel(x=z[subscripts, j, drop=T],
                               y=z[subscripts, i, drop=T],
                               groups = groups,
                               subscripts = subscripts, ...)
                    
                    grid.rect()
                }
                
                pop.viewport(current.viewport())
            }
        
        pop.viewport(current.viewport())
        
    }

    
}








