

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





prepanel.default.splom <-
    function(x, y, type, ...)
{
    list(xlim = c(0,1),
         ylim = c(0,1),
         dx = 1,
         dy = 1)
}




panel.splom <-
    function(...)
    panel.xyplot(...)





panel.pairs <-
    function(z, panel = "panel.splom", groups = NULL,
             panel.subscripts,
             subscripts,
             fontsize.small = 8,
             pscales = 5,
             ...)
{
    panel <- 
        if (is.function(panel)) panel 
        else if (is.character(panel)) get(panel)
        else eval(panel)

    axis.line <- trellis.par.get("axis.line")
    n.var <- ncol(z)

    if(n.var>0) {
        ## there must be a better way to do the foll:
        lim <- list(1:n.var)
        for(i in 1:n.var) {
            lim[[i]] <- extend.limits(range(as.numeric(z[,i])))
        }
        ## should be further complicated by allowing for customization by
        ## prepanel functions --- prepanel(z[i], z[j]) etc
    }
    ## maybe (ideally) this should be affected by scales

    draw <- is.numeric(pscales) && pscales!=0 # whether axes to be drawn

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
                    if (!is.null(colnames(z)))
                        grid.text(colnames(z)[i],
                                  gp = gpar(fontsize = 10))
                    if (draw) {
                        ## plot axes

                        if (is.factor(z[,i])) {
                            axls <- 1:nlevels(z[,i])
                            nal <- length(axls)/2+.5

                            for(tt in seq(along=axls)) {
                                if(tt <= nal) {
                                    
                                    grid.lines(y = unit(rep(axls[tt],2), "native"),
                                               x = unit(c(1,1),"npc") - unit(c(0,.25), "lines"),
                                               gp = gpar(col = axis.line$col))
                                    
                                    grid.text(label = levels(z[,i])[tt],
                                              x = unit(1,"npc") - unit(.5, "lines"),
                                              y = unit(axls[tt], "native"),
                                              just = c("right", "centre"))
                                    
                                    grid.lines(x = unit(rep(axls[tt],2), "native"),
                                               y = unit(c(0,.25), "lines"),
                                               gp = gpar(col = axis.line$col))
                                    
                                    grid.text(label = levels(z[,i])[tt],
                                              rot = 90,
                                              y = unit(0.5, "lines"),
                                              x = unit(axls[tt], "native"),
                                              just = c("bottom", "centre"))
                                    
                                }
                                if(tt >=nal) {
                                    
                                    grid.lines(y = unit(rep(axls[tt],2), "native"),
                                               x = unit(c(0,0.25), "lines"),
                                               gp = gpar(col = axis.line$col))
                                    
                                    grid.text(label = levels(z[,i])[tt],
                                              x = unit(0.5, "lines"),
                                              y = unit(axls[tt], "native"),
                                              just = c("left", "centre"))
                                    
                                    grid.lines(x = unit(rep(axls[tt],2), "native"),
                                               y = unit(c(1,1),"npc") - unit(c(0,.25), "lines"),
                                               gp = gpar(col = axis.line$col))
                                    
                                    grid.text(label = levels(z[,i])[tt], rot = 90,
                                              y = unit(1,"npc") - unit(.5, "lines"),
                                              x = unit(axls[tt], "native"),
                                              just = c("top", "centre"))
                                    
                                }
                                
                            }
                            
                        }
                        else {
                        
                            axls <-
                                if (is.list(pscales) && !is.null(pscales[[i]]$at))
                                    pscales[[i]]$at
                                else
                                    lpretty(lim[[i]], n = pscales)

                            labels <-
                                if (is.list(pscales) && !is.null(pscales[[i]]$lab))
                                    pscales[[i]]$lab
                            ## should be rendered like factors ?
                                else
                                    as.character(axls)

                            axid <- axls>lim[[i]][1] & axls <lim[[i]][2]
                            axls <- axls[axid]
                            labels <- labels[axid]
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
                    }

                    grid.rect()

                }
                else
                {
                    if(!panel.subscripts)
                        panel(x=as.numeric(z[subscripts, j]),
                              y=as.numeric(z[subscripts, i]), ...)

                    else panel(x=as.numeric(z[subscripts, j]),
                               y=as.numeric(z[subscripts, i]),
                               groups = groups,
                               subscripts = subscripts, ...)

                    grid.rect()
                }
                pop.viewport()
            }
        pop.viewport()
    }
}




splom <-
    function(formula,
             data = parent.frame(),
             aspect = 1,
             between = list(x = 0.5, y = 0.5),
             layout = NULL,
             panel = "panel.splom",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = "Scatter Plot Matrix",
             xlim,
             ylab = NULL,
             ylim,
             superpanel = "panel.pairs",
             pscales = 5,
             varnames,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

    ## dots <- eval(substitute(list(...)), data, parent.frame())
    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    ## Step 1: Evaluate x, y, etc. and do some preprocessing
    
    form <- latticeParseFormula(formula, data)
    cond <- form$condition
    number.of.cond <- length(cond)
    x <- as.data.frame(form$right)
    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, nrow(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }
    if (!missing(varnames)) colnames(x) <-
        eval(substitute(varnames), data, parent.frame())
    
    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())
    if ("subscripts" %in% names(formals(eval(panel)))) subscripts <- TRUE
    subscr <- seq(along=x[,1])
    x <- x[subset,, drop = TRUE]
    subscr <- subscr[subset, drop = TRUE]
    
    ##if(!(is.numeric(x) && is.numeric(y)))
    ##    warning("Both x and y should be numeric")    WHAT ?


    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = aspect,
                          between = between,
                          panel = superpanel,
                          strip = strip,
                          xlab = xlab,
                          ylab = ylab), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()
    foo$fontsize.normal <- 10
    foo$fontsize.small <- 8

    ## This is for cases like xlab/ylab = list(cex=2)
    if (is.list(foo$xlab) && !is.character(foo$xlab$label))
        foo$xlab$label <- "Scatter Plot Matrix"
    if (is.list(foo$ylab) && !is.character(foo$ylab$label))
        foo$ylab <- NULL

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## It is not very clear exactly what effect scales is supposed
    ## to have. Not much in Trellis (probably), but there are certain
    ## components which are definitely relevant, and certail others
    ## (like log) which can be used in innovative ways. However, I'm
    ## postponing all that to later, if at all,and for now TOTALLY
    ## ignoring scales
    
    ##scales <- eval(substitute(scales), data, parent.frame())
    ##if (is.character(scales)) scales <- list(relation = scales)
    scales <- list(relation = "same", draw = FALSE)
    foo <- c(foo, 
             do.call("construct.scales", scales))


    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit)) {
        have.ylim <- TRUE
        ylim <- foo$x.scales$limit
    }
    if (have.xlim || have.ylim) {
        warning("Limits cannot be explicitly specified")
    }
    have.xlim <- TRUE
    have.ylim <- TRUE
    xlim <- c(0,1)
    ylim <- c(0,1)
    
    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
#    if (have.xlog) {
#        xlog <- foo$x.scales$log
#        xbase <-
#            if (is.logical(xlog)) 10
#            else if (is.numeric(xlog)) xlog
#            else if (xlog == "e") exp(1)
#
#        x <- log(x, xbase)
#        if (have.xlim) xlim <- log(xlim, xbase)
#    }
#    if (have.ylog) {
#        ylog <- foo$y.scales$log
#        ybase <-
#            if (is.logical(ylog)) 10
#            else if (is.numeric(ylog)) ylog
#            else if (ylog == "e") exp(1)
#
#        y <- log(y, ybase)
#        if (have.ylim) ylim <- log(ylim, ybase)
#    }
    
    ## Step 5: Process cond

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, numlevels))


    id.na <- F
    for (j in 1:ncol(x))
        id.na <- id.na | is.na(x[,j])
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args


    foo$panel.args.common <-
        c(list(z = x,
               panel = panel,
               panel.subscripts = subscripts,
               groups = groups, # xscales = foo$x.scales, yscales = foo$y.scales,
               pscales = pscales),
          dots)

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
                            ((var$x >=
                              var$int[cond.current.level[i], 1])
                             & (var$x <=
                                var$int[cond.current.level[i], 2]))
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
             limits.and.aspect(prepanel.default.splom,
                               prepanel = prepanel, 
                               have.xlim = have.xlim, xlim = xlim, 
                               have.ylim = have.ylim, ylim = ylim, 
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = aspect,
                               nplots = nplots))

    class(foo) <- "trellis"
    foo
}

