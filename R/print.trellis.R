

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



draw.key <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    
    max.length <- 0
    ## maximum of the `row-lengths' of the above
    ## components. There is some scope for confusion
    ## here, e.g., if col is specified in key as a
    ## length 6 vector, and then lines=list(lty=1:3),
    ## what should be the length of that lines column ?
    ## If 3, what happens if lines=list() ?
    ## (Strangely enough, S+ accepts lines=list()
    ## if col (etc) is NOT specified outside, but not
    ## if it is)

    process.key <-
        function(between = 2,
                 align = TRUE,
                 title = "",
                 background = trellis.par.get("background")$col,
                 border = NULL,
                 transparent = FALSE, 
                 columns = 1,
                 divide = 3,
                 between.columns = 3,
                 cex = 1,
                 cex.title = 1.5 * max(cex),
                 col = "black", 
                 lty = 1,
                 lwd = 1,
                 font = 1, 
                 pch = 8,
                 adj = 0,
                 type = "l", 
                 size = 5, 
                 angle = 0, 
                 density = -1,
                 ...)
        {
            list(between = between,
                 align = align,
                 title = title,
                 background = background,
                 border = border,
                 transparent = transparent, 
                 columns = columns,
                 divide = divide,
                 between.columns = between.columns,
                 cex = cex,
                 cex.title = cex.title,
                 col = col,
                 lty = lty,
                 lwd = lwd,
                 font = font, 
                 pch = pch,
                 adj = adj,
                 type = type, 
                 size = size, 
                 angle = angle, 
                 density = density,
                 ...)
        }

    key <- do.call("process.key", key)

    key.length <- length(key)
    key.names <- names(key)    # Need to update
    if (is.logical(key$border)) 
        key$border <-
            if (key$border) "black"
            else NULL

    components <- list()

    for(i in 1:key.length) {

        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))

        if (is.na(curname)) {
            ;## do nothing
        }
        else if (curname == 1) { # "text"
            if (!(is.character(key[[i]][[1]])))
                stop("first component of text has to be vector of labels")
            pars <- list(labels = as.character(key[[i]][[1]]),
                         col = key$col,
                         cex = key$cex,
                         font = key$font)
            key[[i]][[1]] <- NULL
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- length(pars$labels)
            for (j in 1:length(pars))
                pars[[j]] <- rep(pars[[j]], length = tmplen)

            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "text", pars = pars, length = tmplen)

        }
        else if (curname == 2) { # "rectangles"

            pars <- list(col = key$col,
                         size = key$size,
                         angle = key$angle,
                         density = key$density)
            
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "rectangles", pars = pars)
            
        }
        else if (curname == 3) { # "lines"

            pars <- list(col = key$col,
                         size = key$size,
                         lty = key$lty,
                         lwd = key$lwd,
                         type = key$type)
                         
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "lines", pars = pars)
            
        }
        else if (curname == 4) { # "points"

            pars <- list(col = key$col,
                         cex = key$cex,
                         pch = key$pch)
                         
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "points", pars = pars)

        }
    }

    number.of.components <- length(components)
    ## number of components named one of "text",
    ## "lines", "rectangles" or "points"

    for (i in 1:number.of.components)
        if (components[[i]]$type != "text") {
            components[[i]]$pars <-
                lapply(components[[i]]$pars, rep, length = max.length)
            components[[i]]$length <- max.length
        }

    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)

    if (column.blocks > max.length) warning("not enough rows for columns")
    
    key$between <- rep(key$between, length = number.of.components)

    
    if (key$align) {

        ## Setting up the layout

        n.row <- rows.per.block + 1
        n.col <- column.blocks * (1 + 3 * number.of.components) - 1

        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- as.list(1:n.row)

        heights.x[1] <- if (key$title == "") 0 else key$cex.title
        
        widths.x <- rep(key$between.column, n.col)
        widths.units <- rep("strwidth", n.col)
        widths.data <- as.list(rep("o", n.col))
        
        for (i in 1:column.blocks) {
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i-1] <-
                         key$between/2
            
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i+1] <-
                         key$between/2
        }
    
        
        for (i in 1:number.of.components) {

            cur <- components[[i]]

            id <- (1:column.blocks - 1) *
                (number.of.components*3 + 1) + i*3 - 1

            if (cur$type == "text") {
                which.name <- "-"
                for (ss in cur$pars$labels)
                    if (nchar(ss) > nchar(which.name)) which.name <- ss
                
                widths.x[id] <- max(cur$pars$cex)
                widths.units[id] <- "strwidth"

                for (idi in id) widths.data[[idi]] <- which.name

            }
            else if (cur$type == "rectangles") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "lines") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "points") {
                widths.x[id] <- max(cur$pars$cex)
            }
        }
        ## OK, layout set up, now to draw the key

        key.layout <- grid.layout(nrow = n.row, ncol = n.col,
                                  widths = unit(widths.x, widths.units, data=widths.data),
                                  heights = unit(heights.x, heights.units, data=heights.data),
                                  respect = TRUE)

        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize=9), # the value 9 used later
                             draw = FALSE)
        
        if (!key$transparent) {
            grid.pack(key.gf,
                      grid.rect(gp=gpar(fill = key$background, col = key$border),
                                draw = FALSE),
                      draw = FALSE)
        }

        grid.pack(key.gf,
                  grid.rect(gp=gpar(col=key$border), draw = FALSE),
                  draw = FALSE)

        ## Title
        if (!(key$title == ""))
            grid.pack(key.gf, 
                      grid.text(label = key$title, draw = FALSE,
                                gp = gpar(fontsize = 9 * key$cex.title)),  ## should do better than just 9
                      row=1, draw = FALSE)
        

        
        for (i in 1:number.of.components) {

            cur <- components[[i]]

            for (j in 1:cur$length) {

                colblck <- ceiling(j / rows.per.block)

                xx <- (colblck - 1) *
                    (number.of.components*3 + 1) + i*3 - 1

                yy <- j %% rows.per.block + 1
                if (yy == 1) yy <- rows.per.block + 1

                if (cur$type == "text") {
                    grid.pack(key.gf, 
                              grid.text(label = cur$pars$labels[j],
                                        gp = gpar(col = cur$pars$col[j],
                                        fontsize = 9 * cur$pars$cex[j]),  ## should do better than just 9
                                        draw = FALSE),
                              row = yy, col = xx, draw = FALSE)
                    
                }
                else if (cur$type == "rectangles") {
                    grid.pack(key.gf, 
                              grid.rect(width = cur$pars$size[j]/max(cur$pars$size),
                                        ## centred, unlike Trellis, due to aesthetic reasons !
                                        gp = gpar(fill = cur$pars$col[j], col = NULL), 
                                        draw = FALSE),
                              row = yy, col = xx, draw = FALSE)
                    
                    ## Need to make changes to support angle/density
                }
                else if (cur$type == "lines") {
                    if (cur$pars$type[j] == "l") {
                        grid.pack(key.gf, 
                                  grid.lines(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),
                                             ## ^^ this should be centered as well, but since the
                                             ## chances that someone would actually use this feature
                                             ## are astronomical, I'm leaving that for later.
                                             y = c(.5, .5),
                                             gp = gpar(col = cur$pars$col[j],
                                             lty = cur$pars$lty[j],
                                             lwd = cur$pars$lwd[j]),
                                             draw = FALSE),
                                  row = yy, col = xx, draw = FALSE)

                    }
                    else if (cur$pars$type[j] == "p") {
                        grid.pack(key.gf, 
                                  grid.points(x = .5, y = .5,
                                              gp = gpar(col = cur$pars$col[j]),
                                              size = unit(cur$pars$cex[j] * 2.5, "mm"),
                                              pch = cur$pars$pch[j],
                                              draw = FALSE),
                                  row = yy, col = xx, draw = FALSE)

                    }
                    else { # if (cur$pars$type[j] == "b" or "o") -- not differentiating
                        grid.pack(key.gf, 
                                  grid.lines(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),
                                             ## ^^ this should be centered as well, but since the
                                             ## chances that someone would actually use this feature
                                             ## are astronomical, I'm leaving that for later.
                                             y = c(.5, .5),
                                             gp = gpar(col = cur$pars$col[j],
                                             lty = cur$pars$lty[j],
                                             lwd = cur$pars$lwd[j]),
                                             draw = FALSE),
                                  row = yy, col = xx, draw = FALSE)
                        
                        grid.pack(key.gf, 
                                  grid.points(x = (1:key$divide-1)/(key$divide-1),
                                              y = rep(.5, key$divide),
                                              gp = gpar(col = cur$pars$col[j]),
                                              size = unit(cur$pars$cex[j] * 2.5, "mm"),
                                              pch = cur$pars$pch[j],
                                              draw = FALSE),
                                  row = yy, col = xx, draw = FALSE)
                        
                    }
                    
                }
                else if (cur$type == "points") {
                    grid.pack(key.gf, 
                              grid.points(x=.5, y=.5, 
                                          gp = gpar(col = cur$pars$col[j]),
                                          size = unit(cur$pars$cex[j] * 2.5, "mm"),
                                          pch = cur$pars$pch[j],
                                          draw = FALSE),
                              row = yy, col = xx, draw = FALSE)

                    
                }

            }

        }

    }
    else stop("sorry, align=F not supported (yet ?)")

    if (draw)
        grid.draw(key.gf)

    key.gf
}

























draw.colorkey <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    
    process.key <-
        function(col,
                 at,
                 tick.number = 7,
                 width = 2,
                 height = 1,
                 ...)
        {
            list(col = col,
                 at = at,
                 tick.number = tick.number,
                 width = width,
                 height = height,
                 ...)
        }

    key <- do.call("process.key", key)


    ## Setting up the layout

    key$at <- sort(key$at) ## should check if ordered
    if (length(key$at)!=length(key$col)+1) stop("length(col) must be length(at)-1")
    n.row <- length(key$col)
    n.col <- 2

    heights.x <- rev(diff(key$at))
    heights.units <- rep("null", n.row)
    heights.data <- as.list(1:n.row)

    yscale <- range(key$at)
    cex <- 1
    col <- "black"
    font <- 1
    if (is.null(key$lab)) {
        at <- lpretty(yscale, key$tick.number)
        at <- at[at>=yscale[1] & at<=yscale[2]]
        labels <- as.character(at)
    }
    else if (is.character(key$lab) && length(key$lab)==length(key$at)) {
        at <- key$at
        labels <- as.character(key$lab)
    }
    else if (is.list(key$lab)) {
        if (!is.null(key$lab$at)) at <- key$lab$at
        if (!is.null(key$lab$lab)) labels <- as.character(key$lab$lab)
        if (!is.null(key$lab$cex)) cex <- key$lab$cex
        if (!is.null(key$lab$col)) col <- key$lab$col
        if (!is.null(key$lab$font)) font <- key$lab$font
    }

    which.name <- "-"
    for (ss in labels)
        if (nchar(ss) > nchar(which.name)) which.name <- ss
    
    widths.x <- c(key$width,1)
    widths.units <- rep("strwidth", 2)
    widths.data <- as.list(c("W", paste("--",which.name)))
        

    key.layout <- grid.layout(nrow = n.row, ncol = n.col,
                              widths = unit(widths.x, widths.units, data=widths.data),
                              heights = unit(heights.x, heights.units),
                              respect = TRUE)

    key.gf <- grid.frame(layout = key.layout, vp = vp,
                         gp = gpar(fontsize=8), #why 8 ?
                         draw = FALSE)

    for (c in seq(along=key$col))
        grid.pack(key.gf, row = n.row - c + 1, col = 1,
                  grid.rect(gp=gpar(fill=key$col[c],  col = NULL), draw = FALSE),
                  draw = FALSE)

    grid.pack(key.gf, col = 1,
              grid.rect(gp=gpar(col="black"), draw = FALSE),
              draw = FALSE)

    scat <- (at - yscale[1])/diff(yscale)

    grid.pack(key.gf, col = 1,
              grid.text(label = paste("-", labels, sep = ""),
                        x = rep(1, length(scat)),
                        y = scat,
                        just = c("left","center"),
                        gp=gpar(col="black", fontsize = cex * 8),
                        draw = FALSE),
              draw = FALSE)


    
    if (draw)
        grid.draw(key.gf)

    key.gf
}




























print.trellis <-
    function(x, position, split, more = FALSE, ...)
{

    if (is.null(dev.list())) {
        trellis.device()
        grid.start()
        on.exit(grid.stop())
    }
    else if (!exists(".grid.started") || !.grid.started) {
        grid.start(newpage=FALSE)
        on.exit(grid.stop())
    }

    obg <- par(bg = trellis.par.get("background")$col)
    on.exit(par(bg=obg), add = TRUE)
    
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
    
    panel <- # shall use "panel" in do.call
        if (is.function(x$panel)) x$panel 
        else if (is.character(x$panel)) get(x$panel)
        else eval(x$panel)

    x$strip <- 
        if (is.function(x$strip)) x$strip 
        else if (is.character(x$strip)) get(x$strip)
        else eval(x$strip)

    axis.line <- trellis.par.get("axis.line")
    number.of.cond <- length(x$condlevels)
    
    panel.width <- 1
    panel.height <- x$aspect.ratio
    layout.respect <- !x$aspect.fill
    

    if (!is.null(x$key)) {
        key.gf <- draw.key(x$key)
        key.space <-
            if ("space" %in% names(x$key)) x$key$space
            else if ("x" %in% names(x$key) ||
                     "corner" %in% names(x$key)) "inside"
            else "top"
    }
    else if (!is.null(x$colorkey)) {
        key.gf <- draw.colorkey(x$colorkey)
        key.space <- "right"
        ## if ("space" %in% names(x$colorkey)) x$colorkey$space
        ## else if ("x" %in% names(x$colorkey) ||
        ## "corner" %in% names(x$colorkey)) "inside"
        ## else "right"
    }
    
    xaxis.col <-
        if (is.logical(x$x.scales$col)) axis.line$col
        else x$x.scales$col
    xaxis.cex <-
        x$x.scales$cex * x$fontsize.small / x$fontsize.normal
    xaxis.rot <-
        if (is.logical(x$x.scales$rot)) 0
        else x$x.scales$rot
    yaxis.col <-
        if (is.logical(x$y.scales$col)) axis.line$col
        else x$y.scales$col
    yaxis.cex <-
        x$y.scales$cex * x$fontsize.small / x$fontsize.normal
    yaxis.rot <-
        if (!is.logical(x$y.scales$rot)) x$y.scales$rot
        else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) 90
        else 0

    strip.col.default.bg <-
        rep(trellis.par.get("strip.background")$col,length=number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,length=number.of.cond)


    cond.max.level <- integer(number.of.cond)
    for(i in 1:number.of.cond) {
        cond.max.level[i] <-
            if (is.character(x$condlev[[i]])) length(x$condlev[[i]])
            else nrow(x$condlev[[i]])
    }

    if(x$layout[1]==0) { # using device dimensions to
        ddim <- par("din") # calculate default layout
        device.aspect <- ddim[2]/ddim[1]
        panel.aspect <- if(layout.respect) panel.height else 1

        plots.per.page <- x$layout[2]
        m <- round(sqrt(x$layout[2] * device.aspect/panel.aspect))
        n <- ceiling(x$layout[2]/m)
        x$layout[1] <- n
        x$layout[2] <- m
    }
    else plots.per.page <- x$layout[1] * x$layout[2] 

    cols.per.page <- x$layout[1]
    rows.per.page <- x$layout[2]
    number.of.pages <- x$layout[3]
        
    if(cols.per.page>1)
        x.between <- rep(x$x.between, length = cols.per.page-1)
    if(rows.per.page>1) 
        y.between <- rep(x$y.between, length = rows.per.page-1)
    
    x.alternating <- rep(x$x.scales$alternating, length = cols.per.page)
    y.alternating <- rep(x$y.scales$alternating, length = rows.per.page)
    x.relation.same <- x$x.scales$relation == "same"
    y.relation.same <- x$y.scales$relation == "same"

    xlog <- x$x.scales$log
    ylog <- x$y.scales$log
    if (is.logical(xlog) && xlog) xlog <- 10
    if (is.logical(ylog) && ylog) ylog <- 10
    have.xlog <- !is.logical(xlog) || xlog
    have.ylog <- !is.logical(ylog) || ylog
    xlogbase <-
        if (is.numeric(xlog)) xlog
        else exp(1)
    ylogbase <-
        if (is.numeric(ylog)) ylog
        else exp(1)
    xlogpaste <-
        if (have.xlog) paste(as.character(xlog), "^", sep = "")
        else ""
    ylogpaste <-
        if (have.ylog) paste(as.character(ylog), "^", sep = "")
        else ""


    if (!is.logical(x$x.scales$at)) {  # i.e., at explicitly specified 
        if (is.logical(x$x.scales$labels))
            if (have.xlog) {
                x$x.scales$labels <- as.character(x$x.scales$at)
                x$x.scales$at <- log(x$x.scales$at, xlogbase)
            }
            else x$x.scales$labels <- as.character(x$x.scales$at)
        else x$x.scales$labels <- as.character(x$x.scales$labels)
    }
    if (!is.logical(x$y.scales$at)) {  # i.e., at explicitly specified 
        if (is.logical(x$y.scales$labels))
            if (have.ylog) {
                x$y.scales$labels <- as.character(x$y.scales$at)
                x$y.scales$at <- log(x$y.scales$at, ylogbase)
            }
            else x$y.scales$labels <- as.character(x$y.scales$at)
        else x$y.scales$labels <- as.character(x$y.scales$labels)
    }


    if (x.relation.same && is.logical(x$x.scales$at)) {
        x$x.scales$at <-
            lpretty(x$x.limits,
                    n = x$x.scales$tick.number)
        if (is.logical(x$x.scales$labels))
            x$x.scales$labels <- paste(xlogpaste, as.character(x$x.scales$at), sep = "")
        x$x.scales$labels <- as.character(x$x.scales$labels)
    }
    if (y.relation.same && is.logical(x$y.scales$at)) {
        x$y.scales$at <-
            lpretty(x$y.limits,
                    n = x$y.scales$tick.number)
        if (is.logical(x$y.scales$labels))
            x$y.scales$labels <- paste(ylogpaste, as.character(x$y.scales$at), sep = "")
        x$y.scales$labels <- as.character(x$y.scales$labels)
    }

    have.main <- !(is.null(x$main$label) || (is.character(x$main$label) && x$main$label==""))
    have.sub <- !(is.null(x$sub$label)   || (is.character(x$sub$label) && x$sub$label==""))
    have.xlab <- !(is.null(x$xlab$label) || (is.character(x$xlab$label) && x$xlab$label==""))
    have.ylab <- !(is.null(x$ylab$label) || (is.character(x$ylab$label) && x$ylab$label==""))

    ## Shall calculate the per page layout now:

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
                                        (if (x$as.table) 1:(rows.per.page-1)
                                        else (rows.per.page-1):1)
                                        - 1)*(number.of.cond+3)] <-
                                            x$y.between


    heights.x[1] <- if (have.main) 2 * x$main$cex else 1
    heights.x[n.row] <- if (have.sub) 2 * x$sub$cex else 1

    heights.x[2] <- 0 # for the key, currently unimplemented
    heights.x[n.row-1] <- 0 # key


    if (x$x.scales$draw) {
        
        if (x.relation.same) {

            if (xaxis.rot == 0) {
                
                if(any(x.alternating==1)) 
                    heights.x[3] <- 3 * xaxis.cex
                
                if (any(x.alternating==2)) 
                    heights.x[n.row-3] <- 3 * xaxis.cex
                
            }
            else {
                
                which.name <- "-"
                for (ss in x$x.scales$labels)
                    if (nchar(ss) > nchar(which.name)) which.name <- ss
                if(any(x.alternating==1)) {
                    heights.x[3] <- xaxis.cex  # cos(rot) ?
                    heights.units[3] <- "strwidth"
                    heights.data[[3]] <- paste(which.name, "--")
                }
                if (any(x.alternating==2)) {
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

                if (is.logical(x$x.scales$at)) {

                    for (sc in x$x.limits) {
                        at <- lpretty(sc, n = x$x.scales$tick.number)
                        labs <- paste(xlogpaste, as.character(at), sep = "")
                        for (ss in labs)
                            if (nchar(ss) > nchar(which.name)) which.name <- ss
                    }
                }
                else
                    for (ss in x$x.scales$labels)
                        if (nchar(ss) > nchar(which.name)) which.name <- ss
                

                heights.x[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                    yaxis.cex
                heights.units[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                    "strwidth"
                heights.data[number.of.cond + 5 + (1:rows.per.page - 1)*(number.of.cond+3)] <-
                    paste(which.name, "--")
                
            }
                
        }
            
    }

    heights.x[n.row-2] <- if (have.xlab) 2 * x$xlab$cex else 1
    widths.x[2] <- if (have.ylab) 3 * x$ylab$cex else 1
    
    ## this is if strip=F -- strips not to be drawn
    for(crr in 1:number.of.cond)
        heights.x[number.of.cond + 4 + (1:rows.per.page - 1)*(number.of.cond+3) -crr] <-
            if (is.logical(x$strip)) 0  # which means strip = F, strips not to be drawn
            else x$par.strip.text$cex

    ## fine tuning widths:

    widths.x[(1:cols.per.page - 1)*3 + 5] <-
        panel.width # for the panels
    widths.units[(1:cols.per.page - 1)*3 + 5] <-
        "null" # for the panels


    widths.x[(1:cols.per.page - 1)*3 + 4] <- 0
    ## For y-axes to the left of each panel when relation != "same"
    

    if(cols.per.page>1)
        widths.x[(1:(cols.per.page-1) - 1)*3 + 6] <-
            x$x.between
    
    widths.x[1] <- 0 #for the key, currently unimplemented
    widths.x[n.col] <- 1 # key - right



    ## next part of the code decides how much space to leave for y-labels

    if (x$y.scales$draw) {
        if (y.relation.same) {
            if (yaxis.rot == 90) {
                if(any(y.alternating==1)) 
                    widths.x[3] <- 3 * yaxis.cex

                if (any(y.alternating==2)) 
                    widths.x[n.col-1] <- 3 * yaxis.cex
            }
            else {
                
                which.name <- "-"
                for (ss in x$y.scales$labels)
                    if (nchar(ss) > nchar(which.name)) which.name <- ss
                if(any(y.alternating==1)) {
                    widths.x[3] <- yaxis.cex  # cos(rot) ?
                    widths.units[3] <- "strwidth"
                        widths.data[[3]] <- paste(which.name, "--")
                }
                if (any(y.alternating==2)) {
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

                if (is.logical(x$y.scales$at)) {

                    for (sc in x$y.limits) {
                        at <- lpretty(sc, n = x$y.scales$tick.number)
                        labs <- paste(ylogpaste, as.character(at), sep = "")
                        for (ss in labs)
                            if (nchar(ss) > nchar(which.name)) which.name <- ss
                    }
                }
                else
                    for (ss in x$y.scales$labels)
                        if (nchar(ss) > nchar(which.name)) which.name <- ss
                
                widths.x[(1:cols.per.page - 1)*3 + 4] <- yaxis.cex
                widths.units[(1:cols.per.page - 1)*3 + 4] <- "strwidth"
                widths.data[(1:cols.per.page - 1)*3 + 4] <- paste(which.name, "--")
                
            }
            
        }
        
    }


    if (!is.null(x$key) || !is.null(x$colorkey)) {
            
        if (key.space == "left") {
            widths.x[1] <- 1
            widths.units[1] <- "grobwidth"
            widths.data[[1]] <- key.gf
        }
        else if (key.space == "right") {
            widths.x[n.col] <- 1
            widths.units[n.col] <- "grobwidth"
            widths.data[[n.col]] <- key.gf
        }
        else if (key.space == "top") {
            heights.x[2] <- 1
            heights.units[2] <- "grobheight"
            heights.data[[2]] <- key.gf
        }
        else if (key.space == "bottom") {
            heights.x[n.row-1] <- 1
            heights.units[n.row-1] <- "grobheight"
            heights.data[[n.row-1]] <- key.gf
        }
        
    }
    

    ## Constructing the layout:

    page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                               widths = unit(widths.x, widths.units, data=widths.data),
                               heights = unit(heights.x, heights.units, data=heights.data),
                               respect = layout.respect)

    cond.current.level <- rep(1,number.of.cond)
    panel.number <- 1
        
    for(page.number in 1:number.of.pages)
        if (!any(cond.max.level-cond.current.level<0)) {
                
            if(usual) grid.newpage()
            
            push.viewport(viewport(layout = page.layout,
                                   gp = gpar(fontsize = x$fontsize.normal,
                                   col = axis.line$col,
                                   lty = axis.line$lty,
                                   lwd = axis.line$lwd)))
                
            if (have.main)
                grid.text(label = x$main$label,
                          gp = gpar(col = x$main$col,
                          fontsize = x$fontsize.normal * x$main$cex),
                          vp = viewport(layout.pos.row = 1))
                    
                    
            if (have.sub)
                grid.text(label = x$sub$label,
                          gp = gpar(col = x$sub$col,
                          fontsize = x$fontsize.normal * x$sub$cex),
                          vp = viewport(layout.pos.row = n.row))
                    
                    
            if (have.xlab)
                grid.text(label = x$xlab$label,
                          gp = gpar(col = x$xlab$col,
                          fontsize = x$fontsize.normal * x$xlab$cex), 
                          vp = viewport(layout.pos.row = n.row - 2))
                    
                
            if (have.ylab)
                grid.text(label = x$ylab$label, rot = 90,
                          gp = gpar(col = x$ylab$col,
                          fontsize = x$fontsize.normal * x$ylab$cex),
                          vp = viewport(layout.pos.col = 2))
            
            for (row in 1:rows.per.page)
                for (column in 1:cols.per.page)

                    if (!any(cond.max.level-cond.current.level<0) &&
                        (row-1) * cols.per.page + column <= plots.per.page) {

                        if (!is.list(x$panel.args[[panel.number]]))
                            ## corr to skip = T or extra plots
                            panel.number <- panel.number + 1
                            
                        else {
                                
                            actual.row <- if (x$as.table)
                                (rows.per.page-row+1) else row
                            ## this gives the row position from the bottom
                            pos.row <- (rows.per.page-actual.row) *
                                (number.of.cond+3)+4+number.of.cond
                            pos.col <- (column-1)*3 + 5
                            
                            xscale <-
                                if(x.relation.same)
                                    x$x.limits
                                else x$x.limits[[panel.number]]
                            yscale <- 
                                if(y.relation.same)
                                    x$y.limits
                                else x$y.limits[[panel.number]]
                            
                                
                            push.viewport(viewport(layout.pos.row = pos.row,
                                                   layout.pos.col = pos.col,
                                                   xscale = xscale,
                                                   yscale = yscale,
                                                   gp = gpar(fontsize =
                                                   x$fontsize.normal)))


                            pargs <- c(x$panel.args[[panel.number]],
                                       x$panel.args.common)
                            if (!("..." %in% names(formals(panel))))
                                pargs <- pargs[names(formals(panel))]
                            do.call("panel", pargs)

                            ## next few lines deal with drawing axes
                            ## as appropriate
                            ## when relation != same, axes drawn for
                            ## each panel:
                            
                            ## X-axis
                            if(!x.relation.same && x$x.scales$draw) {

                                axs <- x$x.scales

                                if (is.logical(axs$at)) {
                                    axs$at <- lpretty(xscale, n = axs$tick.number)
                                    axs$labels <- paste(xlogpaste, as.character(axs$at), sep = "")
                                } # o.w., done before

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
                                                  fontsize = x$fontsize.normal * xaxis.cex))
                                                
                                    }
                            }
                            ## Y-axis
                            if(!y.relation.same && x$y.scales$draw) {
                                
                                axs <- x$y.scales

                                if (is.logical(axs$at)) {
                                    axs$at <- lpretty(yscale, n = axs$tick.number)
                                    axs$labels <- paste(ylogpaste, as.character(axs$at), sep = "")
                                } # o.w., done before

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
                                                  fontsize = x$fontsize.normal * yaxis.cex))
                                        
                                    }
                                
                            }

                            ## When relation = same, axes drawn based on value of alternating
                            if (y.relation.same && x$y.scales$draw) {
                                
                                ## Y-axis to the left
                                if (column == 1) {
                                    axs <- x$y.scales
                                    axs$limits <- x$y.limits
                                    for(tt in seq(along=axs$at))
                                        if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                        {
                                            
                                            grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                                                       x = unit(c(0,-.25), "lines"),
                                                       gp = gpar(col = yaxis.col))
                                            
                                                if (y.alternating[actual.row]==1)
                                                    grid.text(label = axs$label[tt],
                                                              x = unit(-.5, "lines"),
                                                              y = unit(axs$at[tt], "native"),
                                                              just = if (yaxis.rot != 90) c("right", "centre")
                                                              else c("centre", "left"),
                                                              rot = yaxis.rot, 
                                                              gp =gpar(col = yaxis.col,
                                                              fontsize = x$fontsize.normal * yaxis.cex))
                                            
                                            }
                                }
                                
                                ## Y-axis to the right
                                if (column == cols.per.page) {
                                    axs <- x$y.scales
                                    axs$limits <- x$y.limits                                    
                                    for(tt in seq(along=axs$at))
                                        if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                        {
                                            
                                            grid.lines(y = unit(rep(axs$at[tt],2), "native"),
                                                       x = unit(c(1,1), "npc") + unit(c(0,.25), "lines"),
                                                       gp = gpar(col = yaxis.col))
                                            
                                            if (y.alternating[actual.row]==2)
                                                grid.text(label = axs$label[tt],
                                                          x = unit(1,"npc") + unit(.5, "lines"),
                                                          y = unit(axs$at[tt], "native"),
                                                          just = if (yaxis.rot != 90) c("left", "centre")
                                                          else c("centre", "right"),
                                                          rot = yaxis.rot, 
                                                          gp = gpar(col = yaxis.col,
                                                          fontsize = x$fontsize.normal * yaxis.cex))
                                            
                                        }
                                }
                                
                            }
                                    
                                    
                                
                            ## X-axis to the bottom
                            if (x.relation.same && x$x.scales$draw) {
                                if (actual.row == 1) {
                                    axs <- x$x.scales
                                    axs$limits <- x$x.limits
                                    for(tt in seq(along=axs$at))
                                        if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                        {
                                                
                                            grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                                                       y = unit(c(0,-.25), "lines"),
                                                       gp = gpar(col = xaxis.col))
                                            
                                            if (x.alternating[column]==1)
                                                grid.text(label = axs$label[tt],
                                                          y = unit(-.5, "lines"),
                                                          x = unit(axs$at[tt], "native"),
                                                          just = if (xaxis.rot == 0) c("centre", "right")
                                                          else c("right", "centre"),
                                                          rot = xaxis.rot, 
                                                          gp = gpar(col = xaxis.col,
                                                          fontsize = x$fontsize.normal * xaxis.cex))
                                                    
                                        }
                                }
                                ## X-axis at the top is a bit more complicated as
                                ## it has to be drawn on top of the strips.
                            }
                                    
                                    
                                    
                            grid.rect()
                            
                            pop.viewport()
                            
                            if (!is.logical(x$strip)) # logical ==> FALSE
                                for(i in 1:number.of.cond)
                                {
                                    push.viewport(viewport(layout.pos.row = pos.row-i,
                                                           layout.pos.col = pos.col,
                                                           gp = gpar(fontsize = x$fontsize.normal)))
                                    
                                    grid.rect()
                                    
                                    x$strip(x$condlevel[[i]],
                                              level = cond.current.level[i],
                                              name = names(x$cond)[i],
                                              bg = strip.col.default.bg[i],
                                              fg = strip.col.default.fg[i],
                                              par.strip.text = x$par.strip.text)
                                    
                                    pop.viewport()
                                            
                                }
                            
                            
                            ## X-axis at top
                            if (x.relation.same && x$x.scales$draw)
                                if (actual.row == rows.per.page) {
                                    axs <- x$x.scales
                                    axs$limits <- x$x.limits
                                    push.viewport(viewport(layout.pos.row = pos.row -
                                                           if (is.logical(x$strip)) 0 else number.of.cond,
                                                           layout.pos.col = pos.col,
                                                           xscale = xscale))
                                            
                                        
                                    for(tt in seq(along=axs$at))
                                        if (axs$at[tt]>=axs$limits[1] && axs$at[tt]<=axs$limits[2])
                                        {
                                                    
                                            grid.lines(x = unit(rep(axs$at[tt],2), "native"),
                                                       y = unit(c(1,1), "npc") + unit(c(0,.25), "lines"),
                                                       gp = gpar(col = xaxis.col))
                                            
                                            if (x.alternating[column]==2)
                                                grid.text(label = axs$label[tt],
                                                          y = unit(1,"npc") + unit(.5, "lines"),
                                                          x = unit(axs$at[tt], "native"),
                                                          just = if (xaxis.rot == 0) c("centre","left")
                                                          else c("left", "centre"),
                                                          rot = xaxis.rot, 
                                                          gp = gpar(col = xaxis.col,
                                                          fontsize = x$fontsize.normal * xaxis.cex))
                                                    
                                        }
                                    
                                    pop.viewport()
                                }
                            
                                    
                                
                                
                            cond.current.level <- cupdate(cond.current.level,
                                                          cond.max.level)
                            panel.number <- panel.number + 1
                            
                        }
                        
                    }
            
            
            if (!is.null(x$key) || !is.null(x$colorkey)) {
                
                if (key.space == "left") {
                    push.viewport(viewport(layout.pos.col=1))
                    grid.draw(key.gf)
                    pop.viewport()
                }
                else if (key.space == "right") {
                    push.viewport(viewport(layout.pos.col=n.col,
                                           layout.pos.row = c(4, n.row-4)))
                    grid.draw(key.gf)
                    pop.viewport()
                    }
                else if (key.space == "top") {
                    push.viewport(viewport(layout.pos.row=2))
                    grid.draw(key.gf)
                    pop.viewport()
                }
                else if (key.space == "bottom") {
                    push.viewport(viewport(layout.pos.row=n.row-1))
                    grid.draw(key.gf)
                    pop.viewport()
                }
                else if (key.space == "inside") {
                    
                    push.viewport(viewport(layout.pos.row = c(1, n.row),
                                           layout.pos.col = c(1, n.col)))
                    
                    if (is.null(x$key$corner)) x$key$corner <- c(0,1)
                    if (is.null(x$key$x)) x$key$x <- x$key$corner[1]
                    if (is.null(x$key$y)) x$key$y <- x$key$corner[2]
                    
                    if (all(x$key$corner == c(0,1))) {
                        
                        push.viewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                               widths = unit(c(x$key$x, 1, 1),
                                               c("npc", "grobwidth", "null"),
                                               list(1, key.gf, 1)),
                                               heights = unit(c(1-x$key$y, 1, 1),
                                               c("npc", "grobheight", "null"),
                                               list(1, key.gf, 1)))))
                        
                        push.viewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        
                        grid.draw(key.gf)
                        
                        pop.viewport()
                        pop.viewport()
                        
                    }
                    
                    
                    if (all(x$key$corner == c(1,1))) {
                        
                        push.viewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                               heights = unit(c(1-x$key$y, 1, 1),
                                               c("npc", "grobheight", "null"),
                                               list(1, key.gf, 1)),
                                               widths = unit(c(1, 1, 1-x$key$x),
                                               c("null", "grobwidth", "npc"),
                                               list(1, key.gf, 1)))))
                        
                        push.viewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        
                        grid.draw(key.gf)

                        pop.viewport()
                        pop.viewport()
                        
                    }
                    

                    if (all(x$key$corner == c(0,0))) {
                    
                        push.viewport(viewport(layout = grid.layout(nrow = 3, ncol = 3,
                                               widths = unit(c(x$key$x, 1, 1),
                                               c("npc", "grobwidth", "null"),
                                               list(1, key.gf, 1)),
                                               heights = unit(c(1,1,x$key$y),
                                               c("null", "grobheight", "npc"),
                                               list(1, key.gf, 1)))))
                        
                        push.viewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        
                        grid.draw(key.gf)
                        
                        pop.viewport()
                        pop.viewport()
                        
                    }
                    
                    
                    if (all(x$key$corner == c(1,0))) {
                            
                        push.viewport(viewport(layout=grid.layout(nrow = 3, ncol = 3,
                                               widths = unit(c(1, 1, 1-x$key$x),
                                               c("null", "grobwidth", "npc"),
                                               list(1, key.gf, 1)),
                                               heights = unit(c(1, 1, x$key$y),
                                               c("null", "grobheight", "npc"),
                                               list(1, key.gf, 1)))))
                        
                        push.viewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
                        
                        grid.draw(key.gf)
                        
                        pop.viewport()
                        pop.viewport()
                        
                    }
                    
                    
                    
                    pop.viewport()
                    
                }
                
            }
            
            push.viewport(viewport(layout.pos.row = c(1, n.row),
                                   layout.pos.col = c(1, n.col)))
            if(!is.null(x$page)) x$page(page.number)                
            pop.viewport()
            
            pop.viewport()
            
            
        }


    if (!missing(position)) {
        if (!missing(split)) {
            pop.viewport()
            pop.viewport()
        }
        pop.viewport()
    }
    
    
    else if (!missing(split)) {
        pop.viewport()
        pop.viewport()
    }
    
}

















