

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






## the pos'th entry in the unit vector x is replaced by the unit u.
## Essentially does what x[pos] <- u should have done, only u can only
## be a unit of length 1

rearrangeUnit <- function(x, pos, u)
{
    if (unit.length(x) == 1)
        u
    else if (pos == 1)
        unit.c(u, x[(pos+1):unit.length(x)])
    else if (pos == unit.length(x))
        unit.c(x[1:(pos-1)], u)
    else
        unit.c(x[1:(pos-1)], u, x[(pos+1):unit.length(x)])
}





# convenience function for the most common type of key

simpleKey <- function(text, points = TRUE,
                      rectangles = FALSE,
                      lines = FALSE, ...)
{
    foo <- seq(along = text)
    ans <- list(text = list(lab = text), ...)

    if (points) ans$points <-
        Rows(trellis.par.get("superpose.symbol"), foo)

    if (rectangles) {
        col.regions <- trellis.par.get("regions")$col
        numcol.r <- length(col.regions)
        numcol <- length(foo)
        ans$rectangles <-
            list(col = 
                 if (numcol.r <= numcol) rep(col.regions, length = numcol)
                 else col.regions[floor(1+(foo-1)*(numcol.r-1)/(numcol-1))])
    }
    if (lines) ans$lines <-
        Rows(trellis.par.get("superpose.line"), foo)
    ans
}
             





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
                 title = NULL,
                 background = trellis.par.get("background")$col,
                 border = FALSE,
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

    default.fontsize <- trellis.par.get("fontsize")$default

    key <- do.call("process.key", key)

    key.length <- length(key)
    key.names <- names(key)    # Need to update
    if (is.logical(key$border)) 
        key$border <-
            if (key$border) "black"
            else "transparent"

    components <- list()

    for(i in 1:key.length) {

        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))

        if (is.na(curname)) {
            ;## do nothing
        }
        else if (curname == 1) { # "text"
            if (!(is.characterOrExpression(key[[i]][[1]])))
                stop("first component of text has to be vector of labels")
            pars <- list(labels = key[[i]][[1]],
                         col = key$col,
                         adj = key$adj,
                         cex = key$cex,
                         font = key$font)
            key[[i]][[1]] <- NULL
            pars[names(key[[i]])] <- key[[i]]

            tmplen <- length(pars$labels)
            for (j in 1:length(pars))
                if (is.character(pars))
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
                         cex = key$cex,
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
    if (number.of.components == 0)
        stop("Invalid key, need at least one component named lines, text, rect or points")

    ## The next part makes sure all components have same length,
    ## except text, which should be as long as the number of labels

    for (i in 1:number.of.components)
        if (components[[i]]$type != "text") {
            components[[i]]$pars <-
                lapply(components[[i]]$pars, rep, length = max.length)
            components[[i]]$length <- max.length
        }
        else{
            ## NB: rep doesn't work with expressions of length > 1
            components[[i]]$pars <-
                c(components[[i]]$pars[1],
                  lapply(components[[i]]$pars[-1], rep,
                         length = components[[i]]$length))
        }

    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)

    if (column.blocks > max.length) warning("not enough rows for columns")
    
    key$between <- rep(key$between, length = number.of.components)

    
    if (key$align) {

        ## Setting up the layout


	## The problem of allocating space for text (character strings
	## or expressions) is dealt with as follows: 

	## Each row and column will take exactly as much space as
	## necessary. As steps in the construction, a matrix
	## textMatrix (of same dimensions as the layout) will contain
	## either 0, meaning that entry is not text, or n > 0, meaning
	## that entry has the text given by textList[[n]], where
	## textList is a list consisting of character strings or
	## expressions.



        n.row <- rows.per.block + 1
        n.col <- column.blocks * (1 + 3 * number.of.components) - 1

	textMatrix <- matrix(0, n.row, n.col)
	textList <- list()
	textCex <- numeric(0)

        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- as.list(1:n.row)

        if (key$title != "" && is.characterOrExpression(key$title)) {
            heights.x[1] <- 1.2 * key$cex.title
            heights.units[1] <- "strheight"
            heights.data[[1]] <- key$title
        }
        else heights.x[1] <- 0


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
    
        
	index <- 1

        for (i in 1:number.of.components) {

            cur <- components[[i]]

            id <- (1:column.blocks - 1) *
                (number.of.components * 3 + 1) + i * 3 - 1

            if (cur$type == "text") {

                for (j in 1:cur$length) {

                    colblck <- ceiling(j / rows.per.block)

                    xx <- (colblck - 1) *
                        (number.of.components * 3 + 1) + i * 3 - 1

                    yy <- j %% rows.per.block + 1
                    if (yy == 1) yy <- rows.per.block + 1

		    textMatrix[yy, xx] <- index
		    textList <- c(textList, list(cur$pars$labels[j]) )
		    textCex <- c(textCex, cur$pars$cex[j])
  		    index <- index + 1

		}


            } ## FIXME: do the same as above for those below
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


        ## Need to adjust the heights and widths 
        
        ## adjusting heights
        heights.insertlist.position <- 0
        heights.insertlist.unit <- unit(1, "null")

        for (i in 1:n.row) {
            textLocations <- textMatrix[i,]
            textLocations <- textLocations[textLocations>0]
            if (any(textLocations)) {

                strbar <- textList[textLocations]
                heights.insertlist.position <- c(heights.insertlist.position, i)
                heights.insertlist.unit <-
                    unit.c(heights.insertlist.unit,
                           unit(.2, "lines") + max(unit(textCex[textLocations], "strheight", strbar)))
            }
        }


        layout.heights <- unit(heights.x, heights.units, data=heights.data)
        if (length(heights.insertlist.position)>1)
            for (indx in 2:length(heights.insertlist.position))
                layout.heights <-
                    rearrangeUnit(layout.heights, heights.insertlist.position[indx],
                                  heights.insertlist.unit[indx])





        ## adjusting widths
        widths.insertlist.position <- 0
        widths.insertlist.unit <- unit(1, "null")




        for (i in 1:n.col) {
            textLocations <- textMatrix[,i]
            textLocations <- textLocations[textLocations>0]
            if (any(textLocations)) {

                strbar <- textList[textLocations]
                widths.insertlist.position <- c(widths.insertlist.position, i)
                widths.insertlist.unit <-
                    unit.c(widths.insertlist.unit,
                           max(unit(textCex[textLocations], "strwidth", strbar)))
            }
        }


        layout.widths <- unit(widths.x, widths.units, data=widths.data)
        if (length(widths.insertlist.position)>1)
            for (indx in 2:length(widths.insertlist.position))
                layout.widths <-
                    rearrangeUnit(layout.widths, widths.insertlist.position[indx],
                                  widths.insertlist.unit[indx])


        key.layout <- grid.layout(nrow = n.row, ncol = n.col,
                                  widths = layout.widths,
                                  heights = layout.heights,
                                  respect = FALSE)

        ## OK, layout set up, now to draw the key - no

        
        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize = default.fontsize),
                             draw = FALSE)

        if (!key$transparent) {
            grid.place(key.gf,
                       grid.rect(gp=gpar(fill = key$background, col = key$border),
                                 draw = FALSE),
                       draw = FALSE, row = NULL, col = NULL)
        }
        else
            grid.place(key.gf,
                       grid.rect(gp=gpar(col=key$border), draw = FALSE),
                       draw = FALSE, row = NULL, col = NULL)

        ## Title
        if (!is.null(key$title))
            grid.place(key.gf, 
                       grid.text(label = key$title, draw = FALSE,
                                 gp = gpar(fontsize = default.fontsize * key$cex.title)),
                       row=1, col = NULL, draw = FALSE)
        

        
        for (i in 1:number.of.components) {

            cur <- components[[i]]

            for (j in 1:cur$length) {

                colblck <- ceiling(j / rows.per.block)

                xx <- (colblck - 1) *
                    (number.of.components*3 + 1) + i*3 - 1

                yy <- j %% rows.per.block + 1
                if (yy == 1) yy <- rows.per.block + 1

                if (cur$type == "text") {
                    
                    grid.place(key.gf, 
                               grid.text(x = cur$pars$adj[j],
                                         just = c(
                                         if (cur$pars$adj[j] == 1) "right"
                                         else if (cur$pars$adj[j] == 0) "left"
                                         else "center",
                                         "center"),
                                         label = cur$pars$labels[j],
                                         gp = gpar(col = cur$pars$col[j],
                                         font = cur$pars$font[j],
                                         fontsize = default.fontsize * cur$pars$cex[j]),
                                         draw = FALSE),
                               row = yy, col = xx, draw = FALSE)
                    
                }
                else if (cur$type == "rectangles") {
                    grid.place(key.gf, 
                              grid.rect(width = cur$pars$size[j]/max(cur$pars$size),
                                        ## centred, unlike Trellis, due to aesthetic reasons !
                                        gp = gpar(fill = cur$pars$col[j]), 
                                        draw = FALSE),
                              row = yy, col = xx, draw = FALSE)
                    
                    ## Need to make changes to support angle/density
                }
                else if (cur$type == "lines") {
                    if (cur$pars$type[j] == "l") {
                        grid.place(key.gf,
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
                        grid.place(key.gf,
                                   grid.points(x=.5, y=.5, 
                                               gp = gpar(col = cur$pars$col[j]),
                                               size = unit(cur$pars$cex[j] * 2.5, "mm"),
                                               pch = cur$pars$pch[j],
                                               draw = FALSE),
                                   row = yy, col = xx, draw = FALSE)
                    }
                    else { # if (cur$pars$type[j] == "b" or "o") -- not differentiating
                        grid.place(key.gf, 
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

                        grid.place(key.gf, 
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
                    if (is.character(cur$pars$pch[j]))
                        grid.place(key.gf, 
                                  grid.text(lab = cur$pars$pch[j], x=.5, y=.5, 
                                            gp = gpar(col = cur$pars$col[j],
                                            fontsize = cur$pars$cex[j] * 10),
                                            draw = FALSE),
                                  row = yy, col = xx, draw = FALSE)
                    else {
                        grid.place(key.gf,
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
                 space = "right",
                 ...)
        {
            list(col = col,
                 at = at,
                 tick.number = tick.number,
                 width = width,
                 height = height,
                 space = space,
                 ...)
        }

    default.fontsize <- trellis.par.get("fontsize")$default
    key <- do.call("process.key", key)
    
    ## Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    if (length(key$at)!=length(key$col)+1) stop("length(col) must be length(at)-1")
    atrange <- range(key$at)
    scat <- .5 - key$height/2 + key$height *
        (key$at - atrange[1]) / diff(atrange)
    recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex <- 0.9
    col <- "black"
    font <- 1
    if (is.null(key$lab)) {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at>=atrange[1] & at<=atrange[2]]
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
    else stop("malformed colorkey")
    labscat <- .5 - key$height/2 + key$height *
        (at - atrange[1]) / diff(atrange) # scales tick positions

    which.name <- "W"
    for (ss in labels)
        if (nchar(ss) > nchar(which.name)) which.name <- ss


    ## the tick marks for left and right should be modified


    if (key$space == "right") {

        widths.x <- c(key$width,1)
        widths.units <- rep("strwidth", 2)
        widths.data <- as.list(c("W", paste("--",which.name)))
        
        key.layout <-
            grid.layout(nrow = 1, ncol = 2,
                        widths = unit(widths.x, widths.units, data=widths.data))
        
        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize = default.fontsize),
                             draw = FALSE)
        

######

        grid.pack(frame = key.gf, row = 1, col = 1,
                  grob =
                  grid.rect(x = rep(.5, length(reccentre)), 
                            y = reccentre, default.units = "native",
                            height = recdim, 
                            gp=gpar(fill=key$col,  col = NULL), draw = FALSE),
                  draw = FALSE)

        grid.pack(frame = key.gf, col = 1,
                  grob =
                  grid.rect(height = key$height,
                            gp=gpar(col="black"), draw = FALSE),
                  draw = FALSE)
        
        grid.pack(frame = key.gf, col = 1,
                  grob =
                  grid.text(label = paste("-", labels, sep = ""),
                            x = rep(1, length(labscat)),
                            y = labscat,
                            just = c("left","center"),
                            gp=gpar(col = col, fontsize = cex * default.fontsize, font = font),
                            draw = FALSE),
                  draw = FALSE)
    }
    else if (key$space == "left") {

        widths.x <- c(1,key$width)
        widths.units <- rep("strwidth", 2)
        widths.data <- as.list(c(paste("--",which.name), "W"))
        
        key.layout <-
            grid.layout(nrow = 1, ncol = 2,
                        widths = unit(widths.x, widths.units, data=widths.data))
        
        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize = default.fontsize), 
                             draw = FALSE)
        
        grid.pack(frame = key.gf, row = 1, col = 2,
                  grob =
                  grid.rect(x = rep(.5, length(reccentre)),
                            y = unit(reccentre, "native"),
                            height = unit(recdim, "native"),
                            gp=gpar(fill=key$col,  col = NULL), draw = FALSE),
                  draw = FALSE)

        grid.pack(frame = key.gf, col = 2,
                  grob =
                  grid.rect(height = key$height,
                            gp=gpar(col="black"), draw = FALSE),
                  draw = FALSE)
        
        grid.pack(frame = key.gf, col = 2,
                  grob =
                  grid.text(label = paste(labels, "-", sep = ""),
                            x = rep(0, length(labscat)),
                            y = labscat,
                            just = c("right","center"),
                            gp=gpar(col = col, fontsize = cex * default.fontsize, font = font),
                            draw = FALSE),
                  draw = FALSE)
    }
    else if (key$space == "top") {

        heights.x <- c(1, .2, key$width)
        heights.units <- c("lines", "lines", "strwidth")
        heights.data <- as.list(c("a", "a", "W"))
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 1,
                        heights = unit(heights.x, heights.units, data=heights.data))
        
        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize = default.fontsize), 
                             draw = FALSE)
        
        grid.pack(frame = key.gf, row = 3, col = 1,
                  grob =
                  grid.rect(x = unit(reccentre, "native"),
                            y = rep(.5, length(reccentre)),
                            width = unit(recdim, "native"),
                            gp=gpar(fill=key$col,  col = NULL), draw = FALSE),
                  draw = FALSE)

        grid.pack(frame = key.gf, row = 3,
                  grob =
                  grid.rect(width = key$height,
                            gp=gpar(col="black"), draw = FALSE),
                  draw = FALSE)

        for (c in seq(along = labscat))
            grid.pack(frame = key.gf, row = 2,
                      grob =
                      grid.lines(x = unit(rep(labscat[c], 2), "native"),
                                 gp=gpar(col = col),
                                 draw = FALSE),
                                 draw = FALSE)

        grid.pack(frame = key.gf, row = 1,
                  grob =
                  grid.text(label = labels,
                            x = labscat,
                            just = c("centre","center"),
                            gp=gpar(col = col, fontsize = cex * default.fontsize, font = font),
                            draw = FALSE),
                  draw = FALSE)
    }
    else if (key$space == "bottom") {

        heights.x <- c(key$width, .2, 1)
        heights.units <- c("strwidth", "lines", "lines")
        heights.data <- as.list(c("W", "a", "a"))
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 1,
                        heights = unit(heights.x, heights.units, data=heights.data))
        
        key.gf <- grid.frame(layout = key.layout, vp = vp,
                             gp = gpar(fontsize = default.fontsize),
                             draw = FALSE)
        
        grid.pack(frame = key.gf, row = 1, col = 1,
                  grob =
                  grid.rect(x = unit(reccentre, "native"),
                            y = rep(.5, length(reccentre)),
                            width = unit(recdim, "native"),
                            gp = gpar(fill=key$col, col = NULL), draw = FALSE),
                  draw = FALSE)

        grid.pack(frame = key.gf, row = 1,
                  grob =
                  grid.rect(width = key$height,
                            gp = gpar(col="black"), draw = FALSE),
                  draw = FALSE)

        for (c in seq(along = labscat))
            grid.pack(frame = key.gf, row = 2,
                      grob =
                      grid.lines(x = unit(rep(labscat[c], 2), "native"),
                                 gp=gpar(col = col),
                                 draw = FALSE),
                                 draw = FALSE)

        grid.pack(frame = key.gf, row = 3,
                  grob =
                  grid.text(label = labels,
                            x = labscat,
                            just = c("centre","center"),
                            gp=gpar(col = col, fontsize = cex * default.fontsize, font = font),
                            draw = FALSE),
                  draw = FALSE)
    }
    





    if (draw)
        grid.draw(key.gf)
    
    key.gf
}





















print.trellis <-
    function(x, position, split, more = FALSE,
             newpage = TRUE,
             panel.height = list(1, "null"),
             panel.width = list(1, "null"),
             ...)
{
    if (is.null(dev.list())) trellis.device()
    else if (is.null(trellis.par.get()))
        trellis.device(device = .Device, new = FALSE)
    bg = trellis.par.get("background")$col
    new <- TRUE
    if (.lattice.print.more || !newpage) new <- FALSE
    .lattice.print.more <<- more
    usual  <- (missing(position) & missing(split))
    ##if (!new && usual)
    ##    warning("more is relevant only when split/position is specified")

    fontsize.default <- trellis.par.get("fontsize")$default
    
    if (!missing(position)) {
        if (length(position)!=4) stop("Incorrect value of position")
        if (new) {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
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
        if (new) {
            grid.newpage()
            grid.rect(gp = gpar(fill = bg, col = "transparent"))
        }
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
    
    ##panel.width <- 1
    layout.respect <- !x$aspect.fill
    if (layout.respect) panel.height[[1]] <-
        x$aspect.ratio * panel.width[[1]]

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
        key.space <- 
            if ("space" %in% names(x$colorkey)) x$colorkey$space
            else "right"
    }

    xaxis.col <-
        if (is.logical(x$x.scales$col)) axis.line$col
        else x$x.scales$col
    xaxis.font <-
        if (is.logical(x$x.scales$font)) 1
        else x$x.scales$font
    xaxis.cex <-
        x$x.scales$cex
    xaxis.rot <-
        if (is.logical(x$x.scales$rot)) c(0, 0)
        else x$x.scales$rot
    yaxis.col <-
        if (is.logical(x$y.scales$col)) axis.line$col
        else x$y.scales$col
    yaxis.font <-
        if (is.logical(x$y.scales$font)) 1
        else x$y.scales$font
    yaxis.cex <-
        x$y.scales$cex
    yaxis.rot <-
        if (!is.logical(x$y.scales$rot)) x$y.scales$rot
        else if (x$y.scales$relation != "same" && is.logical(x$y.scales$labels)) c(90, 90)
        else c(0, 0)

    strip.col.default.bg <-
        rep(trellis.par.get("strip.background")$col,length=number.of.cond)
    strip.col.default.fg <-
        rep(trellis.par.get("strip.shingle")$col,length=number.of.cond)


    cond.max.level <- integer(number.of.cond)
    for(i in 1:number.of.cond) {
        cond.max.level[i] <- length(x$condlevels[[i]])
    }

    if(x$layout[1]==0) { # using device dimensions to
        ddim <- par("din") # calculate default layout
        device.aspect <- ddim[2]/ddim[1]
        panel.aspect <- panel.height[[1]]/panel.width[[1]]

        plots.per.page <- x$layout[2]
        m <- max (1, round(sqrt(x$layout[2] * device.aspect/panel.aspect)))
        ## changes made to fix bug (PR#1744)
        n <- ceiling(plots.per.page/m)
        m <- ceiling(plots.per.page/n)
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



    have.main <- !(is.null(x$main$label) || (is.character(x$main$label) && x$main$label==""))
    have.sub <- !(is.null(x$sub$label)   || (is.character(x$sub$label) && x$sub$label==""))
    have.xlab <- !(is.null(x$xlab$label) || (is.character(x$xlab$label) && x$xlab$label==""))
    have.ylab <- !(is.null(x$ylab$label) || (is.character(x$ylab$label) && x$ylab$label==""))

    
    ## Shall calculate the per page layout now:

    ## The idea here is to create a layout with proper widths and
    ## heights (representing the requisite amounts of space required
    ## for different components of the plot -- see descriptions below)
    ## using the various units available in grid.

    ## Most of these components are fairly easy to define, with one
    ## exception -- namely those that involve axis labels. For
    ## instance, one (or more) _columns_ would usually contain the
    ## y-axis tick-labels. The width of this column is determined by
    ## ALL the y-labels; basically, the width of the column would be
    ## the MAXIMUM of the widths of the individual labels.

    ## This is in general not an easy problem, since relative width
    ## depends on the font used (also perhaps the device). Till
    ## lattice version 0.6, this was dealt with naively by treating
    ## the label with highest nchar() to be the widest. Unfortunately,
    ## this was no longer possible with labels that were
    ## expressions. So, after grid.text started supporting expression
    ## markups, the method of determining widths/heights for tick
    ## labels has changed. The new method essentially calculates the
    ## MAXIMUM of several grid UNIT objects (using calls like
    ## max(unit(...))) .

    ## The problem with this is that it is impossible to define the
    ## 'units' argument of those parts of the eventual layout when
    ## it's first being defined (it is not "null", "lines" or anything
    ## like that). So, those parts are calculated as separate units
    ## (via max.unit) and then inserted into the layout later.

    ## All this makes the code a bit difficult to follow. I just hope
    ## this gives some hints to whoever (probably me!) tries to
    ## decipher the following code on some later date.

    
    n.row <- rows.per.page * (number.of.cond + 3) + (rows.per.page-1) + 11
    ##       ^^^^^^^^^^^      ^^^^^^^^^^^^^^^^       ^^^^^^^^^^^^^^^    ^^
    ##          panels         rows per panel           between     see below
    ##               (2 for axes/ticks when relation!=same)

    ## the 11 things are as follows (top to bottom)
    ## 1/2 line space at top
    ## main
    ## key
    ## tick labels
    ## ticks
    ##
    ##   actual panels
    ##
    ## ticks
    ## tick labels
    ## xlab
    ## key
    ## sub
    ## 1/2 line space at bottom

    n.col <- 3 * cols.per.page + (cols.per.page-1) + 9 # similar

    ## the 9 things are as follows (left to right)
    ## 1/2 line space at left
    ## key
    ## ylab
    ## tick labels
    ## ticks
    ##
    ##   actual panels
    ##
    ## ticks
    ## tick labels
    ## key
    ## 1/2 line space at right

    ## The next block applies when aspect is anything other than
    ## "fill", which means that the aspect ratio of panels are
    ## fixed. In grid terms, this means that the 'respect' argument
    ## has to be true for elements of the layout that correspond to
    ## panels.

    ## Earlier code used to set all respect entries to be TRUE in such
    ## cases (no need for matrices then), but that fails with the
    ## complicated layout necessitated by expressions (see above).

    if (layout.respect) {
        layout.respect <- matrix(0, n.row, n.col)

        layout.respect[number.of.cond + 6 + (1:rows.per.page - 1) *
                       (number.of.cond+4), (1:cols.per.page - 1)*4 +
                       8] <- 1

    }

    ## see ?unit before trying to follow this. 

    heights.x <- rep(1, n.row)
    heights.units <- rep("lines", n.row)
    heights.data <- as.list(1:n.row)

    widths.x <- rep(1, n.col)
    widths.units <- rep("lines", n.col)
    widths.data <- as.list(1:n.col) 

    ## fine tuning heights:


    heights.x[number.of.cond + 6 + (1:rows.per.page - 1) * (number.of.cond+4)] <-
        panel.height[[1]] # for the panels
    heights.units[number.of.cond + 6 + (1:rows.per.page - 1) * (number.of.cond+4)] <-
        panel.height[[2]]
    ## was "null" # for the panels

    heights.x[number.of.cond + 7 + (1:rows.per.page - 1) * (number.of.cond+4)] <- 0
    ## This is for the x-axis ticks just below each panel if relation!="same"
    heights.x[number.of.cond + 8 + (1:rows.per.page - 1) * (number.of.cond+4)] <- 0
    ## This is for the x-axis labels just below each panel if relation!="same"

    heights.x[4] <- 0
    heights.x[5] <- 0 # tick axes/labels
    heights.x[n.row-4] <- 0
    heights.x[n.row-5] <- 0



    if (rows.per.page > 1)
        heights.x[number.of.cond + 9 +
                  ((if (x$as.table) 1:(rows.per.page-1)
                  else (rows.per.page-1):1)
                   - 1)*(number.of.cond+4)] <-
                       y.between
    ## y-between


    heights.x[1] <- 0.5
    heights.x[2] <- if (have.main) 2 * x$main$cex else 0
    if (have.main) {
        heights.units[2] <-  "strheight"
        heights.data[[2]] <- x$main$lab
    }



    heights.x[n.row] <- 0.5
    heights.x[n.row-1] <- if (have.sub) 2 * x$sub$cex else 0
    if (have.sub) {
        heights.units[n.row-1] <-  "strheight"
        heights.data[[n.row-1]] <- x$sub$lab
    }

    heights.x[3] <- 0 # for the key
    heights.x[n.row-2] <- 0 # key

    ## next part of the code decides how much space to leave for
    ## x tick-labels. This wasn't that bad earlier, but has become
    ## complicated to support expression-style labels. Not sure if
    ## there's a better way (would definitely need a lot of
    ## redesigning), something to look at later.

    heights.insertlist.position <- 0
    heights.insertlist.unit <- unit(1, "null")
    ## both these dummies, since there is no unit(numeric(0)). These
    ## are necessary for calculating space for axis
    ## labels. Unfortunately this makes the code complicated


    if (x$x.scales$draw) {

        if (x.relation.same) {

            strbar <- 
                as.list(calculateAxisComponents(x = x$x.limits,
                                                at = x$x.scales$at,
                                                labels = x$x.scales$lab,
                                                have.log = have.xlog,
                                                logbase = xlogbase,
                                                logpaste = xlogpaste,
                                                abbreviate = x$x.scales$abbr,
                                                minlength = x$x.scales$minl,
                                                n = x$x.scales$tick.number)$lab)

            #for (ss in seq(along = x$x.scales$labels))
            #    strbar <- c(strbar, list(x$x.scales$labels[ss]))

            heights.x[5] <- 0.5 + max(0.001, x$x.scales$tck[2]) * 0.3
            ## tck = 2 is .5 lines + .6 lines
            heights.x[n.row-5] <- 0.5 + max(0.001, x$x.scales$tck[1]) * 0.3

            if (any(x.alternating==2 | x.alternating==3)) {

                if (xaxis.rot[2]  %in% c(0, 180)) {

                    heights.insertlist.position <- c(heights.insertlist.position, 4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[2],
                                            length(strbar)), "strheight", strbar)))
                }
                else {
                    heights.insertlist.position <- c(heights.insertlist.position, 4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[2] * abs(sin(xaxis.rot[2] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }

            if (any(x.alternating==1 | x.alternating==3)) {

                if (xaxis.rot[1]  %in% c(0, 180)) {
                    
                    heights.insertlist.position <- c(heights.insertlist.position, n.row-4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))
                }

                else {

                    heights.insertlist.position <- c(heights.insertlist.position, n.row-4)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.0 * xaxis.cex[1] * abs(sin(xaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }
        }
        else { # relation != same


            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different heights for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. If at is a list, have to go through all (for
            ## each panel). If not, still have to go through
            ## all. Could save some work if at is explicitly
            ## specified, but ignoring that for now.


            labelChars <- character(0)
            labelExprs <- expression(0)
            for (i in seq(along = x$x.limits)) {
                lab <-
                    calculateAxisComponents(x = x$x.limits[[i]],
                                            at = if (is.list(x$x.scales$at)) x$x.scales$at[[i]] else x$x.scales$at,
                                            labels = if (is.list(x$x.scales$lab)) x$x.scales$lab[[i]] else x$x.scales$lab,
                                            have.log = have.xlog,
                                            logbase = xlogbase,
                                            logpaste = xlogpaste,
                                            abbreviate = x$x.scales$abbr,
                                            minlength = x$x.scales$minl,
                                            n = x$x.scales$tick.number)$lab
                if (is.character(lab)) 
                    labelChars <- c(labelChars, lab)
                else if (is.expression(lab))
                    labelExprs <- c(labelExprs, lab)
            }
            labelChars <- unique(labelChars)

            strbar <- list() ## will contain list for max.unit data
            for (ss in labelChars)
                strbar <- c(strbar, list(ss))
            for (ss in seq(along = labelExprs))
                strbar <- c(strbar, list(labelExprs[ss]))

            if (xaxis.rot[1] %in% c(0, 180)) {

                heights.x[number.of.cond + 7 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                    max(0.001, x$x.scales$tck[1]) * 0.3  ## tck = 1 -- 0.3 lines

                heights.insertlist.position <-
                    c(heights.insertlist.position,
                      number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4))
                for (i in 1:rows.per.page)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.5 * xaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))

            }
            else {

                heights.x[number.of.cond + 7 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                    max(0.001, x$x.scales$tck[1]) * 0.3

                ##if (is.logical(x$x.scales$at)) {
                ##    heights.x[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <-
                ##        1.1 * xaxis.cex * abs(sin(xaxis.rot * pi /180))
                ##    heights.units[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <- "strwidth"
                ##    heights.data[number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4)] <- which.name
                ##}
                ##else {
                heights.insertlist.position <-
                    c(heights.insertlist.position,
                      number.of.cond + 8 + (1:rows.per.page - 1)*(number.of.cond+4))
                for (i in 1:rows.per.page)
                    heights.insertlist.unit <-
                        unit.c(heights.insertlist.unit,
                               max(unit(rep(1.5 * xaxis.cex[1] * abs(sin(xaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                ##}
            }
        }
    }

    heights.x[n.row-3] <- if (have.xlab) 2 * x$xlab$cex else 0 # xlab
    if (have.xlab) {
        heights.units[n.row-3] <-  "strheight"
        heights.data[[n.row-3]] <- x$xlab$lab
    }

    ## this is if strip=F -- strips not to be drawn
    for(crr in 1:number.of.cond)
        heights.x[number.of.cond + 6 + (1:rows.per.page - 1)*(number.of.cond+4) - crr] <-
            if (is.logical(x$strip)) 0  # which means strip = F, strips not to be drawn
            else 1.1 * x$par.strip.text$cex * x$par.strip.text$lines

    ## fine tuning widths:
    ##----------------------------------------------------------------------------------

    widths.x[3] <- if (have.ylab) 2 * x$ylab$cex else 0 # ylab
    if (have.ylab) {
        widths.units[3] <-  "strheight"
        widths.data[[3]] <- x$ylab$lab
    }


    widths.x[(1:cols.per.page - 1)*4 + 8] <-
        panel.width[[1]] # for the panels
    widths.units[(1:cols.per.page - 1)*4 + 8] <-
        panel.width[[2]] # for the panels
    ## was "null"


    widths.x[(1:cols.per.page - 1)*4 + 7] <- 0
    widths.x[(1:cols.per.page - 1)*4 + 6] <- 0
    ## For y-axes labels and ticks to the left of each panel when relation != "same"
    ## (might change later)

    widths.x[4] <- 0
    widths.x[5] <- 0 #ticks/labels
    widths.x[n.col-2] <- 0
    widths.x[n.col-3] <- 0

    if (cols.per.page > 1)
        widths.x[(1:(cols.per.page-1) - 1)*4 + 9] <- x.between
    ## x-between

    widths.x[1] <- 0.5
    widths.x[n.col] <- 0.5
    widths.x[2] <- 0 # key - left
    widths.x[n.col-1] <- 0 # key - right

    ## next part of the code decides how much space to leave for y
    ## tick-labels. This wasn't that bad earlier, but has become
    ## complicated to support expression-style labels. Not sure if
    ## there's a better way (would definitely need a lot of
    ## redesigning), something to look at later.

    widths.insertlist.position <- 0
    widths.insertlist.unit <- unit(1, "null")
    ## both these dummies, since there is no unit(numeric(0)). These
    ## are necessary for calculating space for axis
    ## labels. Unfortunately this makes the code complicated

    if (x$y.scales$draw) {
        
        if (y.relation.same) {

            strbar <- 
                as.list(calculateAxisComponents(x = x$y.limits,
                                                at = x$y.scales$at,
                                                labels = x$y.scales$lab,
                                                have.log = have.ylog,
                                                logbase = ylogbase,
                                                logpaste = ylogpaste,
                                                abbreviate = x$y.scales$abbr,
                                                minlength = x$y.scales$minl,
                                                n = x$y.scales$tick.number)$lab)

            ##strbar <- list() ## will contain list for max.unit data
            ##for (ss in seq(along = x$y.scales$labels))
            ##    strbar <- c(strbar, list(x$y.scales$labels[ss]))

            widths.x[5] <- 0.5 + max(0.001, x$y.scales$tck[1]) * 0.3
            ## tck = 2 is .5 lines + .6 lines
            widths.x[n.col-3] <- max(1, x$y.scales$tck[2]) * 0.5

            if (any(y.alternating==1 | y.alternating==3)) {

                if (abs(yaxis.rot[1]) == 90) {

                    widths.insertlist.position <- c(widths.insertlist.position, 4)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(1.0 * rep(yaxis.cex[1],
                                                  length(strbar)), "strheight", strbar)))
                }
                
                else {

                    widths.insertlist.position <- c(widths.insertlist.position, 4)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[1] * abs(cos(yaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                }
            }

            if (any(y.alternating==2 | y.alternating==3)) {

                if (abs(yaxis.rot[2]) == 90) {
                    widths.insertlist.position <- c(widths.insertlist.position, n.col-2)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[2],
                                            length(strbar)), "strheight", strbar)))
                }

                else {
                    widths.insertlist.position <- c(widths.insertlist.position, n.col-2)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.0 * yaxis.cex[2] * abs(cos(yaxis.rot[2] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                    
                }
            }
        }
        else { # relation != same

            ## See comments for x-scales above
            
            labelChars <- character(0)
            labelExprs <- expression(0)
            for (i in seq(along = x$y.limits)) {
                lab <-
                    calculateAxisComponents(x = x$y.limits[[i]],
                                            at = if (is.list(x$y.scales$at)) x$y.scales$at[[i]] else x$y.scales$at,
                                            labels = if (is.list(x$y.scales$lab)) x$y.scales$lab[[i]] else x$y.scales$lab,
                                            have.log = have.ylog,
                                            logbase = ylogbase,
                                            logpaste = ylogpaste,
                                            abbreviate = x$y.scales$abbr,
                                            minlength = x$y.scales$minl,
                                            n = x$y.scales$tick.number)$lab
                if (is.character(lab)) 
                    labelChars <- c(labelChars, lab)
                else if (is.expression(lab))
                    labelExprs <- c(labelExprs, lab)
            }
            labelChars <- unique(labelChars)

            strbar <- list() ## will contain list for max.unit data
            for (ss in labelChars)
                strbar <- c(strbar, list(ss))
            for (ss in seq(along = labelExprs))
                strbar <- c(strbar, list(labelExprs[ss]))


            if (abs(yaxis.rot[1]) == 90) {

                widths.x[(1:cols.per.page - 1)*4 + 7] <- 
                    max(0.001, x$y.scales$tck[1]) * 0.3  ## tck = 1 -- 0.3 lines

                widths.insertlist.position <-
                    c(widths.insertlist.position,
                      (1:cols.per.page - 1) * 4 + 6)
                for (i in 1:cols.per.page)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.5 * yaxis.cex[1],
                                            length(strbar)), "strheight", strbar)))

            }
            else {

                widths.x[(1:cols.per.page - 1)*4 + 7] <- 
                    max(0.001, x$y.scales$tck[1]) * 0.3

                ##if (is.logical(x$y.scales$at)) {
                ##    widths.x[(1:cols.per.page - 1)*4 + 6] <-
                ##        1.1 * yaxis.cex * abs(cos(yaxis.rot * pi /180))
                ##    widths.units[(1:cols.per.page - 1)*4 + 6] <- "strwidth"
                ##    widths.data[(1:cols.per.page - 1)*4 + 6] <- which.name
                #3}
                ##else {
                widths.insertlist.position <-
                    c(widths.insertlist.position, (1:cols.per.page - 1)*4 + 6)
                for (i in 1:cols.per.page)
                    widths.insertlist.unit <-
                        unit.c(widths.insertlist.unit,
                               max(unit(rep(1.2 * yaxis.cex[1] * abs(cos(yaxis.rot[1] * base::pi /180)),
                                            length(strbar)), "strwidth", strbar)))
                ##}
            }
        }
    }


    if (!is.null(x$key) || !is.null(x$colorkey)) {
            
        if (key.space == "left") {
            widths.x[2] <- 1.2
            widths.units[2] <- "grobwidth"
            widths.data[[2]] <- key.gf
        }
        else if (key.space == "right") {
            widths.x[n.col-1] <- 1.2
            widths.units[n.col-1] <- "grobwidth"
            widths.data[[n.col-1]] <- key.gf
        }
        else if (key.space == "top") {
            heights.x[3] <- 1.2
            heights.units[3] <- "grobheight"
            heights.data[[3]] <- key.gf
        }
        else if (key.space == "bottom") {
            heights.x[n.row-2] <- 1.2
            heights.units[n.row-2] <- "grobheight"
            heights.data[[n.row-2]] <- key.gf
        }
        
    }
    

    ## Constructing the layout:

    layout.heights <- unit(heights.x, heights.units, data=heights.data)
    if (length(heights.insertlist.position)>1)
        for (indx in 2:length(heights.insertlist.position))
            layout.heights <-
                rearrangeUnit(layout.heights, heights.insertlist.position[indx],
                              heights.insertlist.unit[indx])
    

    layout.widths <- unit(widths.x, widths.units, data=widths.data)
    if (length(widths.insertlist.position)>1)
        for (indx in 2:length(widths.insertlist.position))
            layout.widths <-
                rearrangeUnit(layout.widths, widths.insertlist.position[indx],
                              widths.insertlist.unit[indx])
    
    page.layout <- grid.layout(nrow = n.row, ncol = n.col,
                               widths = layout.widths,
                               heights = layout.heights,
                               respect = layout.respect)

        
    cond.current.level <- rep(1,number.of.cond)
    panel.number <- 1
        
    for(page.number in 1:number.of.pages)
        if (!any(cond.max.level-cond.current.level<0)) {
                
            if(usual) {
                if (new) grid.newpage()
                grid.rect(gp = gpar(fill = bg, col = "transparent"))
                new <- TRUE
            }

            push.viewport(viewport(layout = page.layout,
                                   gp = gpar(fontsize = fontsize.default,
                                   col = axis.line$col,
                                   lty = axis.line$lty,
                                   lwd = axis.line$lwd)))

            if (have.main)
                grid.text(label = x$main$label,
                          gp = gpar(col = x$main$col, font = x$main$font, 
                          fontsize = fontsize.default * x$main$cex),
                          vp = viewport(layout.pos.row = 2))
                    
                    
            if (have.sub)
                grid.text(label = x$sub$label,
                          gp = gpar(col = x$sub$col, font = x$sub$font, 
                          fontsize = fontsize.default * x$sub$cex),
                          vp = viewport(layout.pos.row = n.row-1))
                    
                    
            if (have.xlab) 
                grid.text(label = x$xlab$label,
                          gp = gpar(col = x$xlab$col, font = x$xlab$font, 
                          fontsize = fontsize.default * x$xlab$cex), 
                          vp = viewport(layout.pos.row = n.row - 3, layout.pos.col = c(6, n.col - 4)))
                
            if (have.ylab)
                grid.text(label = x$ylab$label, rot = 90,
                          gp = gpar(col = x$ylab$col, font = x$ylab$font, 
                          fontsize = fontsize.default * x$ylab$cex),
                          vp = viewport(layout.pos.col = 3, layout.pos.row = c(6, n.row - 6)))
            
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


                            pos.row <- 6 + number.of.cond + 
                                (rows.per.page - actual.row) *
                                (number.of.cond + 4)
                            pos.col <- (column-1) * 4 + 8


                            xlabelinfo <-
                                calculateAxisComponents(x =
                                                        if (x.relation.same) x$x.limits
                                                        else x$x.limits[[panel.number]],
                                                        at =
                                                        if (is.list(x$x.scales$at)) x$x.scales$at[[panel.number]]
                                                        else x$x.scales$at,
                                                        labels =
                                                        if (is.list(x$x.scales$lab)) x$x.scales$lab[[panel.number]]
                                                        else x$x.scales$lab,
                                                        have.log = have.xlog,
                                                        logbase = xlogbase,
                                                        logpaste = xlogpaste,
                                                        abbreviate = x$x.scales$abbr,
                                                        minlength = x$x.scales$minl,
                                                        n = x$x.scales$tick.number)

                            ylabelinfo <-
                                calculateAxisComponents(x =
                                                        if (y.relation.same) x$y.limits
                                                        else x$y.limits[[panel.number]],
                                                        at =
                                                        if (is.list(x$y.scales$at)) x$y.scales$at[[panel.number]]
                                                        else x$y.scales$at,
                                                        labels =
                                                        if (is.list(x$y.scales$lab)) x$y.scales$lab[[panel.number]]
                                                        else x$y.scales$lab,
                                                        have.log = have.ylog,
                                                        logbase = ylogbase,
                                                        logpaste = ylogpaste,
                                                        abbreviate = x$y.scales$abbr,
                                                        minlength = x$y.scales$minl,
                                                        n = x$y.scales$tick.number)



                            xscale <- xlabelinfo$num.limit
                            yscale <- ylabelinfo$num.limit
                                
                            push.viewport(viewport(layout.pos.row = pos.row,
                                                   layout.pos.col = pos.col,
                                                   xscale = xscale,
                                                   yscale = yscale,
                                                   clip = TRUE,
                                                   gp = gpar(fontsize =
                                                   fontsize.default)))


                            pargs <- c(x$panel.args[[panel.number]],
                                       x$panel.args.common,
                                       list(panel.number = panel.number))
                            if (!("..." %in% names(formals(panel))))
                                pargs <- pargs[names(formals(panel))]
                            do.call("panel", pargs)

                            grid.rect()

                            pop.viewport()

                            ## next few lines deal with drawing axes
                            ## as appropriate

                            ## when relation != same, axes drawn for
                            ## each panel:
                            
                            ## X-axis
                            if (!x.relation.same && x$x.scales$draw) {

                                axs <- x$x.scales

                                #if (is.logical(axs$at)) {
                                #    axs$at <- lpretty(xscale, n = axs$tick.number)
                                #    axs$labels <- paste(xlogpaste, as.character(axs$at), sep = "")
                                #}

                                ok <- (xlabelinfo$at>=xscale[1] & xlabelinfo$at<=xscale[2])

                                push.viewport(viewport(layout.pos.row = pos.row+1,
                                                       layout.pos.col = pos.col,
                                                       xscale = xscale))

                                
                                ##panel.fill(col = "yellow")
                                if (axs$tck[1] !=0 && any(ok))
                                    grid.segments(y0 = unit(rep(1, sum(ok)), "npc"),
                                                  y1 = unit(rep(1, sum(ok)), "npc") -
                                                  unit(rep(0.3 * axs$tck[1], sum(ok)), "lines"),
                                                  x0 = unit(xlabelinfo$at[ok], "native"),
                                                  x1 = unit(xlabelinfo$at[ok], "native"),
                                                  gp = gpar(col = xaxis.col))

                                # deepayan

                                pop.viewport()

                                if (any(ok))
                                    grid.text(label = xlabelinfo$label[ok],
                                              x = unit(xlabelinfo$at[ok], "native"),
                                              y = unit(if (xaxis.rot[1] %in% c(0, 180)) .5 else .95, "npc"),
                                              ##y = unit(.95, "npc"),
                                              just = if (xaxis.rot[1] == 0) c("centre", "centre")
                                              else if (xaxis.rot[1] == 180) c("centre", "centre")
                                              else if (xaxis.rot[1] > 0)  c("right", "centre")
                                              else c("left", "centre"),
                                              rot = xaxis.rot[1],
                                              check.overlap = xlabelinfo$check.overlap,
                                              gp = gpar(col = xaxis.col, font = xaxis.font, 
                                              fontsize = axs$cex[1] * fontsize.default),
                                              vp = viewport(layout.pos.row = pos.row+2,
                                              layout.pos.col = pos.col, xscale = xscale))

                            }
                            ## Y-axis
                            if (!y.relation.same && x$y.scales$draw) {

                                axs <- x$y.scales

                                #if (is.logical(axs$at)) {
                                #    axs$at <- lpretty(yscale, n = axs$tick.number)
                                #    axs$labels <- paste(ylogpaste, as.character(axs$at), sep = "")
                                #}

                                ok <- (ylabelinfo$at>=yscale[1] & ylabelinfo$at<=yscale[2])

                                push.viewport(viewport(layout.pos.row = pos.row,
                                                       layout.pos.col = pos.col-1,
                                                       yscale = yscale))


                                if (axs$tck[1] !=0 && any(ok))
                                    grid.segments(x0 = unit(rep(1, sum(ok)), "npc"),
                                                  x1 = unit(rep(1, sum(ok)), "npc") -
                                                  unit(rep(0.3 * axs$tck[1], sum(ok)), "lines"),
                                                  y0 = unit(ylabelinfo$at[ok], "native"),
                                                  y1 = unit(ylabelinfo$at[ok], "native"),
                                                  gp = gpar(col = yaxis.col))

                                pop.viewport()


                                if (any(ok))
                                    grid.text(label = ylabelinfo$label[ok],
                                              y = unit(ylabelinfo$at[ok], "native"),
                                              x = unit(if ( abs(yaxis.rot[1]) == 90) .5 else .95, "npc"),
                                              ##y = unit(.95, "npc"),
                                              just = if (yaxis.rot[1] == 90) c("centre", "centre")
                                              else if (yaxis.rot[1] == -90) c("centre", "centre")
                                              else if (yaxis.rot[1] > -90 && yaxis.rot[1] < 90) c("right", "centre")
                                              else c("left", "centre"),
                                              rot = yaxis.rot[1],
                                              check.overlap = ylabelinfo$check.overlap,
                                              gp = gpar(col = yaxis.col, font = xaxis.font, 
                                              fontsize = axs$cex[1] * fontsize.default),
                                              vp = viewport(layout.pos.row = pos.row,
                                              layout.pos.col = pos.col-2, yscale = yscale))

                            }

                            ## When relation = same, axes drawn based on value of alternating
                            if (y.relation.same && x$y.scales$draw) {
                                
                                ## Y-axis to the left
                                if (column == 1) {

                                    axs <- x$y.scales

                                    ok <- (ylabelinfo$at>=yscale[1] & ylabelinfo$at<=yscale[2])

                                    push.viewport(viewport(layout.pos.row = pos.row,
                                                           layout.pos.col = pos.col-3,
                                                           yscale = yscale))

                                    if (axs$tck[1] !=0 && any(ok))
                                        grid.segments(x0 = unit(rep(1, sum(ok)), "npc"),
                                                      x1 = unit(rep(1, sum(ok)), "npc") -
                                                      unit(rep(0.3 * axs$tck[1], sum(ok)), "lines"),
                                                      y0 = unit(ylabelinfo$at[ok], "native"),
                                                      y1 = unit(ylabelinfo$at[ok], "native"),
                                                      gp = gpar(col = yaxis.col))

                                    pop.viewport()

                                    if (y.alternating[actual.row]==1 || y.alternating[actual.row]==3) 

                                        if (any(ok)) 

                                            grid.text(label = ylabelinfo$lab[ok],
                                                      y = unit(ylabelinfo$at[ok], "native"),
                                                      x = unit(if (abs(yaxis.rot[1]) == 90) .5 else 1, "npc"),
                                                      ##y = unit(rep(.95, sum(ok)), "npc"),
                                                      just = if (yaxis.rot[1] == -90) c("centre", "centre")
                                                      else if (yaxis.rot[1] == 90) c("centre", "centre")
                                                      else if (yaxis.rot[1] > -90 && yaxis.rot[1] < 90)  c("right", "centre")
                                                      else c("left", "centre"),
                                                      rot = yaxis.rot[1],
                                                      check.overlap = ylabelinfo$check.overlap,
                                                      gp = gpar(col = yaxis.col, font = yaxis.font, 
                                                      fontsize = axs$cex[1] * fontsize.default),
                                                      vp = viewport(layout.pos.row = pos.row,
                                                      layout.pos.col = pos.col-4, yscale = yscale))

                                }


                                ## Y-axis to the right
                                if (column == cols.per.page) {

                                    axs <- x$y.scales

                                    ok <- (ylabelinfo$at>=yscale[1] & ylabelinfo$at<=yscale[2])

                                    push.viewport(viewport(layout.pos.row = pos.row,
                                                           layout.pos.col = pos.col+1,
                                                           yscale = yscale))


                                    if (axs$tck[2] !=0 && any(ok))
                                        grid.segments(x0 = unit(rep(0, sum(ok)), "npc"),
                                                      x1 = unit(rep(0.3 * axs$tck[2], sum(ok)), "lines"),
                                                      y0 = unit(ylabelinfo$at[ok], "native"),
                                                      y1 = unit(ylabelinfo$at[ok], "native"),
                                                      gp = gpar(col = yaxis.col))

                                    pop.viewport()

                                    if (y.alternating[actual.row]==2 || y.alternating[actual.row]==3)

                                        if (any(ok))

                                            grid.text(label = ylabelinfo$label[ok],
                                                      y = unit(ylabelinfo$at[ok], "native"),
                                                      x = unit(if (abs(yaxis.rot[2]) == 90) .5 else 0, "npc"),
                                                      ##y = unit(.05, "npc"),
                                                      just = if (yaxis.rot[2] == -90) c("centre", "centre")
                                                      else if (yaxis.rot[2] == 90) c("centre", "centre")
                                                      else if (yaxis.rot[2] > -90 && yaxis.rot[2] < 90)  c("left", "centre")
                                                      else c("right", "centre"),
                                                      rot = yaxis.rot[2],
                                                      check.overlap = ylabelinfo$check.overlap,
                                                      gp = gpar(col = yaxis.col, font = yaxis.font, 
                                                      fontsize = axs$cex[2] * fontsize.default),
                                                      vp = viewport(layout.pos.row = pos.row,
                                                      layout.pos.col = pos.col+2, yscale = yscale))

                                }
                            }
                                
                            ## X-axis to the bottom
                            if (x.relation.same && x$x.scales$draw) {

                                if (actual.row == 1) {

                                    axs <- x$x.scales

                                    ok <- (xlabelinfo$at>=xscale[1] & xlabelinfo$at<=xscale[2])

                                    push.viewport(viewport(layout.pos.row = pos.row+3,
                                                           layout.pos.col = pos.col,
                                                           xscale = xscale))

                                    if (axs$tck[1] !=0 && any(ok))
                                        grid.segments(y0 = unit(rep(1, sum(ok)), "npc"),
                                                      y1 = unit(rep(1, sum(ok)), "npc") -
                                                      unit(rep(0.3 * axs$tck[1], sum(ok)), "lines"),
                                                      x0 = unit(xlabelinfo$at[ok], "native"),
                                                      x1 = unit(xlabelinfo$at[ok], "native"),
                                                      gp = gpar(col = xaxis.col))

                                    pop.viewport()

                                    if (x.alternating[column]==1 || x.alternating[column]==3) 

                                        if (any(ok)) {

                                            grid.text(label = xlabelinfo$lab[ok],
                                                      x = unit(xlabelinfo$at[ok], "native"),
                                                      y = unit(if (xaxis.rot[1] %in% c(0, 180)) .5 else 1, "npc"),
                                                      ##y = unit(rep(.95, sum(ok)), "npc"),
                                                      just = if (xaxis.rot[1] == 0) c("centre", "centre")
                                                      else if (xaxis.rot[1] == 180) c("centre", "centre")
                                                      else if (xaxis.rot[1] > 0)  c("right", "centre")
                                                      else c("left", "centre"),
                                                      rot = xaxis.rot[1],
                                                      check.overlap = xlabelinfo$check.overlap,
                                                      gp = gpar(col = xaxis.col, font = xaxis.font, 
                                                      fontsize = axs$cex[1] * fontsize.default),
                                                      vp = viewport(layout.pos.row = pos.row + 4,
                                                      layout.pos.col = pos.col, xscale = xscale))
                                        }
                                }
                            }
                                    
                            ##-------------------------


                            if (!is.logical(x$strip)) # logical ==> FALSE
                                for(i in 1:number.of.cond)
                                {
                                    push.viewport(viewport(layout.pos.row = pos.row-i,
                                                           layout.pos.col = pos.col,
                                                           clip = TRUE,
                                                           gp = gpar(fontsize = fontsize.default)))
                                    
                                    grid.rect()
                                    x$strip(which.given = i,
                                            which.panel = cond.current.level,
                                            var.name = names(x$cond),
                                            factor.levels = if (!is.list(x$cond[[i]]))
                                            x$cond[[i]] else NULL,
                                            shingle.intervals = if (is.list(x$cond[[i]]))
                                            do.call("rbind", x$cond[[i]]) else NULL,
                                            ##x = x$condlevel[[i]],
                                            ##level = cond.current.level[i],
                                            ##name = names(x$cond)[i],
                                            bg = strip.col.default.bg[i],
                                            fg = strip.col.default.fg[i],
                                            par.strip.text = x$par.strip.text)
                                    
                                    pop.viewport()
                                            
                                }
                            
                            
                            ## X-axis at top
                            if (x.relation.same && x$x.scales$draw)

                                if (actual.row == rows.per.page) {

                                    axs <- x$x.scales

                                    ok <- (xlabelinfo$at>=xscale[1] & xlabelinfo$at<=xscale[2])

                                    push.viewport(viewport(layout.pos.row = pos.row - 1 - 
                                                           number.of.cond,
                                                           layout.pos.col = pos.col,
                                                           xscale = xscale))

                                    if (axs$tck[2] !=0 && any(ok))
                                        grid.segments(y0 = unit(rep(0, sum(ok)), "npc"),
                                                      y1 = unit(rep(0.3 * axs$tck[2], sum(ok)), "lines"),
                                                      x0 = unit(xlabelinfo$at[ok], "native"),
                                                      x1 = unit(xlabelinfo$at[ok], "native"),
                                                      gp = gpar(col = xaxis.col))

                                    pop.viewport()

                                    if (x.alternating[column]==2 || x.alternating[column]==3)

                                        if (any(ok))

                                            grid.text(label = xlabelinfo$label[ok],
                                                      x = unit(xlabelinfo$at[ok], "native"),
                                                      y = unit(if (xaxis.rot[2] %in% c(0, 180)) .5 else 0, "npc"),
                                                      ##y = unit(.05, "npc"),
                                                      just = if (xaxis.rot[2] == 0) c("centre", "centre")
                                                      else if (xaxis.rot[2] == 180) c("centre", "centre")
                                                      else if (xaxis.rot[2] > 0)  c("left", "centre")
                                                      else c("right", "centre"),
                                                      rot = xaxis.rot[2],
                                                      check.overlap = xlabelinfo$check.overlap,
                                                      gp = gpar(col = xaxis.col, font = xaxis.font, 
                                                      fontsize = axs$cex[2] * fontsize.default),
                                                      vp = viewport(layout.pos.row = pos.row - 2 - 
                                                      number.of.cond, layout.pos.col = pos.col, xscale = xscale))


                                }

                                
                            cond.current.level <- cupdate(cond.current.level,
                                                          cond.max.level)
                            panel.number <- panel.number + 1

                        }
                        
                    }
            
            
            if (!is.null(x$key) || !is.null(x$colorkey)) {
                
                if (key.space == "left") {
                    push.viewport(viewport(layout.pos.col = 2,
                                  layout.pos.row = c(6, n.row-6)))
                    grid.draw(key.gf)
                    pop.viewport()
                }
                else if (key.space == "right") {
                    push.viewport(viewport(layout.pos.col = n.col-1,
                                  layout.pos.row = c(6, n.row-6)))
                    grid.draw(key.gf)
                    pop.viewport()
                    }
                else if (key.space == "top") {
                    push.viewport(viewport(layout.pos.row = 3,
                                           layout.pos.col = c(6,n.col-4)))
                    grid.draw(key.gf)
                    pop.viewport()
                }
                else if (key.space == "bottom") {
                    push.viewport(viewport(layout.pos.row = n.row - 2,
                                           layout.pos.col = c(6,n.col-4)))
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
    invisible(page.layout)
}
