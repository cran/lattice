

### Copyright 2001-2002 Deepayan Sarkar <deepayan@stat.wisc.edu>
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


lattice.theme <- list()

col.whitebg <- function()
    list(background = list(col="transparent"),
         bar.fill = list(col="#c8ffc8"),
         box.rectangle = list(col="darkgreen"),
         box.umbrella = list(col="darkgreen"),
         dot.line = list(col="#e8e8e8"),
         dot.symbol = list(col="darkgreen"),
         plot.line = list(col="darkgreen"),
         plot.symbol = list(col="darkgreen"),
         ##regions=list(col=rev(hsv(h=250:349/1000, v=30:129/150,s=.5,
         ##gamma = .6)))
         regions = list(col = heat.colors(100)),
         strip.shingle = list(col = c("#ff7f00", "#00ff00", "#00ffff",
                                 "#0080ff", "#ff00ff", "#ff0000", "#ffff00")),
         strip.background = list(col = c("#ffe5cc", "#ccffcc", "#ccffff",
                                 "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")),
         reference.line = list(col="#e8e8e8"),
         superpose.line = list(col = c("darkgreen","red","royalblue",
                               "brown","orange","turquoise", "orchid"),
         lty = 1:7),
         superpose.symbol = list(pch = c(1,3,6,0,5,16,17), cex = rep(.7, 7),
         col = c("darkgreen","red","royalblue",
                                 "brown","orange","turquoise", "orchid")))


canonical.theme <- function(name = "null device", color = TRUE)
{
    ## For the purpose of this function, the only differences in the
    ## settings/themes arise from the difference in the default
    ## colors. So, I will first set up the appropriate colors
    ## according to 'name', and then use those to create the
    ## theme. The first 16 colors correspond to trellis.settings
    ## colors, the 17th is the background color.

    if (color)
    {
        ## color colors
        can.col <-
            if (name == "windows" || name == "X11")
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#007eff", "#ffff00", "#ff0000",
                  "#c6ffff", "#ffc3ff", "#c8ffc8", "#ffd18f",
                  "#a9e2ff", "#ffffc3", "#ff8c8a", "#aaaaaa",
                  "#909090")
            else if (name %in% c("postscript", "pdf", "xfig"))
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#0080ff", "#ffff00", "#ff0000",
                  "#ccffff", "#ffccff", "#ccffcc", "#ffe5cc",
                  "#cce6ff", "#ffffcc", "#ffcccc", "#e6e6e6",
                  "transparent")
            else ## default, same as X11 for now
                c("#000000", "#00FFFF", "#FF00FF", "#00FF00",
                  "#FF7F00", "#007EFF", "#FFFF00", "#FF0000",
                  "#C6FFFF", "#FFC3FF", "#C8FFC8", "#FFD18F",
                  "#A9E2FF", "#FFFFC3", "#FF8C8A", "#AAAAAA",
                  "#909090")
    }
    else ## b&w colors, same for all devices (8:16 actually unnecessary)
        can.col <-
            c("#000000", "#999999", "#4C4C4C", "#E6E6E6", "#F2F2F2",
              "#B2B2B2", "#000000", "#030303", "#050505", "#080808",
              "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", "#151515",
              "#171717", "transparent")

    ## color settings, modified later if postscript or color = FALSE
    ans <-
        list(fontsize = list(default = 10),
             background = list(col = can.col[17]), 
             add.line = list(col = can.col[1], lty = 1, lwd = 1),
             add.text = list(cex = 1, col = can.col[1], font = 1),
             bar.fill = list(col = can.col[2]),
             box.dot = list(col = can.col[1], cex = 1, font = 1, pch =
             16),
             box.rectangle = list(col = can.col[2],
             fill = "transparent", lty = 1, lwd = 1),
             box.umbrella = list(col = can.col[2], lty = 2, lwd = 1),
             dot.line = list(col = can.col[16], lty = 1, lwd = 1),
             dot.symbol = list(cex = 0.8, col = can.col[2], font = 1,
             pch = 16),
             plot.line = list(col = can.col[2], lty = 1, lwd = 1),
             plot.symbol = list(cex = 0.8, col = can.col[2], font = 1,
             pch = 1),
             reference.line = list(col = can.col[16], lty = 1, lwd =
             1),
             strip.background = list(col = can.col[c(12, 11, 9, 13,
                                     10, 15, 14)]),
             strip.shingle = list(col = can.col[c(5, 4, 2, 6, 3, 8,
             7)]), superpose.line = list(col = can.col[2:8], lty =
             c(1, 1, 1, 1, 1, 1, 1), lwd = c(1, 1, 1, 1, 1, 1, 1)),
             regions = list(col = rev(cm.colors(100))),
             superpose.symbol = list(cex = c(0.8, 0.8, 0.8, 0.8, 0.8,
             0.8, 0.8), col = can.col[2:8], font = c(1, 1, 1, 1, 1, 1,
             1), pch = c("o", "o", "o", "o", "o", "o", "o")),
             axis.line = list(col = can.col[1], lty = 1, lwd = 1),
             axis.text = list(cex = .8, col = can.col[1], font = 1),
             box.3d = list(col = can.col[1], lty = 1, lwd = 1),
             par.xlab.text = list(cex = 1, col = can.col[1], font =
             1),
             par.ylab.text = list(cex = 1, col = can.col[1], font =
             1),
             par.main.text = list(cex = 1.2, col = can.col[1], font =
             2),
             par.sub.text = list(cex = 1, col = can.col[1], font = 2))

    if (color) {
        if (name == "postscript" || name == "pdf") {
            ans$plot.symbol$col <- can.col[6]
            ans$plot.line$col <- can.col[6]
            ans$dot.symbol$col <- can.col[6]
            ans$box.rectangle$col <- can.col[6]
            ans$box.umbrella$col <- can.col[6]
            ans$superpose.symbol$col <- c(can.col[c(6, 3, 4, 8)],
                                          "orange", "darkgreen", "brown")
            ans$superpose.line$col <- c(can.col[c(6, 3, 4, 8)],
                                          "orange", "darkgreen", "brown")
        }
    }
    else {
        ## black and white settings
        ans$bar.fill$col <- can.col[5]
        ans$box.dot$col <- can.col[1]
        ans$box.rectangle$col <- can.col[1]
        ans$box.umbrella$col <- can.col[1]
        ans$box.umbrella$lty <- 2
        ans$dot.line$col <- can.col[4]
        ans$dot.symbol$col <- can.col[1]
        ans$dot.symbol$cex <- 0.85
        ans$plot.line$col <- can.col[1]
        ans$plot.symbol$col <- can.col[1]
        ans$regions$col <- gray(29:128/128)
        ans$reference.line$col <- can.col[4]
        ans$strip.background$col <- can.col[rep(5, 7)]
        ans$strip.shingle$col <- can.col[rep(6, 7)]
        ans$superpose.line$col <- can.col[rep(1, 7)]
        ans$superpose.line$lty <- 1:7
        ans$superpose.symbol$col <- can.col[rep(1, 7)]
        ans$superpose.symbol$cex <- rep(0.7, 7)
        ans$superpose.symbol$pch <- c(1,3,6,0,5,16,17)
        ##ans$superpose.symbol$pch <- c("o","+",">","s","w","#","{")
    }
    ans
}   





trellis.par.get <-
    function(name = NULL)
{
    ## the default device is opened if none already open
    if (is.null(dev.list())) trellis.device()

    if (!(.Device %in% names(lattice.theme))) {
        ## shouldn't happen
        lattice.theme[[.Device]] <<- list()
        device <- .Device
        color <- !(device == "postscript")
        lset(canonical.theme(device, color))
    }
    if (is.null(name))
        lattice.theme[[.Device]]
    else if (name %in% names(lattice.theme[[.Device]]))
        lattice.theme[[.Device]][[name]]
    else NULL
}

trellis.par.set <-
    function(name, value, warn = TRUE)
{
    ## the default device is opened if none already open
    if (is.null(dev.list())) {
        trellis.device()
        if (warn) cat("Note: The default device has been opened to honour attempt to modify trellis settings\n\n")
    }

    ## if (name %in% names(lattice.theme[[.Device]])) NEEDED as a safeguard ?
    if (!is.list(value)) stop("value must be a list")
    lattice.theme[[.Device]][[name]] <<- value
}



trellis.device <-
    function(device = getOption("device"),
             color = !(dev.name == "postscript"),
             theme = getOption("lattice.theme"),
             bg = NULL,
             new = TRUE,
             retain = FALSE,
             ...)
{
    ## Get device function
    if (is.character(device))
    {
        device.call <- get(device)
        dev.name <- device
    }
    else {
        device.call <- device
        dev.name <- deparse(substitute(device))
    }
    ## Start the new device if necessary.
    ## new = FALSE ignored if no devices open.
    if (new || is.null(dev.list()))
    {
        device.call(...)
        .lattice.print.more <<- FALSE
    }
    if (!is.null(theme) && !is.list(theme)) {
        if (is.character(theme)) theme <- get(theme)
        if (is.function(theme)) theme <- theme()
        if (!is.list(theme)) {
            warning("Invalid theme specified")
            theme <- NULL
        }
    }
    ## If retain = TRUE, retain existing settings for this
    ## particular device, if any. Ignores new.
    if (retain && .Device %in% names(lattice.theme))
    {
        if (!is.null(theme)) lset(theme) 
        return(invisible())
    }
    lset(canonical.theme(name=.Device, color=color))
    if (!is.null(theme)) lset(theme)
    if (!is.null(bg)) trellis.par.set("background", list(col = bg))
}




lset <- function(theme = col.whitebg(), warn = TRUE)
{
    for (item in names(theme)) {
        foo <- trellis.par.get(item)
        bar <- theme[[item]]
        foo[names(bar)] <- bar
        trellis.par.set(item, foo, warn = warn)
    }
}







show.settings <- function(x = NULL)
{
    if (is.null(dev.list())) trellis.device()
    theme <- trellis.par.get()
    if (is.null(theme) && is.null(x)) print("No active device") ## shouldn't happen
    else {
        if (is.null(theme)) { ## also shouldn't happen
            print("No device is currently active but a theme has been explicitly specified.")
            print("Default device options will be used to fill missing components")
            print("of specified theme, if any.")
            theme <- canonical.theme(getOption("device"))
        }
        if (!is.null(x)) {
            for (item in names(x)) {
                foo <- x[[item]]
                theme[[item]][names(foo)] <- foo
            }
        }
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
        grid.rect(gp = gpar(fill = theme$background$col,
                  col = "transparent"))
        push.viewport(viewport(layout = page.layout, gp = gpar(fontsize = theme$fontsize$default)))
        superpose.symbol <- theme$superpose.symbol
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
                  ##gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
        superpose.line <- theme$superpose.line
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
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 4))
        strip.background <- theme$strip.background
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
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 6))
        strip.shingle <- theme$strip.shingle
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
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 3, layout.pos.col = 8))
        push.viewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 2,
                               yscale = extend.limits(c(0,6)),
                               xscale = c(0,6)))
        x <- c(1,2,3,4,5)
        dot.line <- theme$dot.line
        dot.symbol <- theme$dot.symbol
        panel.abline(h=1:5, col=dot.line$col,
                     lty=dot.line$lty, lwd=dot.line$lwd)
        panel.xyplot(x = x, y = x, col = dot.symbol$col, pch = dot.symbol$pch)
        grid.rect()
        pop.viewport()
        grid.text(lab = "dot.[symbol, line]",
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 2))
        box.rectangle <- theme$box.rectangle
        box.dot <- theme$box.dot
        box.umbrella <- theme$box.umbrella
        push.viewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 4,
                               yscale = c(-1.5,1.5),
                               xscale = c(0,6)))
        push.viewport(viewport(height = unit(.1, "npc")))
        grid.rect(width = 1/3, 
                  gp = gpar(lwd = box.rectangle$lwd, 
                  lty = box.rectangle$lty,
                  fill = box.rectangle$fill,
                  col = box.rectangle$col))
        grid.lines(x = unit(c(1/6, 1/3), "npc"), 
                   y = unit(c(0.5, 0.5), "npc"),
                   gp = gpar(col = box.umbrella$col,
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(c(2/3, 5/6), "npc"), 
                   y = unit(c(0.5, 0.5), "npc"),
                   gp = gpar(col = box.umbrella$col,
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(rep(1/6, 2), "npc"), 
                   y = unit(c(0, 1), "npc"),
                   gp = gpar(col = box.umbrella$col, 
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        grid.lines(x = unit(rep(5/6, 2), "npc"), 
                   y = unit(c(0, 1), "npc"),
                   gp = gpar(col = box.umbrella$col, 
                   lwd = box.umbrella$lwd, lty = box.umbrella$lty))
        if (is.character(box.dot$pch)) 
            grid.text(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                      lab = box.dot$pch,
                      gp = gpar(col = box.dot$col, cex = box.dot$cex))
        else
            grid.points(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        pch = box.dot$pch, size = unit(box.dot$cex * 2.5, "mm"),
                        gp = gpar(col = box.dot$col))
        pop.viewport()
        grid.rect()
        pop.viewport()
        grid.text(lab = "box.[dot, rectangle, umbrella]",
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 4))
        add.text <- theme$add.text
        add.line <- theme$add.line
        push.viewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 6,
                               yscale = c(-1,1),
                               xscale = c(0,1)))
        x <- seq(.1, .9, length = 50)
        y <- .9 * sin(.1+11*x)
        llines(x = x, y = y, type = "l", col = add.line$col,
               lty = add.line$lty, lwd = add.line$lwd)
        grid.text(lab = c("Hello", "World"),
                  x = c(.25, .75), y = c(-.5, .5), default.units = "native",
                  gp = gpar(col = add.text$col,
                  fontsize = add.text$cex * trellis.par.get("fontsize")$default,
                  font = add.text$font))
        grid.rect()
        pop.viewport()
        grid.text(lab = "add.[line, text]",
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 6))
        reference.line <- theme$reference.line
        push.viewport(viewport(layout.pos.row = 5,
                               layout.pos.col = 8,
                               yscale = c(0,4),
                               xscale = c(0,4)))
        panel.grid(col = reference.line$col,
                   lwd = reference.line$lwd,
                   lty = reference.line$lty)
        grid.rect()
        pop.viewport()
        grid.text(lab = "reference.line",
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 6, layout.pos.col = 8))
        plot.symbol <- theme$plot.symbol
        plot.line <- theme$plot.line
        push.viewport(viewport(layout.pos.row = 8,
                               layout.pos.col = 2,
                               yscale = c(-1.1,1.1),
                               xscale = c(-.1,1.1)))
        x <- seq(.1, .9, length = 20)
        y <- .9 * sin(.1+11*x)
        panel.xyplot(x = x+.05, y = y+.1, type = "l",
                     lty = plot.line$lty,
                     col = plot.line$col,
                     lwd = plot.line$lwd)
        panel.xyplot(x = x-.05, y = y-.1,
                     col = plot.symbol$col,
                     font = plot.symbol$font,
                     pch = plot.symbol$pch,
                     cex = plot.symbol$cex)
        grid.rect()
        pop.viewport()
        grid.text(lab = "plot.[symbol, line]",
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 2))
        bar.fill <- theme$bar.fill
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
                  ## gp = gpar(fontsize = 8),
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
                  ## gp = gpar(fontsize = 8),
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
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 9, layout.pos.col = 8))
        regions <- theme$regions
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
                  ## gp = gpar(fontsize = 8),
                  vp = viewport(layout.pos.row = 12, layout.pos.col = 2))
    }    
    invisible()
}










    
