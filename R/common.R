

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




latticeParseFormula <-
    function(model, data, dimension = 2)
{
    parseCond <-
        function(model)
        {
            model <- eval(parse(text = paste("~", deparse(model))))[[2]]
            model.vars <- list()
            while (length(model) == 3 && (model[[1]] == as.name("*")
                         || model[[1]] == as.name("+"))) {
                model.vars <- c(model.vars, model[[3]])
                model <- model[[2]]
            }
            rev(c(model.vars, model))
        }
    if (!inherits(model, "formula"))
        stop("model must be a formula object")

    ans <- if (dimension == 2) {
        list(left = NULL, right = NULL, condition = NULL,
             left.name = character(0), right.name = character(0))
    }
    else if (dimension == 3) {
        list(left = NULL, right.x = NULL, right.y = NULL, condition = NULL,
             left.name = character(0), right.x.name = character(0),
             right.y.name = character(0))
    }
    else stop(paste("invalid dimension : ", dimension))

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
    if (dimension == 2) {
        ans$right <- eval(model, data)
        ans$right.name <- deparse(model)
    }
    else if (dimension == 3 && length(model) == 3 &&
             (model[[1]] == "*" || model[[1]] == "+")) {
        ans$right.x <- eval(model[[2]], data)
        ans$right.y <- eval(model[[3]], data)
        ans$right.x.name <- deparse(model[[2]])
        ans$right.y.name <- deparse(model[[3]])
    }
    else stop("invalid model")

    ans
}





banking <- function(dx, dy)
{
    if (is.na(dx)) NA
    else {
        if (is.list(dx)) {
            dy <- dx[[2]]
            dx <- dx[[1]]
        }
        if (length(dx)!=length(dy)) stop("Non matching lengths")
        id <- dx!=0 & dy!=0
        if (any(id)) {
            r  <- abs(dx[id]/dy[id])
            median(r)
        }
        else 1
    }
}





extend.limits <-
    function(lim, length=1, prop = 0.07) {
        if (!is.numeric(lim)) NA
        else if(length(lim)==2) {
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
            print(lim)
            stop("improper length of lim in extend.limits")
        }
    }






construct.scales <-
    function(draw = TRUE,
             tck = 1,
             tick.number = 5,
             cex = 1,
             rot = FALSE,
             at = FALSE,
             labels = FALSE,
             col = FALSE,
             log = FALSE,
             font = FALSE,
             alternating = TRUE,
             relation = "same",
             x = NULL,
             y = NULL,
             ...)
{
    xfoo <- list(draw = draw, tck = tck,
                 tick.number = tick.number,
                 cex = cex, rot = rot, font = font,
                 at = at, labels = labels,
                 col = col, log = log,
                 alternating = alternating,
                 relation = relation)
    yfoo <- xfoo
    if (!is.null(x)) {
        if (is.character(x)) x <- list(relation = x)
        xfoo[names(x)] <- x
    }
    if (is.logical(xfoo$alternating))
        xfoo$alternating <-
            if (xfoo$alternating) c(1,2)
            else 1
    if (!is.null(y)) {
        if (is.character(y)) y <- list(relation = y)
        yfoo[names(y)] <- y
    }
    if (is.logical(yfoo$alternating))
        yfoo$alternating <-
            if (yfoo$alternating) c(1,2)
            else 1
    list(x.scales = xfoo, y.scales = yfoo)
}





construct.3d.scales <-
    function(draw = TRUE,
             tck = 1,
             lty = 1, lwd = 1,
             distance = c(1,1,1),
             tick.number = 5,
             cex = 1,
             rot = FALSE,
             at = FALSE,
             labels = FALSE,
             col = FALSE,
             log = FALSE,
             font = FALSE,
             arrows = TRUE,
             relation = "same",
             x = NULL,
             y = NULL,
             z = NULL,
             ...)
{
    xfoo <- list(draw = draw, tck = tck,
                 lty = 1, lwd = 1,
                 tick.number = tick.number,
                 cex = cex, rot = rot, font = font,
                 at = at, labels = labels,
                 col = col, log = log, arrows = arrows,
                 relation = relation)
    yfoo <- xfoo
    zfoo <- xfoo
    xfoo$distance <- distance[1]
    yfoo$distance <- distance[2]
    zfoo$distance <- distance[3]
    if (!is.null(x)) {
        if (is.character(x)) x <- list(relation = x)
        xfoo[names(x)] <- x
    }
    if (!is.null(y)) {
        if (is.character(y)) y <- list(relation = y)
        yfoo[names(y)] <- y
    }
    if (!is.null(z)) {
        if (is.character(z)) z <- list(relation = z)
        zfoo[names(z)] <- z
    }
    list(x.scales = xfoo, y.scales = yfoo, z.scales = zfoo)
}




limits.and.aspect <-
    function(prepanel.default.function,
             prepanel = NULL,
             have.xlim = FALSE, xlim = NULL,
             have.ylim = FALSE, ylim = NULL,
             x.relation, y.relation,
             panel.args.common = list(),
             panel.args = list(),
             aspect,
             nplots,
             ...)  ## extra arguments for prepanel (for qqmathline)
{

    if (nplots<1) stop("need at least one panel")
    x.limits <- as.list(1:nplots)
    y.limits <- as.list(1:nplots)
    dxdy <- as.list(1:nplots)


    for (count in 1:nplots)
    {
        if (is.list(panel.args[[count]])) {

            pargs <- c(panel.args.common, panel.args[[count]], list(...))
            tem <- do.call("prepanel.default.function", pargs)
            if (is.function(prepanel)) {
                prenames <- names(formals(prepanel))
                if (!("..." %in% prenames)) pargs <- pargs[prenames]
                pretem <- do.call("prepanel", pargs)
                tem[names(pretem)] <- pretem
            }
            x.limits[[count]] <- tem$xlim
            y.limits[[count]] <- tem$ylim
            dxdy[[count]] <- list(tem$dx, tem$dy)

        }
        else {
            x.limits[[count]] <- c(NA, NA)
            y.limits[[count]] <- c(NA, NA)
            dxdy[[count]] <- NA
        } 

    }

    if (x.relation == "same")
        x.limits <-
            if (have.xlim) xlim
            else extend.limits(range(unlist(x.limits), na.rm = TRUE))
    else if (x.relation == "sliced") {
        x.slicelen <- 1.14 * max(unlist(lapply(x.limits, diff)), na.rm = TRUE)
        x.limits <- lapply(x.limits, extend.limits, length = x.slicelen)
    }
    else if (x.relation == "free")
        x.limits <- lapply(x.limits, extend.limits)

    if (y.relation == "same") {
        y.limits <-
            if (have.ylim) ylim
            else extend.limits(range(unlist(y.limits), na.rm = TRUE))
    }
    else if (y.relation == "sliced") {
        y.slicelen <- 1.14 * max(unlist(lapply(y.limits, diff)), na.rm = TRUE)
        y.limits <- lapply(y.limits, extend.limits, length = y.slicelen)
    }
    else if (y.relation == "free")
        y.limits <- lapply(y.limits, extend.limits)

    if (is.character(aspect))
        if (aspect == "xy") {
            aspect <- median(unlist(lapply(dxdy, banking)), na.rm = TRUE)
            if (y.relation == "free")
                warning("aspect=xy when y-relation=free is not sensible")
            else aspect <- aspect *
                if (y.relation == "sliced") y.slicelen else diff(y.limits)
            if (x.relation == "free")
                warning("aspect=xy when x-relation=free is not sensible")
            else aspect <- aspect /
                if (x.relation == "sliced") x.slicelen else diff(x.limits)
        }
    else aspect <- 1

    list(x.limits = x.limits,
         y.limits = y.limits,
         aspect.ratio = aspect)
}






trellis.skeleton <-
    function(as.table = FALSE,
             aspect = "fill",
             between = list(x=0, y=0),
             key = NULL,
             page = NULL,
             main = NULL,
             sub = NULL,
             par.strip.text = list(),
             skip = FALSE,
             strip = strip.default,
             xlab = NULL,
             ylab = NULL,
             panel = panel,
             ...)
{
    foo <- list(as.table = as.table,
                aspect.fill = aspect=="fill",
                key = key,
                panel = panel, 
                page = page,
                skip = skip,
                strip = if (is.logical(strip) && strip) "strip.default"
                else strip,
                x.between = 0,
                y.between = 0,
                par.strip.text = trellis.par.get("add.text"))
    if (is.null(foo$par.strip.text)) foo$par.strip.text = list(col = "black", cex = 1, font = 1)
    foo$par.strip.text$lines <- 1
    
    if (!is.null(between$x)) foo$x.between <- between$x
    if (!is.null(between$y)) foo$y.between <- between$y

    foo$par.strip.text[names(par.strip.text)] <- par.strip.text

    if (!is.null(main)) {
        text <- trellis.par.get("par.main.text")
        if (is.null(text)) text <- list(cex = 1.2, col = "black", font = 2)
        foo$main <- list(label = main[[1]], col = text$col, cex = text$cex, font = text$font)
        if (is.list(main)) foo$main[names(main)] <- main
    }
    if (!is.null(sub)) {
        text <- trellis.par.get("par.sub.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 2)
        foo$sub <- list(label = sub[[1]], col = text$col, cex = text$cex, font = text$font)
        if (is.list(sub)) foo$sub[names(sub)] <- sub
    }
    if (!is.null(xlab)) {
        text <- trellis.par.get("par.xlab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        foo$xlab <- list(label = xlab[[1]], col = text$col, cex = text$cex, font = text$font)
        if (is.list(xlab)) foo$xlab[names(xlab)] <- xlab
    }
    if (!is.null(ylab)) {
        text <- trellis.par.get("par.ylab.text")
        if (is.null(text)) text <- list(cex = 1.2, col = "black", font = 2)
        foo$ylab <- list(label = ylab[[1]], col = text$col, cex = text$cex, font = text$font)
        if (is.list(ylab)) foo$ylab[names(ylab)] <- ylab
    }
list(foo = foo, dots = list(...))
}







compute.layout <-
    function(layout, cond.max.level, skip = FALSE)
{
    number.of.cond <- length(cond.max.level)
    nplots <- prod(cond.max.level)
    
    if (!is.numeric(layout)) {
        layout <- c(0,1,1)
        if (number.of.cond==1) layout[2] <- nplots
        else {
            layout[1] <- cond.max.level[1]
            layout[2] <- cond.max.level[2]
        }
        skip <- rep(skip, length = max(layout[1] * layout[2], layout[2]))
        plots.per.page <- length(skip) - length(skip[skip])
        layout[3] <- ceiling(nplots/plots.per.page) # + 1
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
        layout[3] <- ceiling(nplots/plots.per.page) # + 1 
    }
    else if (length(layout)==3) {
        if(layout[1]<0||layout[2]<1||layout[3]<1)
            stop("invalid value for layout")
    }
    layout
}



