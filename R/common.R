

### Copyright 2001-2002  Deepayan Sarkar <deepayan@stat.wisc.edu>
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
        if (inherits(ans$left, "POSIXt")) ans$left <- as.POSIXct(ans$left)
        ans$left.name <- deparse(model[[2]])
    }
    model <- model[[length(model)]]
    if (length(model) == 3 && model[[1]] == as.name("|")) {
        model.vars <- parseCond(model[[3]])
        ans$condition <- vector("list", length(model.vars))
        names(ans$condition) <- sapply(model.vars, deparse)
        for (i in seq(along = model.vars)) {
            ans$condition[[i]] <- eval(model.vars[[i]], data)
            if (inherits(ans$condition[[i]], "POSIXt"))
                ans$condition[[i]] <- as.POSIXct(ans$condition[[i]])
        }
        model <- model[[2]]
    }
    if (dimension == 2) {
        ans$right <- eval(model, data)
        if (inherits(ans$right, "POSIXt")) ans$right <- as.POSIXct(ans$right)
        ans$right.name <- deparse(model)
    }
    else if (dimension == 3 && length(model) == 3 &&
             (model[[1]] == "*" || model[[1]] == "+")) {
        ans$right.x <- eval(model[[2]], data)
        if (inherits(ans$right.x, "POSIXt")) ans$right.x <- as.POSIXct(ans$right.x)
        ans$right.y <- eval(model[[3]], data)
        if (inherits(ans$right.y, "POSIXt")) ans$right.y <- as.POSIXct(ans$right.y)
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






## modified from axis.POSIXct. This aims to be a general function
## which given a general 'range' x and optional at, generates the
## locations of tick marks and corresponding labels. Ultimately will
## be a replacement for lpretty

calculateAxisComponents <-
    function (x, at = FALSE, labels = FALSE, logpaste = "",
              abbreviate = NULL, minlength = 4,
              format, ...) 
{

    ## x is guaranteed to be given, though it might not be always
    ## necessary. Four cases, corresponding to factors
    ## (is.character(x)), shingle (inherits(x, "shingleLevel")),
    ## POSIXt (inherits(x, "POSIXt") and usual numeric. The last case
    ## will be default, and will be changed later if necessary.

    ## The code for shingles will never really be used. Shingles can
    ## also be thought of as numeric, and that's more appropriate for
    ## functions like xyplot, and limits will be just range. In
    ## functions like bwplot, things will be adjusted elsewhere when
    ## one of the variables is a shingle.

    ## Note that at and labels will never be TRUE (it's set up that
    ## way), so it's enough to check if they are is.logical(), which
    ## means they are not explicitly specified.

    if (is.character(x)) { ## factor
        ans <- list(at = if (is.logical(at)) seq(along = x) else at,
                    labels = if (is.logical(labels)) x else labels,
                    check.overlap = FALSE)
        ans$num.limit <- c(0, length(ans$at) + 1)
    }
    else if (inherits(x, "shingleLevel")) { ## shingle
        ans <- list(at = if (is.logical(at)) seq(along = x) else at,
                    labels = if (is.logical(labels))
                    as.character(seq(along = x)) else labels,
                    check.overlap = FALSE)
        ans$num.limit <- c(0, length(ans$at) + 1)
    }
    else if (is.numeric(x) && inherits(x, "POSIXt")) { ## POSIX time
                    
        num.lim <- as.numeric(range(x))
        mat <- is.logical(at)
        mlab <- is.logical(labels)

        if (!mat)
            x <- as.POSIXct(at)
        else x <- as.POSIXct(x)
        range <- as.numeric(range(x))
        d <- range[2] - range[1]
        z <- c(range, x[is.finite(x)])
        if (d < 1.1 * 60) {
            sc <- 1
            if (missing(format)) 
                format <- "%S"
        }
        else if (d < 1.1 * 60 * 60) {
            sc <- 60
            if (missing(format)) 
                format <- "%M:%S"
        }
        else if (d < 1.1 * 60 * 60 * 24) {
            sc <- 60 * 24
            if (missing(format)) 
                format <- "%H:%M"
        }
        else if (d < 2 * 60 * 60 * 24) {
            sc <- 60 * 24
            if (missing(format)) 
                format <- "%a %H:%M"
        }
        else if (d < 7 * 60 * 60 * 24) {
            sc <- 60 * 60 * 24
            if (missing(format)) 
                format <- "%a"
        }
        else {
            sc <- 60 * 60 * 24
        }
        if (d < 60 * 60 * 24 * 50) {
            zz <- lpretty(z/sc, ...)
            z <- zz * sc
            class(z) <- c("POSIXt", "POSIXct")
            if (missing(format)) 
                format <- "%b %d"
        }
        else if (d < 1.1 * 60 * 60 * 24 * 365) {
            class(z) <- c("POSIXt", "POSIXct")
            zz <- as.POSIXlt(z)
            zz$mday <- 1
            zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
            zz$mon <- lpretty(zz$mon, ...)
            m <- length(zz$mon)
            m <- rep(zz$year[1], m)
            zz$year <- c(m, m + 1)
            z <- as.POSIXct(zz)
            if (missing(format)) 
                format <- "%b"
        }
        else {
            class(z) <- c("POSIXt", "POSIXct")
            zz <- as.POSIXlt(z)
            zz$mday <- 1
            zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
            zz$year <- lpretty(zz$year, ...)
            z <- as.POSIXct(zz)
            if (missing(format)) 
                format <- "%Y"
        }
        if (!mat) 
            z <- x[is.finite(x)]
        z <- z[z >= range[1] & z <= range[2]]
        labels <- format(z, format = format)
        ans <- list(at = as.numeric(z), labels = labels,
                    check.overlap = FALSE,
                    num.limit = num.lim)
    }
    else { ## plain numeric

        ## will check for overlap only when neither at nor labels is specified

        check.overlap <-
            if (is.logical(at) && is.logical(labels)) TRUE
            else FALSE
        
        if (is.logical(at)) { # at not explicitly specified
            eps <- 1e-10
            at <- pretty(x[is.finite(x)], ...)
            
            ## Need to do this because pretty sometimes returns things
            ## like 2.3e-17 instead of 0
            at <- ifelse(abs(at - round(at, 3)) < eps,
                         round(at, 3), 
                         at)
        }
        ans <- list(at = at,
                    labels = if (is.logical(labels)) paste(logpaste,
                    as.character(at), sep = "") else labels,
                    check.overlap = check.overlap,
                    num.limit = range(x))
    }
    if (is.logical(abbreviate) && abbreviate)
        ans$labels <- abbreviate(ans$labels, minlength)
    ans
}








extend.limits <-
    function(lim, length=1, prop = 0.07)
{
    if (!is.numeric(lim)) NA
    else if(length(lim)==2) {
        if (lim[1]>lim[2]) stop("Improper value of limit")
        if (!missing(length) && !missing(prop))
            stop("length and prop cannot both be specified")
        if (length <= 0) stop("length must be positive")
        if (!missing(length))
            prop <- (as.numeric(length) - as.numeric(diff(lim))) / (2 * as.numeric(diff(lim)))
        if (lim[1]==lim[2]) lim + 0.5*c(-length,length)
        else {
            d <- as.numeric(diff(lim))
            lim + prop * d * c(-1,1)
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
             abbreviate = FALSE,
             minlength = 4,
             x = NULL,
             y = NULL,
             ...)   ## FIXME: how to handle ...
{
    xfoo <- list(draw = draw, tck = tck,
                 tick.number = tick.number,
                 cex = cex, rot = rot, font = font,
                 at = at, labels = labels,
                 col = col, log = log,
                 alternating = alternating,
                 relation = relation,
                 abbreviate = abbreviate,
                 minlength = minlength)
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
    distance <- rep(distance, length = 3)
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
    x.limits <- vector("list", nplots)
    y.limits <- vector("list", nplots)
    dxdy <- vector("list", nplots)

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


    ## Some explanation might be helpful here. The for loop above
    ## creates a list of xlims/ylims. Each of these might be either
    ## numeric (when x/y is numeric, shigle or POSIXt), or levels of a
    ## factor (that's how prepanel.default.functions are set
    ## up). However, at this point, all x.limits[[i]] must be of the
    ## same type. Returned limits must be in accordance with this
    ## type.



    if (x.relation == "same")
        if (have.xlim) {
            x.limits <- xlim
            x.slicelen <- if (is.numeric(xlim)) diff(range(xlim)) else length(xlim) + 2
        }
        else if (is.numeric(x.limits[[1]])) {
            x.limits <- extend.limits(range(do.call("c", x.limits), na.rm = TRUE))
            x.slicelen <- if (is.numeric(x.limits)) diff(range(x.limits))
        }
        else {
            x.limits <- x.limits[[1]]
            x.slicelen <- length(x.limits) + 2
        }
    else if (x.relation == "sliced") {
        if (is.numeric(x.limits[[1]])) {
            x.slicelen <- 1.14 * max(unlist(lapply(x.limits, diff)), na.rm = TRUE)
            x.limits <- lapply(x.limits, extend.limits, length = x.slicelen)
        }
        x.slicelen <- length(x.limits[[1]]) + 2
    }
    else if (x.relation == "free")
        if (is.numeric(x.limits[[1]]))
            x.limits <- lapply(x.limits, extend.limits)





    if (y.relation == "same")
        if (have.ylim) {
            y.limits <- ylim
            y.slicelen <- if (is.numeric(ylim)) diff(range(ylim)) else length(ylim) + 2
        }
        else if (is.numeric(y.limits[[1]])) {
            y.limits <- extend.limits(range(do.call("c", y.limits), na.rm = TRUE))
            y.slicelen <- if (is.numeric(y.limits)) diff(range(y.limits))
        }
        else {
            y.limits <- y.limits[[1]]
            y.slicelen <- length(y.limits) + 2
        }
    else if (y.relation == "sliced") {
        if (is.numeric(y.limits[[1]])) {
            y.slicelen <- 1.14 * max(unlist(lapply(y.limits, diff)), na.rm = TRUE)
            y.limits <- lapply(y.limits, extend.limits, length = y.slicelen)
        }
        y.slicelen <- length(y.limits[[1]]) + 2
    }
    else if (y.relation == "free")
        if (is.numeric(y.limits[[1]]))
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
        if (is.null(text)) text <- list(cex = 1.2, col = "black", font = 2) # shouldn't happen
        foo$main <- list(label = if (is.list(main)) main[[1]] else main,
                         col = text$col, cex = text$cex, font = text$font)
        if (is.list(main)) foo$main[names(main)] <- main
    }
    if (!is.null(sub)) {
        text <- trellis.par.get("par.sub.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 2)
        foo$sub <- list(label = if (is.list(sub)) sub[[1]] else sub,
                        col = text$col, cex = text$cex, font = text$font)
        if (is.list(sub)) foo$sub[names(sub)] <- sub
    }
    if (!is.null(xlab)) {
        text <- trellis.par.get("par.xlab.text")
        if (is.null(text)) text <- list(cex = 1, col = "black", font = 1)
        foo$xlab <- list(label = if (is.list(xlab)) xlab[[1]] else xlab,
                         col = text$col, cex = text$cex, font = text$font)
        if (is.list(xlab)) foo$xlab[names(xlab)] <- xlab
    }
    if (!is.null(ylab)) {
        text <- trellis.par.get("par.ylab.text")
        if (is.null(text)) text <- list(cex = 1.2, col = "black", font = 2)
        foo$ylab <- list(label = if (is.list(ylab)) ylab[[1]] else ylab,
                         col = text$col, cex = text$cex, font = text$font)
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



