


# "[.trellis" <-
#     function(x, i, layout = NULL, ...)
# {
#     x$panel.args <- x$panel.args[i]
#     if (is.list(x$x.scales$at))
#         x$x.scales$at <- x$x.scales$at[i]
#     if (is.list(x$x.scales$labels))
#         x$x.scales$labels <- x$x.scales$labels[i]
#     if (missing(layout)) layout <- c(0, length(x$panel.args), 1)
#     x$layout <- layout
#     x
# }

 




update.trellis <-
    function(object,
             aspect,
             as.table,
             between,
             key,
             layout,
             main,
             page,
             panel,
             par.strip.text,
             scales,
             skip,
             strip,
             sub,
             xlab,
             xlim,
             ylab,
             ylim,
             ...)
{
    
    if(!missing(aspect)){
        if (is.numeric(aspect)){
            object$aspect.ratio <- aspect
            object$aspect.fill <- FALSE
        }
        else if (is.character(aspect) && aspect=="fill")
            object$aspect.fill <- TRUE
        else warning("Inappropriate value of aspect")
    }
    
    if(!missing(as.table)){
        if (is.logical(as.table)) object$as.table <- as.table
        else warning("Inappropriate value of as.table")
    }
    
    if(!missing(between)){
        if ("x" %in% names(between)) object$x.between <- between$x
        if ("y" %in% names(between)) object$y.between <- between$y
    }
    
    if(!missing(key)){
        object$key <- key
    }
    
    if(!missing(layout)){
        if (length(layout)==2){
            object$layout[3] <- ceiling(object$layout[1]*object$layout[2]*object$layout[3]/
                                     max(layout[1]*layout[2], layout[2]))
            object$layout[1] <- layout[1]
            object$layout[2] <- layout[2]
        }
        else if (length(layout)==3){
            object$layout <- layout
        }
        else warning("Inappropriate value of layout")
    }



    
    if(!missing(main)){
        if (is.characterOrExpression(main))
            if (is.null(object$main)) object$main <-
                list(label = main, col = "black", cex = 1, font = 1)
            else object$main$label <- main

        else if (is.list(main)) {
            object$main[names(main)] <- main
        }
        else if (is.null(main)) object$main <- NULL
    }
    
    if(!missing(sub)){
        if (is.characterOrExpression(sub))
            if (is.null(object$sub)) object$sub <-
                list(label = sub, col = "black", cex = 1, font = 1)
            else object$sub$label <- sub

        else if (is.list(sub)) {
            object$sub[names(sub)] <- sub
        }
        else if (is.null(sub)) object$sub <- NULL
    }
    
    if(!missing(xlab)){
        if (is.characterOrExpression(xlab))
            if (is.null(object$xlab)) object$xlab <-
                list(label = xlab, col = "black", cex = 1, font = 1)
            else object$xlab$label <- xlab

        else if (is.list(xlab)) {
            object$xlab[names(xlab)] <- xlab
        }
        else if (is.null(xlab)) object$xlab <- NULL
    }
    
    if(!missing(ylab)){
        if (is.characterOrExpression(ylab))
            if (is.null(object$ylab)) object$ylab <-
                list(label = ylab, col = "black", cex = 1, font = 1)
            else object$ylab$label <- ylab

        else if (is.list(ylab)) {
            object$ylab[names(ylab)] <- ylab
        }
        else if (is.null(ylab)) object$ylab <- NULL
    }
    

    
    if(!missing(page)){
        object$page <- page
    }
    
    if(!missing(panel)){
        panel <- 
            if (is.function(panel)) panel 
            else if (is.character(panel)) get(panel)
            else eval(panel)

        if (object$fname == "splom")
            object$panel.args.common$panel <- panel
        else object$panel <- panel
    }
    
    if(!missing(par.strip.text)){
        if (is.list(par.strip.text))
            object$par.strip.text[names(par.strip.text)] <-
                par.strip.text
        else warning("par.strip.text must be a list")
    }
    
    if(!missing(skip)){
        warning("sorry, but skip cannot be changed by update")
    }
    
    if(!missing(strip)){
        if (is.logical(strip)){
            if (strip) object$strip <- strip.default
        else object$strip <- FALSE
        }
        else object$strip <- strip
    }
    
    if(!missing(xlim)){
        if (!is.list(object$x.limits)) object$x.limits <- xlim
        else warning("xlim cannot be specified unless relation = same")
    }
    
    if(!missing(ylim)){
        if (!is.list(object$y.limits)) object$y.limits <- ylim
        else warning("ylim cannot be specified unless relation = same")
    }

    if(!missing(scales)){
        
        if ("relation" %in% names(scales))
            warning("relation cannot be changed via update")
        
      

      
        if ("alternating" %in% names(scales)){
            if (is.logical(scales$alternating))
                if (scales$alternating){
                    object$x.scales$alternating <- c(1,2)
                    object$y.scales$alternating <- c(1,2)
                }
                else {
                    object$x.scales$alternating <- 1
                    object$y.scales$alternating <- 1
                }
            else if (is.numeric(scales$alternating)) {
                object$x.scales$alternating <- scales$alternating
                object$y.scales$alternating <- scales$alternating
            }
        }
        
        
        
        
        if ("x" %in% names(scales) &&
            "alternating" %in% names(scales$x)){
            if (is.logical(scales$x$alternating))
                if (scales$x$alternating){
                    object$x.scales$alternating <- c(1,2)
                }
                else {
                    object$x.scales$alternating <- 1
                }
            else if (is.numeric(scales$x$alternating)) {
                object$x.scales$alternating <- scales$x$alternating
            }
        }
        
        
        
        
        
        if ("y" %in% names(scales) &&
            "alternating" %in% names(scales$y)){
            if (is.logical(scales$y$alternating))
                if (scales$y$alternating){
                    object$y.scales$alternating <- c(1,2)
                }
                else {
                    object$y.scales$alternating <- 1
                }
            else if (is.numeric(scales$y$alternating)) {
                object$y.scales$alternating <- scales$y$alternating
            }
        }


        object$x.scales[names(scales)] <- scales
        if ("x" %in% names(scales)){
                
            object$x.scales[names(scales$x)] <- scales$x
                
            if ("relation" %in% names(scales$x))
                warning("relation cannot be changed via update")
        }
        
        object$y.scales[names(scales)] <- scales
        if ("y" %in% names(scales)){
                
            object$y.scales[names(scales$y)] <- scales$y
                
            if ("relation" %in% names(scales$y))
                warning("relation cannot be changed via update")
        }

    }

    object
}
