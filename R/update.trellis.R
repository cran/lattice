
update.trellis <-
    function(foo,
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
             ylim)
{
    
    if(!missing(aspect)){
        if (is.numeric(aspect)){
            foo$aspect.ratio <- aspect
            foo$aspect.fill <- FALSE
        }
        else if (is.character(aspect) && aspect=="fill")
            foo$aspect.fill <- TRUE
        else warning("Inappropriate value of aspect")
    }
    
    if(!missing(as.table)){
        if (is.logical(as.table)) foo$as.table <- as.table
        else warning("Inappropriate value of as.table")
    }
    
    if(!missing(between)){
        if ("x" %in% names(between)) foo$x.between <- between$x
        if ("y" %in% names(between)) foo$y.between <- between$y
    }
    
    if(!missing(key)){
        foo$key <- key
    }
    
    if(!missing(layout)){
        if (length(layout)==2){
            foo$layout[3] <- ceiling(foo$layout[1]*foo$layout[2]*foo$layout[3]/
                                     max(layout[1]*layout[2], layout[2]))
            foo$layout[1] <- layout[1]
            foo$layout[2] <- layout[2]
        }
        else if (length(layout)==3){
            foo$layout <- layout
        }
        else warning("Inappropriate value of layout")
    }



    
    if(!missing(main)){
        if (is.character(main))
            if (is.null(foo$main)) foo$main <-
                list(label = main, col = "black", cex = 1, font = 1)
            else foo$main$label <- main

        else if (is.list(main)) {
            foo$main[names(main)] <- main
        }
        else if (is.null(main)) foo$main <- NULL
    }
    
    if(!missing(sub)){
        if (is.character(sub))
            if (is.null(foo$sub)) foo$sub <-
                list(label = sub, col = "black", cex = 1, font = 1)
            else foo$sub$label <- sub

        else if (is.list(sub)) {
            foo$sub[names(sub)] <- sub
        }
        else if (is.null(sub)) foo$sub <- NULL
    }
    
    if(!missing(xlab)){
        if (is.character(xlab))
            if (is.null(foo$xlab)) foo$xlab <-
                list(label = xlab, col = "black", cex = 1, font = 1)
            else foo$xlab$label <- xlab

        else if (is.list(xlab)) {
            foo$xlab[names(xlab)] <- xlab
        }
        else if (is.null(xlab)) foo$xlab <- NULL
    }
    
    if(!missing(ylab)){
        if (is.character(ylab))
            if (is.null(foo$ylab)) foo$ylab <-
                list(label = ylab, col = "black", cex = 1, font = 1)
            else foo$ylab$label <- ylab

        else if (is.list(ylab)) {
            foo$ylab[names(ylab)] <- ylab
        }
        else if (is.null(ylab)) foo$ylab <- NULL
    }
    

    
    if(!missing(page)){
        foo$page <- page
    }
    
    if(!missing(panel)){
        if (foo$fname == "splom")
            foo$panel.args.common$panel <- panel
        else foo$panel <- panel
    }
    
    if(!missing(par.strip.text)){
        if (is.list(par.strip.text))
            foo$par.strip.text[names(par.strip.text)] <-
                par.strip.text
        else warning("par.strip.text must be a list")
    }
    
    if(!missing(skip)){
        warning("sorry, but skip cannot be changed by update")
    }
    
    if(!missing(strip)){
        if (is.logical(strip)){
            if (strip) foo$strip <- strip.default
        else foo$strip <- FALSE
        }
        else foo$strip <- strip
    }
    
    if(!missing(xlim)){
        if (foo$x.relation.same) foo$x.scales$limits <- xlim
        else warning("xlim cannot be specified unless relation = same")
    }
    
    if(!missing(ylim)){
        if (foo$y.relation.same) foo$y.scales$limits <- ylim
        else warning("ylim cannot be specified unless relation = same")
    }

    if(!missing(scales)){
        
        if ("relation" %in% names(scales))
            warning("relation cannot be changed via update")
        
      

      
        if ("alternating" %in% names(scales)){
            if (is.logical(scales$alternating))
                if (scales$alternating){
                    foo$x.alternating <- c(1,2)
                    foo$y.alternating <- c(1,2)
                }
                else {
                    foo$x.alternating <- 1
                    foo$y.alternating <- 1
                }
            else if (is.numeric(scales$alternating)) {
                foo$x.alternating <- scales$alternating
                foo$y.alternating <- scales$alternating
            }
        }
        
        
        
        
        if ("x" %in% names(scales) &&
            "alternating" %in% names(scales$x)){
            if (is.logical(scales$x$alternating))
                if (scales$x$alternating){
                    foo$x.alternating <- c(1,2)
                }
                else {
                    foo$x.alternating <- 1
                }
            else if (is.numeric(scales$x$alternating)) {
                foo$x.alternating <- scales$x$alternating
            }
        }
        
        
        
        
        
        if ("y" %in% names(scales) &&
            "alternating" %in% names(scales$y)){
            if (is.logical(scales$y$alternating))
                if (scales$y$alternating){
                    foo$y.alternating <- c(1,2)
                }
                else {
                    foo$y.alternating <- 1
                }
            else if (is.numeric(scales$y$alternating)) {
                foo$y.alternating <- scales$y$alternating
            }
        }
        
        
        
        if (foo$x.relation.same){
            foo$x.scales[names(scales)] <- scales
            if ("x" %in% names(scales)){
                
                foo$x.scales[names(scales$x)] <- scales$x
                
                if ("relation" %in% names(scales$x))
                    warning("relation cannot be changed via update")
            }
        }
        else
            warning("sorry, x-scales cannot be changed unless x-relation = same")
        
        if (foo$y.relation.same){
            foo$y.scales[names(scales)] <- scales
            if ("y" %in% names(scales)){
                
                foo$y.scales[names(scales$y)] <- scales$y
                
                if ("relation" %in% names(scales$y))
                    warning("relation cannot be changed via update")
            }
        }
        else
            warning("sorry, y-scales cannot be changed unless y-relation = same")
    }


    foo
}
