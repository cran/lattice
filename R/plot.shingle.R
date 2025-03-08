

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



## not quite what it should be
plot.shingle <-
    function(x, col = bar.fill$col, aspect = "fill", ...)
{

    bar.fill <- trellis.par.get("bar.fill")
    foo <- list(call = match.call(),
                aspect.fill = aspect == "fill",
                aspect.ratio = if (is.numeric(aspect)) aspect else 1,
                as.table = FALSE,
                condlevels = "1",
                key = NULL,
                layout=c(1,1,1),
                page = NULL,
                panel = function(x, col) {
                    ## x is the list of intervals
                    num.l.y <- length(x)
                    if (num.l.y>0)
                        for(i in 1:num.l.y)
                            grid.rect(x = x[[i]] %*% c(.5,.5),
                                      y = i,
                                      width = diff(x[[i]]),
                                      height = .5,
                                      default.units = "native",
                                      gp = gpar(fill=col)) 
                },
                panel.args = list(list()),
                panel.args.common = list(x=levels(x), col = col),
                par.strip.text = trellis.par.get("add.text"),
                skip = FALSE,
                strip = FALSE,
                main = NULL,
                sub = NULL,
                xlab = list(label = "Range", col = "black", cex = 1, font =1),
                ylab = list(label = "Panel", col = "black", cex = 1, font =1),
                x.scales = 1,
                y.scales = 1,
                x.between = 0,
                y.between = 0,
                x.alternating = 1,
                y.alternating = 1,
                fontsize.normal = 10,
                fontsize.small = 8)
    
    num.l.y <- nlevels(x)
    foo$x.limits <- extend.limits(range(x, levels(x)))
    foo$y.limits <- extend.limits(c(1,num.l.y),
                                  length = .5+num.l.y)


    foo$x.scales <- list(relation = "same",
                         draw = TRUE,
                         alternating = 1,
                         at = FALSE,
                         labels = FALSE,
                         tck = c(1, 1),
                         font = 1,
                         col = FALSE,
                         log = FALSE,
                         cex = c(1, 1),
                         rot = c(FALSE, FALSE),
                         tick.number = 5)
    
    foo$y.scales <- list(relation = "same",
                         draw = TRUE,
                         alternating = 1,
                         at = 1:num.l.y,
                         labels = FALSE,
                         tck = c(1, 1),
                         font = 1,
                         col = FALSE,
                         log = FALSE,
                         cex = c(1, 1),
                         rot = c(FALSE, FALSE),
                         tick.number = num.l.y)
    
    class(foo) <- "trellis"
    foo
    
}




