

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


plot.shingle <-
    function(x, col = bar.fill$col)
{

    bar.fill <- trellis.par.get("bar.fill")
    foo <- list(formula = NULL, 
                fname = "plot.shingle",
                aspect.fill = T,
                aspect.ratio = 1,
                as.table = F,
                cond = NULL,
                key = NULL,
                layout=c(1,1,1),
                page = NULL,
                panel = function(x, col) {
                    ## x is the matrix of intervals
                    num.l.y <- nrow(x)
                    for(i in seq(along = x[,1]))
                        grid.rect(x = x[i,] %*% c(.5,.5),
                                  y = i,
                                  width = diff(x[i,]),
                                  height = .5,
                                  default.units = "native",
                                  gp = gpar(fill=col)) 
                },
                panel.args = list(),
                panel.args.common = list(x=x$int, col = col),
                par.strip.text = trellis.par.get("add.text"),
                skip = F,
                strip = strip.default,
                main = NULL,
                sub = NULL,
                xlab = list(label = "Range", col = "black", cex = 1, font =1),
                ylab = list(label = "Panel", col = "black", cex = 1, font =1),
                x.draw = TRUE,
                y.draw = TRUE,
                x.scales = NULL,
                y.scales = NULL,
                x.between = 0,
                y.between = 0,
                x.relation.same = TRUE,
                y.relation.same = TRUE,
                x.alternating = 1,
                y.alternating = 1,
                fontsize.normal = 10,
                fontsize.small = 8)
    
    num.l.y <- nrow(x$int)

    foo$x.scales <- list(limits = extend.limits(range(x$x, x$int)),
                         at = F,
                         labels = F,
                         tck = 1,
                         col = F,
                         cex = 1,
                         rot = F,
                         tick.number = 5)
    
    foo$y.scales <- list(limits =
                         extend.limits(c(1,num.l.y),
                                       length = .5+num.l.y),
                         at = 1:num.l.y,
                         labels = F,
                         tck = 1,
                         col = F,
                         cex = 1,
                         rot = F,
                         tick.number = num.l.y)
    
    class(foo) <- "trellis"
    foo
    
}


