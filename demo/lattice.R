

par(ask = TRUE)
## changing settings to new 'theme'
lset(theme = "white.bg")

## simulated example, histogram and kernel density estimate superposed
x <- rnorm(500)
densityplot(~x)
histogram(x, type = "density", 
          panel = function(x, ...) {
              panel.histogram(x, ...) 
              panel.densityplot(x, col = "brown", plot.points = F)
          })

## Using a custom panel function to superpose a fitted normal density
## on a Kernel Density Estimate
data(singer)
densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)",
            ylab = "Kernel Density\n with Normal Fit",
            main = list("Estimated Density", cex = 1.4, col = "DarkOliveGreen"),
            panel = function(x, ...) {
                panel.xyplot(x = jitter(x),
                             y = rep(0, length(x)))
                panel.densityplot(x, ...)
                panel.mathdensity(dmath = dnorm,
                                  args = list(mean=mean(x),sd=sd(x)))
            } )

## user defined panel functions and fonts
data(state)
states <- data.frame(state.x77,
                     state.name = dimnames(state.x77)[[1]], 
                     state.region = factor(state.region)) 

xyplot(Murder  ~ Population | state.region, data = states, 
       groups = as.character(state.name), 
       panel = function(x, y, subscripts, groups)  
       ltext(x=x, y=y, label=groups[subscripts], cex=.7, font=3),
       par.strip.text = list(cex = 1.3, font = 4, col = "brown"),
       xlab = list("Estimated Population, July 1, 1975", font = 2),
       ylab = list("Murder Rate (per 100,000 population), 1976", font = 2),
       main = list("Murder Rates in US states", col = "brown", font = 4))

##graphical parameters for xlab etc can also be changed permanently
lset.textfont(xlab = 2, ylab = 2, main = 4)
lset.textcol(main = "brown")

## Same with some multiple line text
levels(states$state.region) <- c("Northeast", "South", "North\n Central",  "West")
xyplot(Murder  ~ Population | state.region, data = states,
       groups = as.character(state.name),
       panel = function(x, y, subscripts, groups)
       grid.text(x=x, y=y, label=groups[subscripts], default.units = "native",
                 gp = gpar(fontsize = 10, font=2, col = "blue"), rot = -50),
       par.strip.text = list(cex = 1.3, font = 4, col = "brown", lines = 2),
       xlab = "Estimated Population\nJuly 1, 1975", 
       ylab = "Murder Rate \n(per 100,000 population)\n 1976", 
       main = "Murder Rates in US states")

##setting these back to their defaults
lset.textfont(xlab = 1, ylab = 1, main = 2)
lset.textcol(main = "black")

##levelplot
data(volcano)
levelplot(volcano, colorkey = list(space = "top"),
          sub = "Maunga Whau volcano")

## Example with panel.superpose. 
data(iris)
xyplot(Petal.Length~Petal.Width, data = iris, groups=Species, 
       panel = panel.superpose,
       type = c("p", "smooth"), span=.75,
       col.line = trellis.par.get("strip.background")$col,
       col.symbol = trellis.par.get("strip.shingle")$col,
       key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
           border = TRUE, 
           points = list(col=trellis.par.get("strip.shingle")$col[1:3],
                         pch = 1),
           text = list(levels(iris$Species))))

par(ask = FALSE)

