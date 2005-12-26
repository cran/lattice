
library(lattice)

postscript("na-tests.ps")

x <- 1:20
y <- 1:20
z <- rep(1:3, length = 20)
g <- factor(sample(letters[1:2], 20, TRUE), levels = letters[1:3])

x[2] <- NA
y[5] <- NA
z[8] <- NA

f <- factor(z)


bwplot(f ~ x)
bwplot(g ~ x)

barchart(f ~ x)
barchart(f ~ x, groups = g)
barchart(f ~ x, groups = factor(g))

dotplot(f ~ x)
stripplot(f ~ x)

densityplot(~x)
histogram(~x)
qqmath(~x)


qq(g ~ x)
qq(factor(g) ~ x)

xyplot(y ~ x)
xyplot(y ~ x, groups = g)

levelplot(z ~ x * y)
## contourplot(z ~ x * y) # fails
cloud(z ~ x * y)

cloud(z ~ x * y, type = 'l', screen = list(z = 10, x = -60))

## wireframe(z ~ x * y) # fails
splom(data.frame(x, y, z))
parallel(data.frame(x, y, z))



dev.off()
