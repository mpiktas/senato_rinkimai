set.seed(12345)
n = 1000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE), x = rnorm(n), y = runif(n))

library(circlize)
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
 
circos.trackPlotRegion(factors = a$factor, y = a$y, panel.fun = function(x, y) { circos.axis()  })
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(a$factor, a$x, a$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.text(1, 0.5, "right", sector.index = "a")
