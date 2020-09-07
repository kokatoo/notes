data <- c(41, 27, 32, 24, 21, 28, 22, 25, 35, 27, 31, 40, 23,
          27, 29, 33, 42, 30, 26, 30, 27, 39, 26, 34, 28, 38,
          29, 36, 24, 37)

stats <- psych::describe(data)
stats$mean
stats$median
as.numeric(names(which.max(table(data))))

median(c(1, 5, 7, 8, 9))
median(c(3, 5, 6, 8, 9, 10))
median(c(3, 4, 4, 4, 6, 20))
median(c(2, 4, 5, 5, 8, 9))

boxplot(c(3, 4, 4, 4, 6, 20))$out

data <- c(130, 110, 160, 120, 170, 120, 150, 140, 160, 140)
as.data.frame(psych::describe(data.frame(data)))[c("mean", "range", "sd")]
var(data)

zscores <- scale(data)[, 1]
cbind(data, zscores, transformed = zscores * 100 + 500)
