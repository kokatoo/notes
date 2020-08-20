## ---- 11.1 Scatter plots
png(file = "./images/chp11-plot1.png")
attach(mtcars)
plot(wt,
  mpg,
  main = "Basic Scatter plot of MPG vs. Weight",
  xlab = "Car Weight (lbs/1000)",
  ylab = "Miles Per Gallon ",
  pch = 19
)
abline(lm(mpg ~ wt), col = "red", lwd = 2, lty = 1)
lines(lowess(wt, mpg), col = "blue", lwd = 2, lty = 2)
dev.off()
detach(mtcars)

library(car)

png(file = "./images/chp11-plot2.png")
scatterplot(
  mpg ~ wt | cyl,
  data = mtcars,
  lwd = 2,
  main = "Scatter Plot of MPG vs. Weight by # Cylinders",
  xlab = "Weight of Car (lbs/1000)",
  ylab = "Miles Per Gallon",  
  boxplots = "xy"
)
dev.off()

## --- 11.1.1 Scatter-plot matrices
png(file = "./images/chp11-plot3.png")
pairs(
  ~ mpg + disp + drat + wt,
  data = mtcars,
  main = "Basic Scatter Plot Matrix"
)
dev.off()

png(file = "./images/chp11-plot22.png")
pairs(
  ~ mpg + disp + drat + wt,
  data = mtcars,
  upper.panel = NULL,
  main = "Basic Scatter Plot Matrix"
)
dev.off()

png(file = "./images/chp11-plot4.png")
library(car)
scatterplotMatrix(
  ~ mpg + disp + drat + wt,
  data = mtcars,
  col = palette()[1],
  smooth = list(col.smooth = palette()[2], col.spread = palette()[4]),
  regLine = list(col = palette()[3]),
  upper.panel = NULL,
  main = "Scatter Plot Matrix via car Package"
)
dev.off()

## --- 11.1.2 High-density scatter plots
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean = 0, sd = .5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2), ncol = 2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
png(file = "./images/chp11-plot5.png")
with(
  mydata,
  plot(x, y, pch = 19, main = "Scatter Plot with 10,000 Observations")
)
dev.off()

png(file = "./images/chp11-plot6.png")
with(
  mydata,
  smoothScatter(x, y, main = "Scatter Plot Colored by Smoothed Densities")
)
dev.off()

png(file = "./images/chp11-plot7.png")
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins = 50)
  plot(bin, main = "Hexagonal Binning with 10,000 Observations")
})
dev.off()

## --- 11.1.3 3D scatter plots
library(scatterplot3d)
attach(mtcars)

png(file = "./images/chp11-plot8.png")
scatterplot3d(
  wt,
  disp,
  mpg,
  main = "Basic 3D Scatter Plot"
)
dev.off()

png(file = "./images/chp11-plot9.png")
scatterplot3d(wt,
  disp,
  mpg,
  pch = 16,
  highlight.3d = TRUE,
  type = "h",
  main = "3D Scatter Plot with Vertical Lines"
)
dev.off()

library(scatterplot3d)
attach(mtcars)

png(file = "./images/chp11-plot10.png")
s3d <- scatterplot3d(
  wt,
  disp,
  mpg,
  pch = 16,
  highlight.3d = TRUE,
  type = "h",
  main = "3D Scatter Plot with Vertical Lines and Regression Plane"
)
fit <- lm(mpg ~ wt + disp)
s3d$plane3d(fit)
dev.off()

## --- 11.1.4 Spinning 3D scatter plots

library(rgl)
attach(mtcars)

plot3d(wt, disp, mpg, col = "red", size = 5)

library(car)
with(mtcars,
     scatter3d(wt, disp, mpg))

## --- 11.1.5 Bubble plots
attach(mtcars)
r <- sqrt(disp / pi)
png(file = "./images/chp11-plot13.png")
symbols(
  wt,
  mpg,
  circle = r,
  inches = 0.30,
  fg = "white",
  bg = "lightblue",
  main = "Bubble Plot with point size proportional to displacement",
  ylab = "Miles Per Gallon",
  xlab = "Weight of Car (lbs/1000)"
)
text(wt, mpg, rownames(mtcars), cex = 0.6)
dev.off()
detach(mtcars)

## --- 11.2 Line charts
opar <- par(no.readonly = TRUE)

png(file = "./images/chp11-plot14.png")
par(mfrow = c(1, 2))
t1 <- subset(Orange, Tree == 1)
plot(
  t1$age,
  t1$circumference,
  xlab = "Age (days)",
  ylab = "Circumference (mm)",
  main = "Orange Tree 1 Growth"
)
plot(
  t1$age,
  t1$circumference,
  xlab = "Age (days)",
  ylab = "Circumference (mm)",
  main = "Orange Tree 1 Growth",
  type = "b"
)
dev.off()
par(opar)

opar <- par(no.readonly = TRUE)
png(file = "./images/chp11-plot23.png")
types <- c("p", "l", "o", "b",
           "c", "s", "S", "h")
par(mfrow = c(2, 4))
for (t in types) {
  plot(
    t1$age,
    t1$circumference,
    xlab = "Age (days)",
    ylab = "Circumference (mm)",
    main = "Orange Tree 1 Growth",
    type = t
  )
}
dev.off()
par(opar)

Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)

png(file = "./images/chp11-plot15.png")
plot(
  xrange,
  yrange,
  type = "n",
  xlab = "Age (days)",
  ylab = "Circumference (mm)"
)
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18 + ntrees, 1)

for (i in 1:ntrees) {
  tree <- subset(Orange, Tree == i)

  lines(
    tree$age,
    tree$circumference,
    type = "b",
    lwd = 2,
    lty = linetype[i],
    col = colors[i],
    pch = plotchar[i]
  )
}

title("Tree Growth", "example of line plot")
legend(
  xrange[1],
  yrange[2],
  1:ntrees,
  cex = 0.8,
  col = colors,
  pch = plotchar,
  lty = linetype,
  title = "Tree"
)
dev.off()

## ---- 11.3 Corrgrams
options(digits = 2)
cor(mtcars)
library(corrgram)

png(file = "./images/chp11-plot16.png")
corrgram(
  mtcars,
  order = TRUE,
  lower.panel = panel.shade,
  upper.panel = panel.pie,
  text.panel = panel.txt,
  main = "Corrgram of mtcars intercorrelations"
)
dev.off()

library(corrgram)
png(file = "./images/chp11-plot17.png")
corrgram(
  mtcars,
  order = TRUE,
  lower.panel = panel.ellipse,
  upper.panel = panel.pts,
  text.panel = panel.txt,
  diag.panel = panel.minmax,
  main = "Corrgram of mtcars data using scatter plots and ellipses"
)
dev.off()

library(corrgram)
png(file = "./images/chp11-plot18.png")
corrgram(
  mtcars,
  lower.panel = panel.shade,
  upper.panel = NULL,
  text.panel = panel.txt,
  main = "Car Mileage Data (unsorted)"
)
dev.off()

png(file = "./images/chp11-plot19.png")
library(corrgram)
cols <- colorRampPalette(
  c(
    "darkgoldenrod4",
    "burlywood1",
    "darkkhaki",
    "darkgreen"
  )
)
corrgram(
  mtcars,
  order = TRUE,
  col.regions = cols,
  lower.panel = panel.shade,
  upper.panel = panel.conf,
  text.panel = panel.txt,
  main = "A Corrgram (or Horse) of a Different Color"
)
dev.off()

## ---- 11.4 Mosaic plots
ftable(Titanic)

png(file = "./images/chp11-plot20.png")
library(vcd)
mosaic(Titanic, shade=TRUE, legend=TRUE)
dev.off()

png(file = "./images/chp11-plot21.png")
library(vcd)
mosaic(
  ~ Class + Sex + Age + Survived,
  data = Titanic, shade = TRUE,
  legend = TRUE)
dev.off()
