## -- 3.4.2 Axes
x <- c(1:10)
y <- x
z <- 10 / x

opar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 4, 8) + 0.1)

plot(
  x,
  y,
  type = "b",
  pch = 21,
  col = "red",
  yaxt = "n",
  lty = 3,
  ann = FALSE
)

lines(
  x,
  z,
  type = "b",
  pch = 22,
  col = "blue",
  lty = 2
)

axis(
  2,
  at = x,
  labels = x,
  col.axis = "red",
  las = 2
)

axis(
  4,
  at = z,
  labels = round(z, digits = 2),
  col.axis = "blue",
  las = 2,
  cex.axis = 0.7,
  tck = -0.01
)

mtext("y=1/x", side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")
title("An Example of Creative Axes", xlab = "X values", ylab = "Y=X")

par(opar)

## -- 3.4.4 Legend
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

opar <- par(no.readonly = TRUE)

png(file = "./images/chp03-plot4.png")
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(
  dose,
  drugA,
  type = "b",
  pch = 15,
  lty = 1,
  col = "red",
  ylim = c(0, 60),
  main = "Drug A vs. Drug B",
  xlab = "Drug Dosage",
  ylab = "Drug Response"
)

lines(dose,
  drugB,
  type = "b",
  pch = 17,
  lty = 2,
  col = "blue"
)
abline(h = c(30), lwd = 1.5, lty = 2, col = "gray")

library(Hmisc)

minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
legend(
  "topleft", # or locator(1)
  inset = .05,
  title = "Drug Type",
  c("A", "B"),
  lty = c(1, 2),
  pch = c(15, 17),
  col = c("red", "blue")
)
dev.off()
par(opar)

## --- 3.4.5 Text annotations
png(file = "./images/chp03-plot5.png")
attach(mtcars)
plot(
  wt,
  mpg,
  main = "Mileage vs. Car Weight",
  xlab = "Weight",
  ylab = "Mileage",
  pch = 18,
  col = "blue"
)

text(
  wt,
  mpg,
  row.names(mtcars),
  cex = 0.6,
  pos = 4,
  col = "red"
)
detach(mtcars)
dev.off()

opar <- par(no.readonly = TRUE)
png(file = "./images/chp03-plot6.png")
par(cex = 1.5)
plot(1:7, 1:7, type = "n")
text(3, 3, "Example of default text")
text(4, 4, family = "mono", "Example of mono-spaced text")
text(5, 5, family = "serif", "Example of serif text")
dev.off()
par(opar)

png(file = "./images/chp03-plot7.png")
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)
dev.off()
