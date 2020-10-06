fit <- lm(weight ~ height, data = women)
summary(fit)
fitted(fit)
residuals(fit)

plot(
  women$height,
  women$weight,
  xlab = "Height (in inches)",
  ylab = "Weight (in pounds)"
)
abline(fit)

fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)
plot(
  women$height,
  women$weight,
  xlab = "Height (in inches)",
  ylab = "Weight (in pounds)"
)
lines(women$height, fitted(fit2))

car::scatterplot(
  weight ~ height,
  data = women,
  pch = 19,
  main = "Women Age 30-30",
  xlab = "Height (in inches)",
  ylab = "Weight (in pounds)"
  )

cols <- c("Murder", "Population", "Illiteracy", "Income", "Frost")
states <- as.data.frame(state.x77[, cols])

car::scatterplotMatrix(
  states,
  main = "Scatter Plot Matrix"
)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit)

fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)

plot(effects::effect("hp:wt", fit, , list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)
confint(fit)
par(mfrow = c(2, 2))
plot(fit)

par(mfrow = c(1, 1))
car::qqPlot(fit)
rstudent(fit)

residplot <- function(fit, nbreaks = 10) {
  z <- rstudent(fit)

  hist(z,
    breaks = nbreaks, freq = FALSE,
    xlab = "Studentized Residual",
    main = "Distribution of Errors"
  )
  rug(jitter(z), col = "brown")

  curve(dnorm(x, mean = mean(z), sd = sd(z)),
    add = TRUE, col = "blue", lwd = 2
  )

  lines(density(z)$x,
    density(z)$y,
    col = "red",
    lwd = 2, lty = 2
  )

  legend("topright",
    legend = c("Normal Curve", "Kernel Density Curve"),
    lty = 1:2, col = c("blue", "red"), cex = .7
  )
}

residplot(fit)

car::durbinWatsonTest(fit)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
car::crPlots(fit)

car::ncvTest(fit)
car::spreadLevelPlot(fit)

summary(gvlma::gvlma(fit))

car::vif(fit)

car::outlierTest(fit)
plot(hatvalues(fit))

car::influencePlot(fit)
summary(car::powerTransform(states$Murder))

car::boxTidwell(Murder ~ Population + Illiteracy, data = states)

fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)

anova(fit1, fit2)
AIC(fit1, fit2)

MASS::stepAIC(fit, direction = "forward")
MASS::stepAIC(fit, direction = "backward")

leaps <- leaps::regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = states, nbest = 4)
plot(leaps, scale = "adjr2")
car::subsets(leaps, statistic = "cp")

zfit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = as.data.frame(scale(states)))
coef(zfit)
