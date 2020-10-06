trt <- multcomp::cholesterol$trt
response <- multcomp::cholesterol$response

table(trt)
aggregate(response, by = list(trt), FUN = mean)
aggregate(response, by = list(trt), FUN = sd)

fit <- aov(response ~ trt)
summary(fit)

gplots::plotmeans(
  response ~ trt,
  xlab = "Treatment",
  ylab = "Response"
  )

TukeyHSD(fit)

par(las = 2)
par(mar = c(5, 8, 4, 2))
plot(TukeyHSD(fit))

par(mar = c(5, 4, 6, 2))
tuk <- multcomp::glht(fit, linfct = multcomp::mcp(trt = "Tukey"))
plot(multcomp::cld(tuk, level = .05), col = "lightgrey")

car::qqPlot(lm(response ~ trt, data = multcomp::cholesterol))
bartlett.test(response ~ trt, data = multcomp::cholesterol)

car::outlierTest(fit)
