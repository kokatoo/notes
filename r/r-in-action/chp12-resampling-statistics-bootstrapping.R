## ---- 12.1 Permutation tests
## --- 12.2.1 Independent two-sample and k-sample tests
library(coin)
score <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
treatment <- factor(c(rep("A", 5), rep("B", 5)))
mydata <- data.frame(treatment, score)
t.test(score ~ treatment, data = mydata, var.equal = TRUE)

oneway_test(score ~ treatment, data = mydata, distribution = "exact")

library(MASS)
UScrime <- transform(UScrime, So = factor(So))
wilcox_test(Prob ~ So, data = UScrime, distribution = "exact")

library(multcomp)
set.seed(1234)
oneway_test(
  response ~ trt,
  data = cholesterol,
  distribution = approximate(B = 9999)
)

## --- 12.2.2 Independence in contingency tables
library(coin)
library(vcd)
Arthritis <- transform(
  Arthritis,
  Improved = as.factor(as.numeric(Improved))
)
set.seed(1234)
chisq_test(
  Treatment ~ Improved,
  data = Arthritis,
  distribution = approximate(B = 9999)
)

## --- 12.2.3 Independence between numeric variables
states <- as.data.frame(state.x77)
set.seed(1234)
spearman_test(
  Illiteracy ~ Murder,
  data = states,
  distribution = approximate(B = 9999)
)

## --- 12.2.4 Dependent two-sample and k-sample tests
library(coin)
library(MASS)
wilcoxsign_test(U1 ~ U2, data = UScrime, distribution = "exact")

## --- 12.3 Permutation tests with the lmPerm package
## -- 12.3.1 Simple and polynomial regression
library(lmPerm)
set.seed(1234)
fit <- lmp(weight ~ height, data = women, perm = "Prob")
summary(fit)

library(lmPerm)
set.seed(1234)
fit <- lmp(
  weight ~ height + I(height^2),
  data = women,
  perm = "Prob"
)
summary(fit)

## --- 12.3.2 Multiple regression
library(lmPerm)
set.seed(1234)
states <- as.data.frame(state.x77)
fit <- lmp(
  Murder ~ Population + Illiteracy + Income + Frost,
  data = states,
  perm = "Prob"
)
summary(fit)

## --- 12.3.3 One-way ANOVA and ANCOVA
## -- Permutation test for one-way ANOVA
library(lmPerm)
library(multcomp)

set.seed(1234)
fit <- aovp(response ~ trt, data = cholesterol, perm = "Prob")
anova(fit)

## -- Permutation test for one-way ANCOVA
library(lmPerm)
set.seed(1234)
fit <- aovp(
  weight ~ gesttime + dose,
  data = litter,
  perm = "Prob"
)
anova(fit)

## --- 12.3.4 Two-way ANOVA
library(lmPerm)
set.seed(1234)

fit <- aovp(len ~ supp * dose,
  data = ToothGrowth,
  perm = "Prob"
)
anova(fit)

## ---- 12.4 Additional comments on permutation tests
## ---- 12.5 Bootstrapping
## ---- 12.6 Bootstrapping with the boot package

## --- 12.6.1 Bootstrapping a single statistic
rsq <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}

library(boot)
set.seed(1234)
results <- boot(
  data = mtcars,
  statistic = rsq,
  R = 1000,
  formula = mpg ~ wt + disp
)

print(results)

png(file = "./images/chp12-plot1.png")
plot(results)
dev.off()

boot.ci(results, type = c("perc", "bca"))

## --- 12.6.2 Bootstrapping several statistics
bs <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

library(boot)
set.seed(1234)
results <- boot(
  data = mtcars,
  statistic = bs,
  R = 1000,
  formula = mpg ~ wt + disp
)

print(results)

png(file = "./images/chp12-plot2.png")
plot(results, index = 1)
dev.off()

png(file = "./images/chp12-plot3.png")
plot(results, index = 2)
dev.off()

png(file = "./images/chp12-plot4.png")
plot(results, index = 3)
dev.off()

boot.ci(results, type = "bca", index = 1)
boot.ci(results, type = "bca", index = 2)
boot.ci(results, type = "bca", index = 3)
