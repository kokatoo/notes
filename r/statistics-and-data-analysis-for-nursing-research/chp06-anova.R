nonsmokers <- c(19, 23, 17, 20, 26)
smokers <- c(26, 29, 22, 30, 23)
quitters <- c(37, 32, 27, 41, 38)

data <- data.frame(nonsmokers, smokers, quitters)
data <- as.matrix(data)
apply(data, 2, mean)
mean(data)

n <- length(data)
sum_squares_total <- sum(data^2) - sum(data)^2 / n
sum_squares_total

sum_squares_within <- sum(apply(data^2, 2, sum) - (apply(data, 2, sum)^2 / nrow(data)))
sum_squares_within

sum_squares_between <- sum((apply(data, 2, mean) - mean(data))^2 * nrow(data))
sum_squares_between

df_between <- ncol(data) - 1
df_between

df_within <- n - df_between - 1
df_within

mean_squares_between <- sum_squares_between / df_between
mean_squares_between

mean_squares_within <- sum_squares_within / df_within
mean_squares_within

f_ratio <- mean_squares_between / mean_squares_within
f_ratio

1 - pf(f_ratio, df1 = df_between, df2 = df_within)

data_melt <- reshape2::melt(data)[, -1]
data_melt
names(data_melt) <- c("Group", "Score")

fit <- aov(Score ~ Group, data = data_melt)
summary(fit)

t_alpha <- qt(0.975, df = df_within)
t_alpha

lsd <- t_alpha * sqrt(mean_squares_within * 2 / 5)
lsd

(mean(data[, "smokers"]) - mean(data[, "nonsmokers"])) /
  sqrt(mean_squares_within * (1 / 5 + 1 / 5))
(mean(data[, "quitters"]) - mean(data[, "nonsmokers"])) /
  sqrt(mean_squares_within * (1 / 5 + 1 / 5))
(mean(data[, "smokers"]) - mean(data[, "quitters"])) /
  sqrt(mean_squares_within * (1 / 5 + 1 / 5))

library(agricolae)
out <- LSD.test(fit, "Group", group = FALSE)
out$statistics
out$comparison

## A5
eta2 <- sum_squares_between / sum_squares_total
eta2

stats::power.anova.test(
  groups = 3,
  n = 5,
  between.var = sum_squares_between,
  within.var = sum_squares_within
)

pwr::pwr.anova.test(k = 3, n = 5, f = 0.833, power = NULL)

sqrt(sum_squares_between / sum_squares_total)

## A6
1 - pf(2.80, 4, 40)
1 - pf(5.02, 3, 60)
1 - pf(3.45, 3, 27)
1 - pf(4.99, 2, 150)
1 - pf(2.09, 2, 250)

## A7
n <- 4 * 20
mean_squares_A <- 74.5 / (2 - 1)
mean_squares_A

mean_squares_B <- 37.0 / (2 - 1)
mean_squares_B

mean_squares_AB <- 54 / (2 - 1) * (2 - 1)
mean_squares_AB

mean_squares_within <- 1025.0 / (n - (1 + 1 + 1 + 1))
mean_squares_within

1 - pf(mean_squares_A / mean_squares_within, df1 = 1, df2 = n - 3)
1 - pf(mean_squares_B / mean_squares_within, df1 = 1, df2 = n - 3)
1 - pf(mean_squares_AB / mean_squares_within, df1 = 1, df2 = n - 3)

((74.5 + 37 + 54) / 3) / mean_squares_within
((1190.5 - 1025) / 3) / mean_squares_within

## A8
n <- 15
df_site <- 3 - 1
df_subjects <- n - 1
df_error <- df_site * df_subjects
df_error

mean_squares_subject <- 17993 / df_site
mean_squares_subject

mean_squares_error <- 48349 / df_error
mean_squares_error

f_ratio <- mean_squares_subject / mean_squares_error
f_ratio

1 - pf(f_ratio, df_site, df_error)
