undergrad_gpa <- c(3.4, 3.1, 3.7, 3.2, 3.5, 2.9, 3.3, 2.9, 3.4, 3.2, 3.7, 3.0, 3.1, 3.7, 3.9, 3.5, 3.1, 2.9, 3.2, 3.6)
gre_verbal <- c(600, 510, 650, 530, 610, 540, 530, 540, 550, 700, 630, 480, 530, 580, 710, 500, 490, 560, 550, 600)
gre_quant <- c(540, 480, 710, 450, 500, 620, 510, 600, 580, 630, 700, 490, 520, 610, 660, 480, 510, 540, 590, 550)
motivation <- c(75, 70, 85, 60, 90, 60, 75, 55, 75, 65, 80, 75, 60, 65, 80, 75, 60, 55, 65, 60)
graduate_gpa <- c(3.6, 3.0, 3.9, 2.8, 3.7, 2.6, 3.4, 2.7, 3.3, 3.5, 3.6, 2.8, 3.0, 3.5, 3.8, 3.2, 2.4, 2.7, 3.1, 3.6)

data <- data.frame(
  graduate_gpa,
  undergrad_gpa,
  gre_verbal,
  gre_quant,
  motivation
)

fit <- lm(
  graduate_gpa ~ undergrad_gpa + gre_verbal + gre_quant + motivation,
  data = data
)

predict(fit)
summary(fit)$residuals^2

f_ratio <- function(r2, k, n) {
  (r2 / k) / (
    (1 - r2) / (n - k - 1)
  )
}

f_ratio(0.53, 5, 120)
qf(0.95, 5, (120 - 5 - 1))

f_ratio(0.53, 5, 30)
qf(0.95, 5, (30 - 5 - 1))

f_ratio(0.28, 4, 64)
qf(0.95, 4, (64 - 4 - 1))

f_ratio(0.14, 4, 64)
qf(0.95, 5, (64 - 4 - 1))

f_ratio(0.53, 5, 120)
qf(0.99, 5, (120 - 5 - 1))

f_ratio(0.53, 5, 30)
qf(0.99, 5, (30 - 5 - 1))

f_ratio(0.28, 4, 64)
qf(0.99, 4, (64 - 4 - 1))

f_ratio(0.14, 4, 64)
qf(0.99, 5, (64 - 4 - 1))

psychometric::CI.Rsq(0.22, 100, 6)
psychometric::CI.Rsq(0.22, 200, 6)
psychometric::CI.Rsq(0.22, 100, 10)

pwr::pwr.r.test(r = sqrt(0.20), sig.level = 0.05, power = 0.80)
pwr::pwr.r.test(r = sqrt(0.13), sig.level = 0.05, power = 0.80)
pwr::pwr.r.test(r = sqrt(0.20), sig.level = 0.05, power = 0.80)

pwr::pwr.f2.test(
  u = 6,
  f2 = 0.20 / (1 - 0.20),
  sig.level = 0.05,
  power = 0.80
)$v + 6 + 1

pwr::pwr.f2.test(
  u = 8,
  f2 = 0.13 / (1 - 0.13),
  sig.level = 0.05,
  power = 0.80
)$v + 8 + 1

pwr::pwr.f2.test(
  u = 4,
  f2 = 0.08 / (1 - 0.08),
  sig.level = 0.05,
  power = 0.80
)$v + 4 + 1
