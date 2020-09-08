t_stat <- function(r, n) {
  r / sqrt(1 - r^2) * sqrt(n - 2)
}

1 - pt(t_stat(0.29, 35), df = 35 - 2)
1 - pt(t_stat(0.50, 15), df = 15 - 2)
1 - pt(t_stat(0.12, 500), df = 500 - 2)
1 - pt(t_stat(0.55, 12), df = 12 - 2)
1 - pt(t_stat(0.44, 26), df = 26 - 2)

0.76^2
0.33^2
0.91^2
0.14^2

sqrt(0.66)
sqrt(0.13)
sqrt(0.29)
sqrt(0.07)

6.5 - -0.1 * 33

predict_y <- function(x) {
  9.8 - 0.1 * x
}

predict_y(52)
predict_y(68)
predict_y(23)
predict_y(10)

pwr::pwr.r.test(n = 75, r = 0.19, sig.level = 0.05)$power
1 - pwr::pwr.r.test(n = 75, r = 0.19, sig.level = 0.05)$power

pwr::pwr.r.test(power = 0.8, r = 0.19, sig.level = 0.05)$n
