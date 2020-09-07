x1 <- 78.5
x2 <- 72.1

var1 <- 42.1
var2 <- 39.7

n <- 25

d1 <- ((n - 1) * var1 + (n - 1) * var2) / (n + n - 2)
d2 <- (1 / n + 1 / n)

t <- (x1 - x2) / sqrt(d1 * d2)
t

df <- n + n - 2
df

qt(0.975, df)
1 - pt(3.54, df)

data1 <- rt(n, df) * sqrt(var1) + x1
data2 <- rt(n, df) * sqrt(var2) + x2

t.test(data1, data2)

d <- (x1 - x2) / 7.05
d

res <- pwr::pwr.t.test(n = n, d = d, sig.level = 0.05)
res

res$power
1 - res$power

1 - pt(2.40, 25)
1 - pt(5.52, 10)
1 - pt(2.02, 150)

qt(0.975, 20 + 20 - 2)
qt(0.99, 30 + 30 - 2)
qt(0.995, 10 + 10 - 2)
qt(0.95, 60 + 60 - 2)
qt(0.995, 15 + 10 - 2)

res <- pwr::pwr.t.test(n = 30, d = 0.6, sig.level = 0.05)
res$power
1 - res$power

res <- pwr::pwr.t.test(power = 0.8, d = 0.6, sig.level = 0.05)
res$n


bed_rest <- c(67, 68, 70, 66, 68, 62, 71, 65, 67, 65)
high_activity <- c(63, 62, 69, 64, 67, 60, 66, 65, 63, 62)

t.test(bed_rest, high_activity, paired = TRUE)

x1 <- 12.5
x2 <- 13.2

var1 <- 4.41
var2 <- 15.21

n1 <- 20
n2 <- 50

t <- (x1 - x2) / sqrt(var1 / n1 + var2 / n2)
t

df <- n1 + n2 - 2
df

pt(t, df)
