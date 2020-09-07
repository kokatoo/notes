mytable <- as.table(
  matrix(c(20, 80, 45, 55, 25, 75),
    nrow = 2,
    dimnames = list(c("flu-shot", "no-flu-shot"),
                    c("GroupA", "GroupB", "GroupC"))
  )
)

fit <- chisq.test(mytable)
fit
df <- (nrow(mytable) - 1) * (ncol(mytable) - 1)
freq_ratios <- (fit$observed - fit$expected)^2 / fit$expected
freq_ratios
sum(freq_ratios)
qchisq(0.95, df)

1 - pchisq(3.72, 1)
1 - pchisq(9.59, 4)
1 - pchisq(10.67, 3)
1 - pchisq(9.88, 1)
