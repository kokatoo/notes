## Prob of drawing a spade
1 / 4

## Prob of drawing 5 spades
0.25^5

scores <- c(95, 115, 80, 130)
(scores - 100) / 10

pnorm(50, mean = 60, sd = 5)
pnorm((50 - 60) / 5)

data <- c(3, 3, 4, 4, 4, 5, 5, 5, 5, 5,
          5, 6, 6, 6, 7, 7)

mean(data)
sd(data)
sd(data) / sqrt(length(data) - 1)

6 / sqrt(30)
6 / sqrt(50)

7.8 + qt(0.025, 60) * 2.5 / sqrt(60 - 1)
7.8 + qt(0.975, 60) * 2.5 / sqrt(60 - 1)

7.8 + qt(0.005, 60) * 2.5 / sqrt(60 - 1)
7.8 + qt(0.995, 60) * 2.5 / sqrt(60 - 1)

(57 - 55) / (8 / sqrt(50))
qt(0.975, 50 - 1)

qt(0.95, 50 - 1)
