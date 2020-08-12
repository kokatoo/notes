## ---- The Least squares Line
ohio_temps <- c(31, 20, 65, 38, 36, 29, 53, 57, 35, 39)
nc_temps <- c(50, 51, 79, 54, 57, 46, 70, 66, 55, 65)

fit <- lm(nc_temps ~ ohio_temps)
fit$coef
coefficients(fit)

## ---- Activity 8-3: Air Fares (cont.)
mydata <- data.frame(
  mean = c(166.92, 712.67),
  sd = c(59.45, 402.69),
  row.names = c("air.fare", "distance")
)
r <- 0.795

b <- r * mydata["air.fare", "sd"] / mydata["distance", "sd"]
b

intercept <- mydata["air.fare", "mean"] - b * mydata["distance", "mean"]
intercept

dist <- 300
b * dist + intercept

dist <- 1500
b * dist + intercept

## ---- Activity 8-5: Air Fares (cont.)
mydata <- "
destination;distance;air.fare
Atlanta;576;178
Boston;370;138
Chicago;612;94
Dallas/Fort Worth;1216;278
Detroit;409;158
Denver;1502;258
Miami;946;198
New Orleans;998;188
New York;189;98
Orlando;787;179
Pittsburgh;210;138
St. Louis;737;98
"
mydata <- read.table(header = TRUE, text = mydata, sep = ";")

myfile <- "./data/chp07-fare.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp07-fare.rds"
mydata <- readRDS(myfile)

fit <- lm(air.fare ~ distance, data = mydata)
summary(fit)

mydata <- transform(mydata, fitted = fitted(fit), resid = residuals(fit))
mydata[which.max(mydata$resid), ]
mydata[which.min(mydata$resid), ]

air_fare_sd <- 59.45

r <- sd(mydata$fit) / air_fare_sd
r
r2 <- r^2
r2

## ---- Activity 8-10: Basketball Rookie Salaries
mydata <- "
pick salary
1 3333333
2 2900000
3 2867100
4 2750000
5 2458333
6 1736250
7 1590000
8 1500000
9 1400000
10 1010652
11 997120
12 1370000
13 817000
14 675000
16 1120000
17 1120000
18 875000
19 828750
20 740000
21 775000
22 180000
23 550000
24 610000
26 180000
27 605000
"
mydata <- read.table(header = TRUE, text = mydata)

myfile <- "./data/chp07-nba.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp07-nba.rds"
mydata <- readRDS(myfile)

png(file = "./images/chp08-plot1.png")
plot(mydata$pick, mydata$salary,
  xlab = "Pick #", ylab = "Salary"
  )
lines(mydata$pick, fitted(fit))
dev.off()

fit <- lm(salary ~ pick, data = mydata)
attributes(summary(fit))
summary(fit)$r.squared

predict(fit, data.frame(pick = 12))
predict(fit, data.frame(pick = 15))

coefficients(fit)[2]
