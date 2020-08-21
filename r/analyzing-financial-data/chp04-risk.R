## ---- 4.1 Risk-Return Trade-Off
## --- Step 1: Import Fama-French Data from Professor Kenneth French’s Website
library(quantmod)
library(xts)

ff_raw <- read.fwf(
  file = "./data/F-F_Research_Data_Factors.txt",
  widths = c(6, 8, 8, 8, 8),
  skip = 4
)


head(ff_raw)
tail(ff_raw)

## --- Step 2: Clean up Data

ff_raw <- ff_raw[-1051:-1226, ]
names(ff_raw) <- paste(c("text.date", "RmxRf", "SMB", "HML", "Rf"))
head(ff_raw)
tail(ff_raw)

str(ff_raw)

ff_raw <- ff_raw[, c(-1, -3, -4)]

ff_raw$RmxRf <- as.numeric(ff_raw$RmxRf) / 100
ff_raw$Rf <- as.numeric(ff_raw$Rf) / 100

ff_raw$date <- seq(as.Date("1926-07-01"), as.Date("2013-12-31"), by = "months")

ff_raw$date <- as.yearmon(ff_raw$date, "%Y-%m-%d")
ff_raw[c(1:3, nrow(ff_raw)), ]

str(ff_raw)

## --- Step 3: Calculate Raw Market Return Variable
ff_raw$Rm <- ff_raw$RmxRf + ff_raw$Rf
ff_raw[c(1:3, nrow(ff_raw)), ]

## --- Step 4: Subset Data from December 1963 to December 2013

ff <- subset(ff_raw, ff_raw$date >= "1963-12-01" & ff_raw$date <= "2013-12-31")
ff[c(1:3, nrow(ff)), ]

## --- Step 5: Calculate Gross Returns for the Market and Risk-free Rate

ff$Gross.Rm <- 1 + ff$Rm
ff[1, "Gross.Rm"] <- 1
ff$GrossRf <- 1 + ff$Rf
ff[1, "GrossRf"] <- 1
ff[c(1:3, nrow(ff)), ]

## --- Step 6: Calculate Cumulative Returns for the Market and Risk-free Raet

ff$cum.Rm <- cumprod(ff$Gross.Rm)
ff$cum.Rf <- cumprod(ff$GrossRf)
ff[c(1:3, nrow(ff)), ]
p
## --- Step 7: Plot the Data
y_range <- range(ff$cum.Rm, ff$cum.Rf)
y_range

png(file = "./images/chp04-plot1.png", width = 1000)
title1 <- "Stock vs. Bond Returns"
title2 <- "1964 to 2013"
plot(
  x = ff$date,
  y = ff$cum.Rm,
  type = "l",
  xlab = "Date",
  ylab = "Value of $1 Investment ($)",
  ylim = y_range,
  main = paste(title1, "\n", title2)
)
lines(x = ff$date, y = ff$cum.Rf, lty = 2)
legend(
  "topleft",
  c("Stocks (2013 Ending Value: $124.89)", "Bonds (2013 Ending Value: $12.10)"),
  lty = c(1, 2)
)
dev.off()

## --- Step 8: Plot Stock and Bond Returns
y_range <- range(ff$Rm, ff$Rf)
y_range

png(file = "./images/chp04-plot2.png", width = 1000)

title1 <- "Volatility of Stock vs. Bond Returns"
title2 <- "1964 to 2013"
plot(
  x = ff$date,
  y = ff[, "Rm"],
  type = "l",
  xlab = "Date",
  ylab = "Returns (%)",
  ylim = y_range,
  col = "gray50",
  main = paste(title1, "\n", title2)
)
lines(x = ff$date, y = ff$Rf)
abline(h = 0)
legend("topleft", c("Stocks", "Bonds"), lty = c(1, 2))

dev.off()

## ---- 4.2 Individual Security Risk

## --- Step 1: Import AMZN Data from Yahoo Finance
data_amzn <- readRDS("./data/amzn.rds")

## --- Step 2: Calculate Returns
amzn_ret <- data_amzn$AMZN.Adjusted
amzn_ret$Return <- Delt(amzn_ret$AMZN.Adjusted)
amzn_ret <- amzn_ret[-1, 2]
amzn_ret[c(1:3, nrow(amzn_ret))]

## --- Step 3: Calculate Full Period (2011–2013) Variance and Standard Deviation
amzn_var_full <- var(amzn_ret$Return)
amzn_var_full
amzn_sd_full <- sd(amzn_ret$Return)
amzn_sd_full

## --- Step 4: Calculate Variance and Standard Deviation for 2011
amzn_2011 <- subset(amzn_ret,
                    index(amzn_ret) >= "2011-01-01" &

                    index(amzn_ret) <= "2011-12-31")
amzn_2011[c(1:3, nrow(amzn_2011)), ]

amzn_var_2011 <- var(amzn_2011)
amzn_var_2011

amzn_sd_2011 <- sd(amzn_2011)
amzn_sd_2011

## --- Step 5: Calculate Variance and Standard Deviation for 2012 and 2013
amzn_2012 <- subset(amzn_ret,
                    index(amzn_ret) >= "2012-01-01" &
                    index(amzn_ret) <= "2012-12-31")
amzn_2012[c(1:3, nrow(amzn_2012)), ]

amzn_var_2012 <- var(amzn_2012)
amzn_var_2012

amzn_sd_2012 <- sd(amzn_2012)
amzn_sd_2012

amzn_2013 <- subset(amzn_ret,
                    index(amzn_ret) >= "2013-01-01" &
                    index(amzn_ret) <= "2013-12-31")
amzn_2013[c(1:3, nrow(amzn_2013)), ]

amzn_var_2013 <- var(amzn_2013)
amzn_var_2013

amzn_sd_2013 <- sd(amzn_2013)
amzn_sd_2013

## --- Step 6: Calculate Average Return for the Full Period
## --- and Each of the Subperiods

mean_ret_full <- mean(amzn_ret)
mean_ret_full

mean_ret_2011 <- mean(amzn_2011)
mean_ret_2011

mean_ret_2012 <- mean(amzn_2012)
mean_ret_2012

mean_ret_2013 <- mean(amzn_2013)
mean_ret_2013

## --- Step 7: Combine All Data

amzn_risk <- rbind(
  cbind(amzn_var_full, amzn_var_2011, amzn_var_2012, amzn_var_2013),
  cbind(amzn_sd_full, amzn_sd_2011, amzn_sd_2012, amzn_sd_2013),
  cbind(mean_ret_full, mean_ret_2011, mean_ret_2012, mean_ret_2013)
)
amzn_risk

## --- Step 8: Cleanup Data

options(digits = 3)
rownames(amzn_risk) <- c("Variance", "Std Dev", "Mean")
colnames(amzn_risk) <- c("2011-2013", "2011", "2012", "2013")
amzn_risk
options(digits = 7)

options(digits = 3)
annual_vol <- amzn_risk
annual_vol[1, ] <- annual_vol[1, ] * 252
annual_vol[2, ] <- annual_vol[2, ] * sqrt(252)
annual_vol[3, ] <- annual_vol[3, ] * 252
annual_vol
options(digits = 7)

## ---- 4.3 Portfolio Risk
## --- 4.3.1 Two Assets (Manual Approach)

## --- Step 1: Calculate Weights of Securities in the Portfolio

wgt_amzn <- .25
wgt_ibm <- .75

## --- Step 2: Import AMZN and IBM Data from Yahoo Finance and Calculate Total Returns
data_amzn <- readRDS("./data/amzn.rds")
data_amzn[c(1:3, nrow(data_amzn)), ]

amzn_ret <- Delt(data_amzn$AMZN.Adjusted)
amzn_ret[c(1:3, nrow(amzn_ret)), ]

data_ibm <- readRDS("./data/ibm.rds")
ibm_ret <- Delt(data_ibm$IBM.Adjusted)
ibm_ret[c(1:3, nrow(ibm_ret)), ]

## --- Step 3: Combine the Two Return Series

returns <- cbind(amzn_ret, ibm_ret)
returns[c(1:3, nrow(returns)), ]

names(returns) <- paste(c("amzn_ret", "ibm_ret"))
returns[c(1:3, nrow(returns)), ]
returns <- returns[-1, ]
returns[c(1:3, nrow(returns)), ]

## --- Step 4: Calculate Standard Deviation and Covariance of the Securities

sd_amzn <- sd(returns$amzn_ret) * sqrt(252)
sd_amzn
sd_ibm <- sd(returns$ibm_ret) * sqrt(252)
sd_ibm
ret_cov <- cov(returns$amzn_ret, returns$ibm_ret) * 252
ret_cov

ret_correl <- cor(returns$amzn_ret, returns$ibm_ret)
ret_correl
ret_correl * sd_amzn * sd_ibm

## --- Step 5: Calculate Portfolio Risk

port_var <- wgt_amzn^2 * sd_amzn^2 + wgt_ibm^2 * sd_ibm^2 + 2 * ret_cov * wgt_amzn * wgt_ibm
port_var

port_sd <- sqrt(port_var)
port_sd
wgtd_sd <- wgt_amzn * sd_amzn + wgt_ibm * sd_ibm
wgtd_sd

## --- 4.3.2 Two Assets (Matrix Algebra)

## -- Step 1: Create Vector of Weights

wgt_2asset <- c(0.25, 0.75)
wgt_2asset
wgt_2asset <- matrix(wgt_2asset, 1)
wgt_2asset

## -- Step 2: Create Transposed Vector of Weights
twgt_2asset <- t(wgt_2asset)
twgt_2asset

## -- Step 3: Construct Variance–Covariance Matrix
mat_ret <- as.matrix(returns)
head(mat_ret)

options(scipen <- "100")
cov(mat_ret)
vcov_2asset <- cov(mat_ret) * 252
vcov_2asset

## -- Step 4: Calculate Portfolio Risk

mat_var2asset <- wgt_2asset %*% vcov_2asset %*% twgt_2asset
mat_var2asset

mat_sd2asset <- sqrt(mat_var2asset)
mat_sd2asset

## --- 4.3.3 Multiple Assets

## -- Step 1: Import Data for AMZN, IBM, YHOO, and TSLA

data_amzn <- readRDS("./data/amzn.rds")
data_ibm <- readRDS("./data/ibm.rds")
data_tsla <- readRDS("./data/tsla.rds")
data_gspc <- readRDS("./data/gspc.rds")

data_amzn[c(1:3, nrow(data_amzn)), ]
data_ibm[c(1:3, nrow(data_ibm)), ]
data_tsla[c(1:3, nrow(data_tsla)), ]
data_gspc[c(1:3, nrow(data_gspc)), ]

## -- Step 2: Extract Adjusted Prices of Each Security
multi <- data_amzn[, "AMZN.Adjusted"]
multi <- merge(multi, data_ibm[, "IBM.Adjusted"])
multi <- merge(multi, data_tsla[, "TSLA.Adjusted"])
multi <- merge(multi, data_gspc[, "GSPC.Adjusted"])
multi[c(1:3,nrow(multi)),]

## -- Step 3: Calculate Returns for Each Security
mat.price <- matrix(multi, nrow(multi))

prc2ret <- function(x) Delt(x)

mat.ret <- apply(mat.price, 2, function(x) {
  Delt(c(x))
})

mat.ret[1:4, ]

## -- Step 4: Clean up Returns Data
mat.ret <- mat.ret[-1, ]
mat.ret[1:4, ]

colnames(mat.ret) <- c("AMZN", "IBM", "TSLA", "GSPC")
mat.ret[1:4, ]

## -- Step 5: Calculate Annualized Variance–Covariance Matrix

VCOV <- cov(mat.ret)
VCOV

VCOV.annual <- 252 * VCOV
VCOV.annual

## -- Step 6: Create a Row vector of Weights
wgt = c(.2, .2, .3, .3)

mat_wgt <- matrix(wgt, 1)
mat_wgt

## -- Step 7: Create a Column Vector of Weights by Transposing the Row Vector of Weights

tmat_wgt <- t(mat_wgt)
tmat_wgt

## -- Step 8: Calculate the Portfolio Variance
port.var <- mat_wgt %*% VCOV.annual %*% tmat_wgt
port.var[1, 1]

## -- Step 9: Calculate the Portfolio Standard Deviation
port.sd <- sqrt(port.var)
port.sd[1, 1]

## ---- 4.4 Value-at-Risk

## --- 4.4.1 Gaussian VaR

## -- Step 1: Import Daily Portfolio Returns for the Last Year

port.ret <- read.csv("./data/HypotheticalPortfolioDaily.csv")
port.vw_cum <- port.ret$vw_cum[nrow(port.ret)]
port.vw_cum * 1000000
port.ret[c(1:3, nrow(port.ret)), ]
port.ret <- port.ret$vw_ret[-1]
port.ret[1:5]

## -- Step 2: Calculate Mean and Standard Deviation of Historical Daily Portfolio Returns

port.mean <- mean(port.ret)
port.mean
port.risk <- sd(port.ret)
port.risk

## -- Step 3: Calculate 1 and 5 % VaR
VaR01.Gaussian <- -(port.mean + port.risk * qnorm(0.01)) * port.vw_cum * 1000000
VaR01.Gaussian <- format(VaR01.Gaussian, big.mark = ',')
VaR01.Gaussian

VaR05.Gaussian <- -(port.mean + port.risk * qnorm(0.05)) * port.vw_cum * 1000000
VaR05.Gaussian <- format(VaR05.Gaussian, big.mark = ",")
VaR05.Gaussian

## --- 4.4.2 Historical VaR
## -- Step 1: Import Three Years of Daily Returns Data for Each Security in the Portfolio

data_amzn[c(1:3, nrow(data_amzn)), ]

AMZN.Ret <- Delt(data_amzn$AMZN.Adjusted)
AMZN.Ret[c(1:3, nrow(AMZN.Ret)), ]

data_ibm[c(1:3, nrow(data_ibm)), ]
IBM.Ret <- Delt(data_ibm$IBM.Adjusted)
IBM.Ret[c(1:3, nrow(IBM.Ret)), ]

data_gspc[c(1:3, nrow(data_gspc)), ]

GSPC.Ret <- Delt(data_gspc$GSPC.Adjusted)
GSPC.Ret[c(1:3, nrow(GSPC.Ret)), ]

## -- Step 2: Combine Returns Data into One Data Object
ret.data <- cbind(AMZN.Ret[-1, ], IBM.Ret[-1, ], GSPC.Ret[-1, ])
ret.data[c(1:3, nrow(ret.data)), ]

names(ret.data) <- paste(c("AMZN.Ret", "GSPC.Ret", "IBM.Ret"))
ret.data[c(1:3, nrow(ret.data)), ]

## -- Step 3: Identify the Value of Each Security in the Portfolio as of December 31, 2013
last.idx <- c(0.5370, 0.1205, 0.6022) * 1000000
last.idx

port.val <- sum(last.idx)
port.val

## -- Step 4: Calculate Simulated Portfolio Returns Applying Current Portfolio Weights to Historical Security Returns
sim.portPnL <-
  last.idx[1] * ret.data$AMZN.Ret +
  last.idx[2] * ret.data$GSPC.Ret +
  last.idx[3] * ret.data$IBM.Ret
sim.portPnL[c(1:3, nrow(sim.portPnL)), ]

names(sim.portPnL) <- paste("Port.PnL")
sim.portPnL[c(1:3, nrow(sim.portPnL)), ]

## -- Step 5: Calculate Appropriate Quantile for the 1 and 5 % VaR
VaR01.Historical <- quantile(-sim.portPnL$Port.PnL, 0.99)
VaR01.Historical<-format(VaR01.Historical, big.mark = ',')
VaR01.Historical

VaR05.Historical <- quantile(-sim.portPnL$Port.PnL, 0.95)
VaR05.Historical <- format(VaR05.Historical, big.mark = ",")
VaR05.Historical

## -- Step 6: Plot the VaR in Relation to P&L Density
ret.d <- density(sim.portPnL$Port.PnL)
ret.d

png(file = "./images/chp04-plot3.png")
plot(ret.d,
  xlab = "Profit & Loss",
  ylab = "",
  yaxt = "n",
  main = paste("Density of Simulated Portfolio P&L Over Three Years",
               "\n",
               "And 1% and 5% 1-Day Historical Value-at-Risk (VaR)")
  )
abline(
  v = -quantile(-sim.portPnL$Port.PnL, 0.99),
  col = "gray",
  lty = 1
)
abline(v = -quantile(-sim.portPnL$Port.PnL, 0.95), col = "black", lty = 2)
dev.off()

x <- seq(min(sim.portPnL$Port.PnL),
  max(sim.portPnL$Port.PnL),
  length = 1000
)
head(x)
tail(x)

y <- dnorm(x,
  mean = mean(sim.portPnL$Port.PnL),
  sd = sd(sim.portPnL$Port.PnL)
)

head(y)
tail(y)

png(file = "./images/chp04-plot4.png")
plot(ret.d,
  xlab = "Profit & Loss",
  ylab = "",
  yaxt = "n",
  main = paste(
    "Density of Simulated Portfolio P&L Over Three Years",
    "\n",
    "And 1% and 5% 1-Day Historical Value-at-Risk (VaR)"
  )
)
abline(
  v = -quantile(-sim.portPnL$Port.PnL, 0.99),
  col = "gray",
  lty = 1
)
abline(v = -quantile(-sim.portPnL$Port.PnL, 0.95), col = "black", lty = 2)
lines(x, y, type = "l", col = "black", lwd = 1, lty = 3)
legend(
  "topright",
  c("Simulated P&L Distribution", "Normal Distribution", "1% 1-Day VaR", "5% 1-Day VaR"),
  col = c("black", "black", "gray", "black"),
  lty = c(1, 3, 1, 2)
)
dev.off()

VaR01.10day <- quantile(-sim.portPnL$Port.PnL, 0.99) * sqrt(10)
VaR01.10day

## ---- 4.5 Expected Shortfall
## --- 4.5.1 Gaussian ES
ES01.Gaussian <- 1259700 * (port.mean + port.risk * (dnorm(qnorm(.01)) / .01))
ES01.Gaussian <- format(ES01.Gaussian, big.mark = ",")
ES01.Gaussian

ES05.Gaussian <- 1259700 * (port.mean + port.risk * (dnorm(qnorm(.05)) / .05))
ES05.Gaussian <- format(ES05.Gaussian, big.mark = ",")
ES05.Gaussian

## --- 4.5.2 Historical ES
## -- Step 1: Identify Historical VaR Limit for Portfolio
VaR01.hist <- -quantile(-sim.portPnL$Port.PnL, 0.99)
VaR01.hist

VaR05.hist <- -quantile(-sim.portPnL$Port.PnL, 0.95)
VaR05.hist

## -- Step 2: Identify Simulated Portfolio Losses in Excess of VaR
ES.PnL <- sim.portPnL$Port.PnL
ES.PnL[c(1:3, nrow(ES.PnL)), ]

ES.PnL$dummy01 <- ifelse(ES.PnL$Port.PnL < VaR01.hist, 1, 0)
ES.PnL$dummy05 <- ifelse(ES.PnL$Port.PnL < VaR05.hist, 1, 0)
ES.PnL[c(1:3, nrow(ES.PnL)), ]

## -- Step 3: Extract Portfolio Losses in Excess of VaR
shortfall01 <- subset(ES.PnL, ES.PnL$dummy01 == 1)
head(shortfall01)
shortfall05 <- subset(ES.PnL, ES.PnL$dummy05 == 1)
head(shortfall05)

## -- Step 4: Compute Average of Losses in Excess of VaR
avg.ES01 <- -mean(shortfall01$Port.PnL)
avg.ES01

ES01.Historical <- format(avg.ES01, big.mark = ",")
ES01.Historical
avg.ES05 <- -mean(shortfall05$Port.PnL)
avg.ES05

ES05.Historical <- format(avg.ES05, big.mark = ",")
ES05.Historical

## --- 4.5.3 Comparing VaR and ES
VaR.ES.Combined <-
  data.frame(rbind(
    cbind(VaR01.Historical, ES01.Historical[1], VaR01.Gaussian, ES01.Gaussian[1]),
    cbind(VaR05.Historical, ES05.Historical[1], VaR05.Gaussian, ES05.Gaussian[1])
  ))
VaR.ES.Combined

names(VaR.ES.Combined) <- paste(c("VaR Historical", "ES Historical", "VaR Gaussian", "ES Gaussian"))
rownames(VaR.ES.Combined) <- paste(c("1% 1-Day", "5% 1-Day"))
VaR.ES.Combined

## ---- 4.6 Alternative Risk Measures
## --- 4.6.1 Parkinson
## -- Step 1: Import Amazon Data from Yahoo Finance
data.AMZN <- readRDS("./data/amzn.rds")

## -- Step 2: Keep the High and Low Price
parkinson <- data.AMZN[-1, 2:3]
parkinson[c(1:3, nrow(parkinson)), ]

## -- Step 3: Calculate the Terms in the Parkinson Formula
parkinson$log.hi.low <- log(parkinson$AMZN.High / parkinson$AMZN.Low)
parkinson$log.square <- (parkinson$log.hi.low)**2
parkinson[c(1:3, nrow(parkinson)), ]

## -- Step 4: Calculate the Sum of the Values Under the log.square Column
parkinson.sum <- sum(parkinson$log.square)
parkinson.sum

## -- Step 5: Calculate the Daily Parkinson Volatility Measure
parkinson.vol <- sqrt(1 / (4 * nrow(parkinson) * log(2)) * parkinson.sum)
parkinson.vol

## -- Step 6: Calculate the Annualized Parkinson Volatility
annual.parkinson.vol <- parkinson.vol * sqrt(252)
annual.parkinson.vol

## --- 4.6.2 Garman-Klass
## -- Step 1: Import Amazon Open, High, Low, and Close Price Data
garman.klass <- data.AMZN[-1, 1:4]
garman.klass[c(1:3, nrow(garman.klass)), ]

## -- Step 2: Calculate the First Term
garman.klass.one <- (1 / (2 * nrow(garman.klass))) * parkinson.sum
garman.klass.one

## -- Step 3: Calculate the Second Term
garman.klass.two <- ((2 * log(2) - 1) / nrow(garman.klass)) *
  sum(log(garman.klass$AMZN.Close / garman.klass$AMZN.Open)**2)
garman.klass.two

## -- Step 4: Calculate the Daily Garman-Klass Volatility
garman.klass.vol <- sqrt(garman.klass.one - garman.klass.two)
garman.klass.vol

## -- Step 5: Annualize the Volatility
annual.garman.klass.vol <- garman.klass.vol * sqrt(252)
annual.garman.klass.vol

## --- 4.6.3 Rogers, Satchell, and Yoon
## -- Step 1: Obtain Open, High, Low, and Close Data
rsy.vol <- data.AMZN[-1, 1:4]
rsy.vol[c(1:3, nrow(rsy.vol)), ]

## -- Step 2: Calculate the Product of First Two Log Terms
rsy.one <- log(rsy.vol$AMZN.High / rsy.vol$AMZN.Close)
rsy.one[c(1:3, nrow(rsy.one)), ]
rsy.two <- log(rsy.vol$AMZN.High / rsy.vol$AMZN.Open)
rsy.two[c(1:3, nrow(rsy.two)), ]
rsy.one.two <- rsy.one * rsy.two
rsy.one.two[c(1:3, nrow(rsy.one.two)), ]

## -- Step 3: Calculate the Product of Last Two Log Terms
rsy.three <- log(rsy.vol$AMZN.Low / rsy.vol$AMZN.Close)
rsy.three[c(1:3, nrow(rsy.three)), ]
rsy.four <- log(rsy.vol$AMZN.Low / rsy.vol$AMZN.Open)
rsy.four[c(1:3, nrow(rsy.four)), ]
rsy.three.four <- rsy.three * rsy.four
rsy.three.four[c(1:3, nrow(rsy.three.four)), ]

## -- Step 4: Calculate the RSY Volatility Measure
rsy.vol <- sqrt((1 / nrow(rsy.vol)) * sum((rsy.one.two + rsy.three.four)))
rsy.vol

## -- Step 5: Annualize the RSY Volatility Measure
annual.rsy.vol <- rsy.vol * sqrt(252)
annual.rsy.vol

## --- 4.6.4 Yang and Zhang
## -- Step 1: Import Amazon Open, High, Low, and Close Data and Create Variable for Yesterday’s Closing Price
yz.vol <- data.AMZN[, 1:4]
yz.vol$Lag.Close <- Lag(yz.vol$AMZN.Close, k = 1)
yz.vol[c(1:3, nrow(yz.vol)), ]

## -- Step 2: Delete December 31, 2010 Data
yz.vol <- yz.vol[-1, ]
yz.vol[c(1:3, nrow(yz.vol)), ]

## -- Step 3: Calculate the First Term in the Yang-Zhang Equation
yz.one.mean <- mean(log(yz.vol$AMZN.Open / yz.vol$Lag.Close))
yz.one.mean

yz.one <- 1 / (nrow(yz.vol) - 1) * sum((log(yz.vol$AMZN.Open / yz.vol$Lag.Close) - yz.one.mean)**2)
yz.one

## -- Step 4: Calculate the Second Term in the Yang-Zhang Equation
yz.two.mean <- mean(log(yz.vol$AMZN.Close / yz.vol$AMZN.Open))
yz.two.mean
yz.two <- 1 / (nrow(yz.vol) - 1) * sum((log(yz.vol$AMZN.Close / yz.vol$AMZN.Open) - yz.two.mean)**2)
yz.two

## -- Step 5: Calculate k
k = 0.34 / (1.34 + (nrow(yz.vol) + 1) / (nrow(yz.vol) - 1))
k

## -- Step 6: Calculate the Annualized Yang-Zhang Volatility
annual.yz.vol <- sqrt(yz.one + k * yz.two + (1 - k) * rsy.vol^2) * sqrt(252)
annual.yz.vol

## --- 4.6.5 Comparing the Risk Measures
## -- Step 1: Calculate Amazon Returns Data

## -- Step 2: Calculate Log Returns
cl2cl.ret <- AMZN.ret[-1]
names(cl2cl.ret) <- "Return"
cl2cl.ret$logret <- log(1 + cl2cl.ret$Return)
cl2cl.ret[c(1:3, nrow(cl2cl.ret)), ]

## -- Step 3: Calculate Standard Deviation of the Returns
cl2cl.vol <- sd(cl2cl.ret$logret)
cl2cl.vol

## -- Step 4: Annualize the Close-to-Close Volatility
annual.cl2cl.vol <- cl2cl.vol * sqrt(252)
annual.cl2cl.vol

## -- Step 5: Create Table of the Different Volatility Measures
vol.measures <- rbind(annual.cl2cl.vol, annual.parkinson.vol, annual.garman.klass.vol, annual.rsy.vol, annual.yz.vol)
rownames(vol.measures) <- c("Close-to-Close", "Parkinson", "Garman-Klass", "Rogers et al", "Yang-Zhang")
colnames(vol.measures) <- c("Volatility")
vol.measures

