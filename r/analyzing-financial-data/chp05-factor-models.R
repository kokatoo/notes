## ---- Appendix B Constructing a Hypothetical Portfolio
data.AMZN <- readRDS("./data/amzn.rds")
AMZN.monthly <- to.monthly(data.AMZN)
AMZN.monthly[c(1:3, nrow(AMZN.monthly)), ]

AMZN.monthly <- AMZN.monthly[, "data.AMZN.Adjusted"]
AMZN.ret <- Delt(AMZN.monthly$data.AMZN.Adjusted)
names(AMZN.ret) <- paste("AMZN.ret")

data.IBM <- readRDS("./data/ibm.rds")

IBM.monthly <- to.monthly(data.IBM)
IBM.monthly[c(1:3, nrow(IBM.monthly)), ]

IBM.monthly <- IBM.monthly[, "data.IBM.Adjusted"]
IBM.ret <- Delt(IBM.monthly$data.IBM.Adjusted)
names(IBM.ret) <- paste("IBM.ret")

IBM.ret[c(1:3, nrow(IBM.ret)), ]

data.TSLA <- readRDS("./data/tsla.rds")

TSLA.monthly <- to.monthly(data.TSLA)
TSLA.monthly[c(1:3, nrow(TSLA.monthly)), ]

TSLA.monthly <- TSLA.monthly[, "data.TSLA.Adjusted"]
TSLA.ret <- Delt(TSLA.monthly$data.TSLA.Adjusted)

names(TSLA.ret) <- paste("TSLA.ret")
TSLA.ret[c(1:3, nrow(TSLA.ret)), ]

port <- cbind(AMZN.ret, TSLA.ret, IBM.ret)
port <- port[-1,]
port[c(1:3, nrow(port)), ]

port$port.ret <- rowMeans(port)
port[c(1:3, nrow(port)), ]

csv.port <- cbind(data.frame(index(port)), data.frame(port))
names(csv.port)[1] <- paste("date")
csv.port[c(1:3, nrow(csv.port)), ]

rownames(csv.port) <- seq(1, nrow(csv.port), by = 1)
csv.port[c(1:3, nrow(csv.port)), ]

write.csv(csv.port, "./data/HypotheticalPortfolioMonthly.csv")

## ---- 5.1 CAPM
## --- Step 1: Import Portfolio Returns and Convert to a data.frame Object
library(quantmod)
library(xts)

port <- read.csv("./data/HypotheticalPortfolioMonthly.csv")
port[c(1:3, nrow(port)), ]

class(port$date)
port$date <- as.yearmon(as.character(port$date), "%b %Y")
port[c(1:3, nrow(port)), ]

class(port$date)
port.df <- data.frame(port)
port.df[c(1:3, nrow(port.df)), ]

## --- Step 2: Import S&P 500 Index Data from Yahoo Finance and Calculate Monthly Market Returns
data.mkt <- readRDS("./data/gspc.rds")
data.mkt[c(1:3, nrow(data.mkt)), ]

mkt.monthly <- to.monthly(data.mkt)
mkt.monthly[c(1:3, nrow(mkt.monthly)), ]

mkt.monthly <- mkt.monthly[, 6]
mkt.ret <- Delt(mkt.monthly$data.mkt.Adjusted)
names(mkt.ret) <- paste("mkt.ret")
mkt.ret[c(1:3, nrow(mkt.ret)), ]

mkt.ret <- mkt.ret[-1, ]
mkt.ret[c(1:3, nrow(mkt.ret)), ]

market.df <- data.frame(mkt.ret)
head(market.df)

## --- Step 3: Import Risk-Free Rate Data from FRED and Setup Data to Contain Monthly Risk-Free Returns
library(Quandl)
rf <- Quandl("FRED/DGS3MO")

saveRDS(rf, "./data/DGS3MO_FRED.rds")

rf <- readRDS("./data/DGS3MO_FRED.rds")

rf <- rf[order(rf$Date), ]
rf <- rf[rf$Date > as.Date("1982-01-03"), ]
rf[c(1:3, nrow(rf)), ]

str(rf)

## -- Step 3a: Convert to xts Object
rf <- xts(rf$Value, order.by = rf$Date)
rf[1:3, ]

names(rf) <- paste("DGS3MO")
rf[1:3, ]

## -- Step 3b: Apply to.monthly Command to Identify First Yield for Each Month
rf.monthly <- to.monthly(rf)
rf.monthly[1:3, ]

## -- Step 3c: Convert Opening Annualized Yield for Each Month Into a Monthly Yield
options(scipen = "100")
rf.monthly <- (1 + rf.monthly[, 1] / 100) ^ (1 / 12) - 1
rf.monthly[c(1:3, nrow(rf.monthly)), ]

## -- Step 3d: Subset Data to January 2011 Through December 2013
rf.sub <- subset(
  rf.monthly,
  index(rf.monthly) >= as.yearmon("Jan 2011") &
    index(rf.monthly) <= as.yearmon("Dec 2013")
)
rf.sub[c(1:3, nrow(rf.sub)), ]

## -- Step 4: Combine Firm, Market, and Risk-Free Data Into One Data Object
combo <- cbind(market.df, data.frame(rf.sub), port.df$port.ret)
combo[c(1:3, nrow(combo)), ]

names(combo) <- paste(c("mkt.ret", "rf", "port.ret"))
combo[c(1:3, nrow(combo)), ]

## -- Step 5: Calculate Excess Firm Return and Excess Market Return
combo$exret <- combo$port.ret - combo$rf
combo$exmkt <- combo$mkt.ret - combo$rf
combo[c(1:3, nrow(combo)), ]

## -- Step 6: Run Regression of Excess Firm Return on Excess Market Return
options(digits = 3)
CAPM <- lm(combo$exret ~ combo$exmkt)
summary(CAPM)

beta <- summary(CAPM)$coefficients[2]
beta
beta.pval <- summary(CAPM)$coefficients[8]
beta.pval

adj.beta <- (2 / 3) * beta + (1 / 3) * 1
adj.beta
options(digits = 7)
## ----- 5.2 Market Model
library(quantmod)
library(xts)

options(digits = 3)
reg <- lm(combo$port.ret ~ combo$mkt.ret)
summary(reg)

beta.mktmod <- summary(reg)$coefficients[2]
beta.mktmod

adj.beta.mktmod <- (2 / 3) * beta.mktmod + (1 / 3) * 1
adj.beta.mktmod
options(digits = 7)

## ---- 5.3 Rolling Window Regressions
## --- Step 1: Import Amazon.com and S&P 500 Index Data
data.AMZN <- readRDS("./data/amzn.rds")
data.mkt <- readRDS("./data/gspc.rds")

## --- Step 2: Calculate the Amazon.com and Market Returns
rets <- diff(log(data.AMZN$AMZN.Adjusted))
rets$GSPC <- diff(log(data.mkt$GSPC.Adjusted))
names(rets)[1] <- "AMZN"
rets[c(1:3, nrow(rets)), ]
rets <- rets[-1, ]
rets[c(1:3, nrow(rets)), ]

## --- Step 3: Create the Rolling Window Regression Function
require(zoo)
coeffs <- rollapply(
  rets,
  width = 252,
  FUN = function(X) {
    roll.reg <- lm(
      AMZN ~ GSPC,
      data = as.data.frame(X)
    )

    return(roll.reg$coef)
  }, by.column = FALSE
)
coeffs[c(1, 251:253, nrow(coeffs)), ]

## --- Step 4: Remove NAs From the Data
coeffs <- na.omit(coeffs)
coeffs[c(1:3, nrow(coeffs)), ]

## --- Step 5: Clean-Up Data
coeffs <- coeffs[-1, ]
names(coeffs) <- c("Alpha", "Beta")
options(digits = 3)
coeffs[c(1:3, nrow(coeffs)), ]

png(file = "./images/chp05-plot1.png")
par(oma = c(0, 0, 4, 0))
par(mfrow = c(2, 1))
plot(
  x = index(coeffs),
  xlab = "Date",
  y = coeffs$Alpha,
  ylab = "alpha",
  type = "l"
)
plot(
  x = index(coeffs),
  xlab = "Date",
  y = coeffs$Beta,
  ylab = "beta",
  type = "l"
)
title(
  main = paste(
    "Amazon.com Inc. Alpha and Beta Using Rolling 252-Day Windowns",
    "\n",
    "and Daily Returns From 2012 to 2013"
  ),
  outer = TRUE
)
dev.off()
par(mfrow = c(1, 1))

## ---- 5.4 Fama-French Three Factor Model
## --- Step 1: Import Portfolio Returns Data
port[c(1:3, nrow(port)), ]

## --- Step 2: Import Fama-French Data Retrieved From Ken French’s Website
FF.raw <- read.fwf(
  file = "./data/F-F_Research_Data_Factors.txt",
  widths = c(6, 8, 8, 8, 8),
  skip = 4
)
head(FF.raw)
tail(FF.raw)

FF.raw <- FF.raw[-1051:-1226, ]
names(FF.raw) <- paste(c("text.date", "RmxRf", "SMB", "HML", "Rf"))
head(FF.raw)
tail(FF.raw)

FF.raw <- FF.raw[, -1]
FF.raw$RmxRf <- as.numeric(as.character(FF.raw$RmxRf)) / 100
FF.raw$Rf <- as.numeric(as.character(FF.raw$Rf)) / 100
FF.raw$SMB <- as.numeric(as.character(FF.raw$SMB)) / 100
FF.raw$HML <- as.numeric(as.character(FF.raw$HML)) / 100
FF.raw$FF.date <- seq(
  as.Date("1926-07-01"),
  as.Date("2013-12-31"),
  by = "months"
)
FF.raw$FF.date <- as.yearmon(FF.raw$FF.date, "%Y-%m-%d")
FF.raw[c(1:3, nrow(FF.raw)), ]

## --- Step 3: Subset Fama-French Data to Relevant Time Period
FF.data <- subset(
  FF.raw,
  FF.raw$FF.date >= "2011-01-01" &
    FF.raw$FF.date <= "2013-12-31"
)
FF.data[c(1:3, nrow(FF.data)), ]

## --- Step 4: Combine Portfolio Returns Data and Fama-French Data
options(digits = 3)
FF.data <- cbind(FF.data, data.frame(port))
FF.data[c(1:3, nrow(FF.data)), ]
rownames(FF.data) <- seq(1, nrow(FF.data))
FF.data$date <- format(FF.data$date, "%Y-%m")
FF.data$exret <- FF.data$port.ret - FF.data$Rf
FF.data[c(1:3, nrow(FF.data)), ]

## --- Step 5: Run Regression Using Fama-French Factors
FF.reg <- lm(FF.data$exret ~ RmxRf + SMB + HML, data = FF.data)
summary(FF.reg)

## -- Compare Fama-French Results with CAPM Results
CAPM.reg <- lm(exret ~ RmxRf, data = FF.data)
summary(CAPM.reg)

betas <- rbind(
  cbind(
    summary(FF.reg)$coefficient[2],
    summary(FF.reg)$coefficient[14],
    summary(FF.reg)$adj.r.squared
  ),
  cbind(
    summary(CAPM.reg)$coefficient[2],
    summary(CAPM.reg)$coefficient[8],
    summary(CAPM.reg)$adj.r.squared
  )
)
betas

colnames(betas) <- paste(c("Beta", "p-Value", "Adj. R-Squared"))
rownames(betas) <- paste(c("Fama-French", "CAPM"))
betas
options(digits = 7)

## ---- 5.5 Event Studies
## --- 5.5.1 Example: Netflix July 2013 Earnings Announcement
## -- Step 1: Identify the Event and the Event Window

## -- Step 2: Identify the Estimation Period
## -- Step 3: Import Netflix Data From Yahoo Finance From July 20, 2012 to July 23, 2013
getSymbols.yahoo(
  Symbols = c("NFLX", "SPY"),
  env = ".GlobalEnv",
  from = "2012-07-01",
  to = "2013-07-24"
)

data.NFLX <- NFLX
saveRDS(data.NFLX, file = "./data/nflx.rds")

data.SPY <- SPY
saveRDS(data.NFLX, file = "./data/spy.rds")

library(xts)
firm <- data.NFLX
firm[c(1:3, nrow(firm)), ]

names(firm) <- paste(
  c(
    "Firm.Open",
    "Firm.High",
    "Firm.Low",
    "Firm.Close",
    "Firm.Volume",
    "Firm.Adjusted"
  )
)
firm[c(1:3, nrow(firm)), ]

## -- Step 4: Import SPDR S&P 500 Index ETF Data From Yahoo Finance From July 22, 2012 to July 23, 2013
market <- data.SPY
names(market) <- paste(
  c(
    "Mkt.Open",
    "Mkt.High",
    "Mkt.Low",
    "Mkt.Close",
    "Mkt.Volume",
    "Mkt.Adjusted"
  )
)

market[c(1:3, nrow(market)), ]

## -- Step 5: Combine the Two Data Objects
data.all <- merge(firm[, 6], market[, 6])
data.all[c(1:3, nrow(data.all)), ]

## -- Step 6: Calculate NFLX and SPY Returns
library(quantmod)
data.all$Firm.Ret <- diff(log(data.all$Firm.Adjusted)) * 100
data.all$Mkt.Ret <- diff(log(data.all$Mkt.Adjusted)) * 100
data.all[c(1:3, nrow(data.all)), ]

## -- Step 7: Perform Market Model Regression During the Estimation Period
est.per <- data.all[c(-1, -nrow(data.all)), 3:4]
est.per[c(1:3, nrow(est.per)), ]

mkt.model <- lm(est.per$Firm.Ret ~ est.per$Mkt.Ret)
summary(mkt.model)

## -- Step 8: Calculate Abnormal Return, t-Statistic, and p-Value for Dates in Event Window
event.window <- data.all[nrow(data.all), 3:4]
event.window

event.window$Pred.Ret <-
  summary(mkt.model)$coefficients[1] +
  summary(mkt.model)$coefficients[2] *
                   event.window$Mkt.Ret

event.window$Ab.Ret <- event.window$Firm.Ret - event.window$Pred.Ret
event.window$tStat <- event.window$Ab.Ret / summary(mkt.model)$sigma
event.window$pval <-
  2 * (1 - pt(abs(event.window$tStat), df = nrow(est.per) - 2))
options(digits = 3)
event.window

png(file = "./images/chp05-plot2.png", width = 1000)
title1 <- "Netflix Stock Price"
title2 <- "July 20, 2012 to July 23, 2013"
plot(data.all$Firm.Adjusted,
  auto.grid = FALSE,
  xlab = "Date",
  ylab = "Price ($)",
  main = paste(title1, "\n", title2)
)
dev.off()

## -- Step 9: Identify the Date of the Jump in January 2013
subset(est.per,
       index(est.per) >= "2013-01-01" &
       index(est.per) <= "2013-01-31")

## -- Step 10: Construct Series for Estimation Period Beginning January 28, 2013
est.per2 <- subset(est.per, index(est.per) >= "2013-01-28")
est.per2[c(1, 3, nrow(est.per2)), ]

## -- Step 11: Calculate the Market Model Parameters of this Alternative Estimation Period Using the lm command,
## -- we run an OLS regression of the Netflix returns on
mkt.model2 <- lm(est.per2$Firm.Ret ~ est.per2$Mkt.Ret)
summary(mkt.model2)

## -- Step 12: Calculate Abnormal Return and Statistical Significance of the Event Date
event.window2 <- data.all[nrow(data.all), 3:4]
event.window2
event.window2$Pred.Ret <-
  summary(mkt.model2)$coefficients[1] +
  summary(mkt.model2)$coefficients[2] *
    event.window2$Mkt.Ret

event.window2$Ab.Ret <- event.window2$Firm.Ret - event.window2$Pred.Ret
event.window2$tStat <- event.window2$Ab.Ret / summary(mkt.model2)$sigma
event.window2$pval <- 2 * (1 - pt(abs(event.window2$tStat), df = nrow(est.per2) - 2))

options(digits = 3)
event.window2
options(digits = 7)
