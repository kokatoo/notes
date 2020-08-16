## ---- 1.1 Importing Daily Stock Price Data

library(quantmod)

getSymbols.yahoo(Symbols = "AMZN",
                 env = ".GlobalEnv",
                 from = "2010-12-31",
                 to = "2013-12-31")

data_amzn <- AMZN
names(data_amzn)

## ---- 1.3 Checking the Data
## --- 1.3.1 Plotting the Data

png(file = "./images/chp01-plot1.png", width = 1000)
plot(data_amzn$AMZN.Close)
dev.off()

data_missing <- data_amzn[-400:-500, ]
png(file = "./images/chp01-plot2.png", width = 1000)
plot(data_missing$AMZN.Close)
dev.off()

## --- 1.3.2 Checking the Dimension
dim(data_amzn)

#--- 1.3.3 Outputting Summary Statistics
summary(data_amzn)

## ---- 1.4 Basic Data Manipulation Techniques
## --- 1.4.1 Keeping and Deleting One Row
amzn_only_first <- data_amzn[1, ]
head(amzn_only_first)

amzn_del_first <- data_amzn[-1, ]
head(amzn_del_first)

## --- 1.4.2 Keeping First and Last Rows
data_amzn[c(1, nrow(data_amzn)), ]

## --- 1.4.3 Keeping Contiguous Rows
amzn_first_week <- data_amzn[2:6, ]
amzn_first_week

amzn_last30 <- data_amzn[((nrow(data_amzn) - 29)):nrow(data_amzn), ]
amzn_last30

## --- 1.4.4 Keeping First Three Rows and Last Row
data_amzn[c(1:3, nrow(data_amzn)), ]

## --- 1.4.5 Keeping and Deleting One Column
names(data_amzn)

amzn_only_price <- data_amzn[, 4]
amzn_only_price[c(1:3, nrow(amzn_only_price)), ]

amzn_only_price2 <- data_amzn$AMZN.Close
amzn_only_price2[c(1:3, nrow(amzn_only_price2)), ]

amzn_del_adj_price <- data_amzn[, -6]
amzn_del_adj_price[c(1:3, nrow(amzn_del_adj_price)), ]

## ---- 1.4.6 Keeping Non-Contiguous Columns
amzn_open_close <- data_amzn[, c(1, 4)]
amzn_open_close[c(1:3, nrow(amzn_open_close)), ]

## --- 1.4.7 Keeping Contiguous Columns
amzn_price_vol <- data_amzn[, 4:5]
amzn_price_vol[c(1:3, nrow(amzn_price_vol)), ]

## --- 1.4.8 Keeping Contiguous and Non-Contiguous Columns
amzn_open_close_vol <- data_amzn[, c(1, 4:5)]
amzn_open_close_vol[c(1:3, nrow(amzn_open_close_vol)), ]

amzn_open_close_vol <- data_amzn[, c(-2:-3, -6)]
amzn_open_close_vol[c(1:3, nrow(amzn_open_close_vol)), ]

## --- 1.4.9 Subsetting Rows and Columns
data_vwap <- data_amzn[((nrow(data_amzn) - 29)):nrow(data_amzn), c(4, 5)]
data_vwap[c(1:3, nrow(data_vwap)), ]

## ---1.4.10 Subsetting Using Dates
class(data_amzn)
xts_2012 <- subset(data_amzn[, 4],
                   index(data_amzn) >= "2012-01-01" &
                   index(data_amzn) <= "2012-12-31")
xts_2012[c(1:3, nrow(xts_2012))]

amzn_2012 <- cbind(index(data_amzn), data.frame(data_amzn[, 4]))
amzn_2012[c(1:3, nrow(amzn_2012)), ]
class(amzn_2012)

names(amzn_2012)[1] <- paste("date")
rownames(amzn_2012) <- seq(1, nrow(amzn_2012))
amzn_2012[c(1:3, nrow(amzn_2012)), ]
amzn_2012 <- subset(amzn_2012, amzn_2012$date >= "2012-01-01" &
                               amzn_2012$date <= "2012-12-31")
amzn_2012[c(1:3, nrow(amzn_2012)), ]

## --- 1.4.11 Converting Daily Prices to Weekly and Monthly Prices
wk <- data_amzn
data_weekly <- to.weekly(wk)
data_weekly[c(1:3, nrow(data_weekly)), ]

data_amzn[2:6, ]
sum(data_amzn[2:6, "AMZN.Volume"])

mo <- data_amzn
data_monthly <- to.monthly(mo)
data_monthly[c(1:3, nrow(data_monthly)), ]

# Plotting a Candlestick Chart Using Monthly Data
library(quantmod)

ohlc <- data_monthly[-1, -6]
amzn_ohlc <- as.quantmod.OHLC(ohlc,
  col.names = c("Open", "High", "Low", "Close", "Volume")
)
class(amzn_ohlc)
amzn_ohlc[c(1:3, nrow(amzn_ohlc)), ]
png(file = "./images/chp01-plot3.png", width = 1000)
chartSeries(amzn_ohlc, theme = chartTheme("black"), name = "amzn OHLC")
dev.off()

## ---- 1.5 Comparing Capital Gains of Multiple Securities Over Time
ls()
rm(list = ls())
ls()

data_amzn <- read.csv("amzn Yahoo.csv", header = TRUE)

date <- as.Date(data_amzn$Date, format = "%Y-%m-%d")

data_amzn <- cbind(date, data_amzn[, -1])

data_amzn <- data_amzn[order(data_amzn$date), ]

data_amzn <- xts(data_amzn[, 2:7], order.by = data_amzn[, 1])

names(data_amzn) <- paste(c("amzn.Open", "amzn.High", "amzn.Low",
                            "amzn_close", "amzn.Volume", "amzn.Adjusted"))

data_amzn[c(1:3, nrow(data_amzn)), ]

data_yhoo <- read.csv("YHOO Yahoo.csv", header = TRUE)

date <- as.Date(data_yhoo$Date, format = "%Y-%m-%d")

data_ibm <- read.csv("IBM Yahoo.csv", header = TRUE)
date <- as.Date(data_ibm$Date, format = "%Y-%m-%d")
data_ibm <- cbind(date, data_ibm[, -1])
data_ibm <- data_ibm[order(data_ibm$date), ]
data_ibm <- xts(data_ibm[, 2:7], order.by = data_ibm[, 1])
names(data_ibm) <- paste(c("IBM.Open", "IBM.High", "IBM.Low",
                           "IBM.Close", "IBM.Volume", "IBM.Adjusted"))

data_ibm[c(1:3, nrow(data_ibm)), ]

data_gspc <- read.csv("GSPC Yahoo.csv", header = TRUE)
date <- as.Date(data_gspc$Date, format = "%Y-%m-%d")
data_gspc <- cbind(date, data_gspc[, -1])
data_gspc <- data_gspc[order(data_gspc$date), ]
data_gspc <- xts(data_gspc[, 2:7], order.by = data_gspc[, 1])
names(data_gspc) <- paste(c("GSPC.Open", "GSPC.High", "GSPC.Low",
                            "GSPC.Close", "GSPC.Volume", "GSPC.Adjusted"))
data_gspc[c(1:3, nrow(data_gspc)), ]

close_prices <- data_amzn$AMZN.Close
close_prices <- cbind(close_prices, data_gspc$GSPC.Close,
                      data_ibm$IBM.Close)
close_prices[c(1:3, nrow(close_prices)), ]

multi_df <- cbind(index(close_prices), data.frame(close_prices))
names(multi_df) <- paste(c("date", "AMZN", "GSPC", "IBM"))
rownames(multi_df) <- seq(1, nrow(multi_df), 1)
multi_df[c(1:3, nrow(multi_df)), ]

multi_df$amzn_idx <- multi_df$AMZN / multi_df$AMZN[1]
multi_df$gspc_idx <- multi_df$GSPC / multi_df$GSPC[1]
multi_df$ibm_idx <- multi_df$IBM / multi_df$IBM[1]

options(digits = 5)
multi_df[c(1:3, nrow(multi_df)), ]
options(digits = 7)

png(file = "./images/chp01-plot4.png", width = 1000)
#x11(width = 15)
plot(
  x = multi_df$date,
  y = multi_df$gspc_idx,
  type = "l",
  xlab = "Date",
  ylab = "Value of Investment ($)",
  col = "black",
  lty = 1,
  lwd = 2,
  main = paste("Value of $1 Investment in AMZN, IBM, and the \n",
               "GSPC December 31, 2010 - December 31, 2013")
  )
lines(x = multi_df$date, y = multi_df$amzn_idx, col = "black", lty = 2, lwd = 1)
lines(x = multi_df$date, y = multi_df$ibm_idx, col = "gray", lty = 2, lwd = 1)
abline(h = 1, lty = 1, col = "black")
legend("topleft",
       c("AMZN", "IBM", "S&P 500 Index"),
       col = c("black", "gray", "black"),
       lty = c(2, 2, 1),
       lwd = c(1, 1, 2))
dev.off()
#X11(width = 7)

y_range <- range(multi_df[, c("gspc_idx", "ibm_idx", "amzn_idx")])
y_range
png(file = "./images/chp01-plot5.png", width = 1000)
plot(
  x = multi_df$date,
  y = multi_df$gspc_idx,
  type = "l", xlab = "Date",
  ylim = y_range,
  ylab = "Value of Investment ($)",
  col = "black",
  lty = 1,
  lwd = 2,
  main = paste(
    "Value of $1 Investment in amzn, IBM, YHOO, and the \n",
    "S&P 500 Index December 31, 2010 - December 31, 2013"
  )
)
lines(x = multi_df$date, y = multi_df$amzn_idx, col = "black", lty = 2, lwd = 1)
lines(x = multi_df$date, y = multi_df$ibm_idx, col = "gray", lty = 2, lwd = 1)
abline(h  =  1, lty  =  1, col  =  "black")
legend("topleft",
       c("AMZN", "IBM", "S&P 500 Index"),
       col  =  c("black", "gray", "black"),
       lty  =  c(2, 2, 1),
       lwd  =  c(1, 1, 2))
dev.off()

## --- 1.5.1 Alternative Presentation of Normalized Price Chart
png(file = "./images/chp01-plot6.png", width = 1000)
par(oma  =  c(0, 0, 3, 0))
par(mfrow  =  c(2, 2))

plot(x = multi_df$date,
     xlab = "",
     y = multi_df$gspc_idx,
     ylim = y_range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Amazon Stock")
lines(x = multi_df$date, y = multi_df$ibm_idx, col = "gray")
lines(x = multi_df$date, y = multi_df$amzn_idx, col = "black", lwd = 2)
abline(h = 1)

plot(
  x = multi_df$date,
  xlab = "",
  y = multi_df$gspc_idx,
  ylim = y_range,
  ylab = "",
  type = "l",
  col = "gray",
  main = "IBM Stock"
)

lines(x = multi_df$date, y = multi_df$amzn_idx, col = "gray")
lines(x = multi_df$date, y = multi_df$ibm_idx, col = "black", lwd = 2)
abline(h = 1)

plot(
  x = multi_df$date,
  xlab = "",
  y = multi_df$ibm_idx,
  ylim = y_range,
  ylab = "",
  type = "l",
  col = "gray",
  main = "S&P 500 Index"
)
lines(x = multi_df$date, y = multi_df$amzn_idx, col = "gray")
lines(x = multi_df$date, y = multi_df$gspc_idx, col = "black", lwd = 2)
abline(h = 1)

title1 <-  "Value of $1 Invested in Amazon, IBM, and the Market"
title2 <- "December 31, 2010 - December 31, 2013"
title(main = paste(title1, "\n", title2), outer = T)
dev.off()

## ----- 1.6 Technical Analysis Examples

## --- 1.6.1 Trend: Simple Moving Average Crossover
amzn_sma <- data_amzn[, 4]
amzn_sma[c(1:3, nrow(amzn_sma)), ]

amzn_sma$sma50 <- rollmeanr(amzn_sma$AMZN.Close, k = 50)
amzn_sma$sma200 <- rollmeanr(amzn_sma$AMZN.Close, k = 200)
amzn_sma[c(1:3, nrow(amzn_sma)), ]

amzn_sma[48:52, ]
amzn_sma[198:202, ]

amzn_sma2012 <- subset(amzn_sma, index(amzn_sma) >= "2012-01-01")
amzn_sma2012[c(1:3, nrow(amzn_sma2012)), ]

y_range <- range(amzn_sma2012, na.rm = TRUE)
y_range

png(file = "./images/chp01-plot7.png", width = 1000)
par(mfrow = c(1, 1))
plot(
  x = index(amzn_sma2012),
  xlab = "Date",
  y = amzn_sma2012$AMZN.Close,
  ylim = y_range,
  ylab = "Price ($)",
  type = "l",
  main = "Amazon - SMA January 1, 2012 - December 31, 2013"
)
lines(x = index(amzn_sma2012), y = amzn_sma2012$sma50)
lines(x = index(amzn_sma2012), y = amzn_sma2012$sma200, lty = 2)
legend("topleft",
  c("Amazon Price", "50-Day Moving Average", "200-Day Moving Average"),
  lty = c(1, 1, 2)
)
dev.off()

## --- 1.6.2 Volatility: Bollinger Bands
amzn_bb <- data_amzn[, 4]
amzn_bb[c(1:3, nrow(amzn_bb)), ]

amzn_bb$avg <- rollmeanr(amzn_bb$AMZN.Close, k = 20)
amzn_bb$sd <- rollapply(amzn_bb$AMZN.Close, width = 20, FUN = sd, fill = NA)
amzn_bb[c(1:3, nrow(amzn_bb)), ]

amzn_bb[18:22, ]

amzn_bb2013 <- subset(amzn_bb, index(amzn_bb) >= "2013-01-01")
amzn_bb2013[c(1:3, nrow(amzn_bb2013)), ]

amzn_bb2013$sd2up <- amzn_bb2013$avg + 2 * amzn_bb2013$sd
amzn_bb2013$sd2down <- amzn_bb2013$avg - 2 * amzn_bb2013$sd
amzn_bb2013[c(1:3, nrow(amzn_bb2013)), ]

y_range <- range(amzn_bb2013[, -3], na.rm = TRUE)
y_range

png(file = "./images/chp01-plot8.png", width = 1000)
plot(
  x = index(amzn_bb2013),
  xlab = "Date",
  y = amzn_bb2013$AMZN.Close,
  ylim = y_range, ylab = "Price ($)",
  type = "l",
  lwd = 3,
  main = paste(
    "Amazon - Bollinger Bands (20 days, 2 deviations) \n",
    " January 1, 2013 - December 31, 2013"
  )
)
lines(x = index(amzn_bb2013), y = amzn_bb2013$avg, lty = 2)
lines(x = index(amzn_bb2013), y = amzn_bb2013$sd2up, col = "gray40")
lines(x = index(amzn_bb2013), y = amzn_bb2013$sd2down, col = "gray40")
legend("topleft",
  c("Amazon Price", "20-Day Moving Average", "Upper Band", "Lower Band"),
  lty = c(1, 2, 1, 1),
  lwd = c(3, 1, 1, 1),
  col = c("black", "black", "gray40", "gray40")
)
dev.off()

## --- 1.6.3 Momentum: Relative Strength Index
amzn_rsi <- data_amzn[, 4]
amzn_rsi$delta <- diff(amzn_rsi$AMZN.Close)
amzn_rsi[c(1:3, nrow(amzn_rsi)), ]

amzn_rsi$up <- ifelse(amzn_rsi$delta > 0, 1, 0)
amzn_rsi$down <- ifelse(amzn_rsi$delta < 0, 1, 0)
amzn_rsi[c(1:3, nrow(amzn_rsi)), ]

amzn_rsi$up_val <- amzn_rsi$delta * amzn_rsi$up
amzn_rsi$down_val <- -amzn_rsi$delta * amzn_rsi$down
amzn_rsi <- amzn_rsi[-1, ]
amzn_rsi[c(1:3, nrow(amzn_rsi)), ]

amzn_rsi$up.first.avg <- rollapply(amzn_rsi$up_val,
  width = 14,
  FUN = mean,
  fill = NA,
  na.rm = TRUE
)
amzn_rsi$down.first.avg <- rollapply(amzn_rsi$down_val,
  width = 14,
  FUN = mean,
  fill = NA,
  na.rm = TRUE
)
amzn_rsi[c(1:15, nrow(amzn_rsi)), ]

up_val <- as.numeric(amzn_rsi$up_val)
down_val <-  as.numeric(amzn_rsi$down_val)
amzn_rsi$up.avg <- amzn_rsi$up.first.avg
for (i in 15:nrow(amzn_rsi)){
  amzn_rsi$up.avg[i] <- ((amzn_rsi$up.avg[i-1] * 13 + up_val[i]) / 14)
}
amzn_rsi$down.avg <- amzn_rsi$down.first.avg
for (i in 15:nrow(amzn_rsi)){
  amzn_rsi$down.avg[i] <- ((amzn_rsi$down.avg[i-1] * 13 + down_val[i]) / 14)
}
amzn_rsi[c(1:20, nrow(amzn_rsi)), ]

amzn_rsi$RS <- amzn_rsi$up.avg / amzn_rsi$down.avg
amzn_rsi$RSI <- 100 - (100 / (1 + amzn_rsi$RS))

amzn_rsi[c(14:20, nrow(amzn_rsi)), ]

amzn_rsi2012 <- subset(amzn_rsi[,ncol(amzn_rsi)],
                       index(amzn_rsi) >= "2012-01-01")
amzn_rsi2012[c(1:3, nrow(amzn_rsi2012)), ]

png(file = "./images/chp01-plot9.png", width = 1000)
title1 <- "Amazon - Relative Strength Index"
title2 <- "January 2012 - December 2013"
plot(
  x = index(amzn_rsi2012),
  xlab = "Date",
  y = amzn_rsi2012$RSI,
  ylab = "RSI (14-Day Moving Average)",
  ylim = c(0, 100),
  type = "l",
  main = paste(title1, "\n", title2)
)
abline(h = c(30, 70), lty = 2)
dev.off()


## ---- Further Reading
png(file = "./images/chp01-plot10.png", width = 1000)
chartSeries(data_amzn[, 4], TA = c(addSMA(n = c(50, 200))))
zoomChart("2012::2013")
dev.off()

png(file = "./images/chp01-plot11.png", width = 1000)
chartSeries(data_amzn[, 4],
  TA = c(addBBands(n = 20, sd = 2, maType = "SMA", draw = "bands"))
)
zoomChart("2012::2013")
dev.off()

png(file = "./images/chp01-plot12.png", width = 1000)
chartSeries(data_amzn[, 4],
  TA = c(addRSI(n = 14, maType = "EMA", wilder = TRUE))
)
zoomChart("2012::2013")
dev.off()
