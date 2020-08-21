##---- 2.1 Price Returns

##--- Step 1: Import IBM Data from Yahoo Finance

library(quantmod)
library(xts)

getSymbols.yahoo(
  Symbols = c("AMZN", "IBM", "^GSPC", "TSLA"),
  env = ".GlobalEnv",
  from = "2010-12-31",
  to = "2014-01-01"
)

data_amzn  <-  AMZN
data_ibm  <-  IBM
data_gspc  <-  GSPC
data_tsla <- TSLA

saveRDS(data_amzn, file = "./data/amzn.rds")
saveRDS(data_ibm, file = "./data/ibm.rds")
saveRDS(data_gspc, file = "./data/gspc.rds")
saveRDS(data_tsla, file = "./data/tsla.rds")


## --- Step 2: Subset the Data to Only Include the Closing Price

ibm_prc_ret <- data_ibm[, 4]
ibm_prc_ret[c(1:3, nrow(ibm_prc_ret)), ]

## --- Step 3: Calculate IBM’s Price Return

ibm_prc_ret$ibm_prc_ret <- Delt(ibm_prc_ret$IBM.Close)
ibm_prc_ret[c(1:3, nrow(ibm_prc_ret)), ]

## --- Step 4: Clean up Data Object

options(digits = 3)
ibm_prc_ret <- ibm_prc_ret[-1, 2]
ibm_prc_ret[c(1:3, nrow(ibm_prc_ret)), ]
options(digits = 7)

##---- 2.2 Total Returns

data_ibm[715:720, ]

## --- Step 1: Import Adjusted Closing Price Data

ibm_ret <- data_ibm[, "IBM.Adjusted"]
ibm_ret[c(1:3, nrow(ibm_ret)), ]

## --- Step 2: Calculate Total Return
ibm_ret$ibm_tot_ret <- Delt(ibm_ret$IBM.Adjusted)
ibm_ret[c(1:3, nrow(ibm_ret)), ]

##--- Step 3: Clean up the Data
options(digits = 3)
ibm_tot_ret <- ibm_ret[, "ibm_tot_ret"]
ibm_tot_ret[c(1:3, nrow(ibm_tot_ret)), ]
options(digits = 7)

## ---- 2.3 Logarithmic Total Returns

## --- Step 1: Import Adjusted Closing Price Data

ibm_log_ret <- data_ibm[, "IBM.Adjusted"]
ibm_log_ret[c(1:3, nrow(ibm_log_ret)), ]

## --- Step 2: Calculate Log Returns

ibm_log_ret$ibm_log_ret <- diff(log(ibm_ret$IBM.Adjusted))
ibm_log_ret[c(1:3, nrow(ibm_log_ret)), ]

##---Step 3: Clean up the Data

options(digits = 3)
ibm_log_ret <- ibm_log_ret[, 2]
ibm_log_ret[c(1:3, nrow(ibm_log_ret)), ]
options(digits = 7)

## --- Compare Log Returns with Arithmetic Returns
options(digits = 3, scipen = 100)
tot_rets <- cbind(ibm_tot_ret, ibm_log_ret)
tot_rets[c(1:3, nrow(tot_rets)), ]

max(abs(tot_rets$ibm_tot_ret - tot_rets$ibm_log_ret), na.rm = TRUE)
min(abs(tot_rets$ibm_tot_ret - tot_rets$ibm_log_ret), na.rm = TRUE)
options(digits = 7, scipen = 0)

## ---- 2.4 Cumulating Multi-Day Returns

## --- 2.4.1 Cumulating Arithmetic Returns

## --- Step 1: Import Data and Calculate Arithmetic Returns
ibm_acum <- ibm_ret[, "ibm_tot_ret"]
ibm_acum[c(1:3, nrow(ibm_acum)), ]

## --- Step 2: Set First Day Total Return Value to Zero
ibm_acum[1, 1] <- 0
ibm_acum[c(1:3, nrow(ibm_acum)), ]

## --- Step 3: Calculate Gross Daily Returns
ibm_acum$gross_ret <- 1 + ibm_acum$ibm_tot_ret
ibm_acum[c(1:3, nrow(ibm_acum)), ]

## --- Step 4: Calculate Cumulative Gross Returns
ibm_acum$gross_cum <- cumprod(ibm_acum$gross_ret)
ibm_acum[c(1:3, nrow(ibm_acum)), ]

## --- Step 5: Convert Cumulative Gross Returns to Cumulative Net Returns
ibm_acum$net_cum <- ibm_acum$gross_cum - 1
ibm_acum[c(1:3, nrow(ibm_acum)), ]

## --- 2.4.2 Cumulating Logarithmic Returns

## -- Step 1: Import Data and Calculate Logarithmic Returns
ibm_logcum <- ibm_log_ret
ibm_logcum[c(1:3, nrow(ibm_logcum)), ]

## -- Step 2: Set the First Log Return to Zero
ibm_logcum[1, 1] <- 0
ibm_logcum[c(1:3, nrow(ibm_logcum)), ]

## -- Step 3: Take the Sum of all Log Rets During the Investment Period
logcumret <- sum(ibm_logcum$ibm_log_ret)
logcumret

## --- Step 4: Convert Log Return Back to Arithmetic Return
cumret <- exp(logcumret) - 1
cumret

## ---- 2.4.3 Comparing Price Return and Total Return

## --- Step 1: Import Data and Calculate Price and Total Returns
ibm_ret <- cbind(ibm_prc_ret, ibm_ret[, "ibm_tot_ret"])
head(ibm_ret)
names(ibm_ret) <- c("prc.ret", "tot.ret")
ibm_ret[c(1:3, nrow(ibm_ret)), ]

##--- Step 2: Set First Returns to Zero
ibm_ret$prc.ret[1] <- 0
ibm_ret$tot.ret[1] <- 0
ibm_ret[c(1:3, nrow(ibm_ret)), ]

## --- Step 3: Calculate Gross Returns
ibm_ret$gross_prc <- 1 + ibm_ret$prc.ret
ibm_ret$gross_tot <- 1 + ibm_ret$tot.ret
ibm_ret[c(1:3, nrow(ibm_ret)), ]

##--- Step 4: Cumulate the Gross Returns
ibm_ret$cum_prc <- cumprod(ibm_ret$gross_prc)
ibm_ret$cum_tot <- cumprod(ibm_ret$gross_tot)
ibm_ret[c(1:3, nrow(ibm_ret)), ]

## --- Step 5: Plot the Two Return Series
y_range <- range(ibm_ret[, 5:6])
y_range

png(file = "./images/chp02-plot1.png", width = 1000)
plot(
  ibm_ret$cum_tot,
  type = "l",
  auto_grid = FALSE,
  xlab = "Date",
  ylab = "Value of Investment ($)",
  ylim = y_range,
  minor_ticks = FALSE,
  main = paste(
    "IBM Stock Performance Based On Total Returns and Price Returns ",
    "\n",
    "December 31, 2010 - December 31, 2013"
  )
)
lines(ibm_ret$cum_prc, type = "l", lty = 3)
abline(h = 1, col = "black")
legend(
  "topleft",
  col = c("black", "black"),
  lty = c(1, 3),
  c(
    "Value Based on Total Return",
    "Value Based on Price Return"
  )
)
dev.off()

##---- 2.5 Weekly Returns
class(data_amzn)

## --- Step 1: Import Data into R
wk <- data_amzn
wk[c(1:3, nrow(data_amzn)), ]

## --- Step 2: Convert to Daily Data Data to Weekly Data
amzn_weekly <- to.weekly(wk)
amzn_weekly[c(1:3, nrow(amzn_weekly)), ]

## --- Step 3: Clean up Weekly Data to Keep Only the Adjusted Closing Prices
## --- at the End of Each Week
amzn_weekly <- amzn_weekly[, "wk.Adjusted"]
amzn_weekly[c(1:3, nrow(amzn_weekly)), ]

## --- Step 4: Calculate Weekly Returns
amzn_weekly$ret <- Delt(amzn_weekly$wk.Adjusted)
amzn_weekly[c(1:3, nrow(amzn_weekly)), ]

## --- Step 5: Cleanup Weekly Returns Data by Deleting the First Observation
amzn_weekly <- amzn_weekly[-1, 2]
amzn_weekly[c(1:3, nrow(amzn_weekly)), ]

## --- Verify Weekly Return Calculations
amzn_acum[c(6, 11, 15), ]

## ---- 2.6 Monthly Returns
## --- Step 1: Import Data into R
mo <- data_amzn
mo[c(1:3, nrow(data_amzn)), ]

## --- Step 2: Convert Daily Data to Monthly Data
amzn_monthly <- to.monthly(mo)
amzn_monthly[c(1:3, nrow(amzn_monthly)), ]

## --- Step 3: Clean up Data to Include Only Adjusted Closing Prices
## --- for the End of Each Month
amzn_monthly <- amzn_monthly[, "mo.Adjusted"]
amzn_monthly[c(1:3, nrow(amzn_monthly)), ]

## ---- Step 4: Calculate Monthly Returns
amzn_monthly$ret <- Delt(amzn_monthly$mo.Adjusted)
amzn_monthly[c(1:3, nrow(amzn_monthly)), ]

## --- Step 5: Cleanup Monthly Data Object
amzn_monthly <- amzn_monthly[-1, 2]
amzn_monthly[c(1:3, nrow(amzn_monthly)), ]

## ---- 2.7 Comparing Performance of Multiple Securities: Total Returns
##--- Step 1: Importing Price Data
data_amzn[c(1:3, nrow(data_amzn)), ]
data_ibm[c(1:3, nrow(data_ibm)), ]

## --- Step 2: Combine Data
multi <- data_amzn[, "AMZN.Adjusted"]
multi <- merge(multi, data_gspc[, "GSPC.Adjusted"])
multi <- merge(multi, data_ibm[, "IBM.Adjusted"])
multi[c(1:3, nrow(multi)), ]

## --- Step 3: Converting Data into a data.frame Object
multi_df <- cbind(data.frame(index(multi)), data.frame(multi))
names(multi_df) <- paste(c("date", "AMZN", "GSPC", "IBM"))
multi_df[c(1:3, nrow(multi_df)), ]

## --- Step 4: Constructing Normalized Values for Each Security
multi_df$amzn_idx <- multi_df$AMZN / multi_df$AMZN[1]
multi_df$gspc_idx <- multi_df$GSPC / multi_df$GSPC[1]
multi_df$ibm_idx <- multi_df$IBM / multi_df$IBM[1]
multi_df[c(1:3, nrow(multi_df)), c("amzn_idx", "ibm_idx", "gspc_idx")]

## --- Step 5: Plotting the Index Values of Each Security
y_range <- range(multi_df[, c("amzn_idx", "ibm_idx", "gspc_idx")])
y_range

png(file = "./images/chp02-plot2.png", width = 1000)
par(mfrow = c(1, 1))
plot(
  x = multi_df$date,
  xlab = "Date",
  y = multi_df$gspc_idx,
  ylim = y_range,
  ylab = "Value of $1 Investment ($)",
  type = "l",
  col = "black",
  lty = 1,
  lwd = 2,
  main = paste("Value of $1 Invested in AMZN, IBM, YHOO, And the S&P 500 ",
               "\n",
               " Index Based on Total Returns ",
               " December 31, 2010 - December 31, 2013")
)
lines(x = multi_df$date,
      y = multi_df$amzn_idx,
      col = "black",
      lty = 2,
      lwd = 1)
lines(x = multi_df$date,
      y = multi_df$ibm_idx,
      col = "gray40",
      lty = 1,
      lwd = 2)
abline(h  =  1, lty  =  1, col  =  "black")
legend("topleft",
       c("AMZN", "IBM", "S&P 500 Index"),
       col  =  c("black", "gray40", "gray60", "black"),
       lty  =  c(2, 1, 1, 1),
       lwd  =  c(1, 2, 1, 2))
dev.off()
