## ---- Chapter 3: Portfolio Returns

## --- Step 1: Find First and Last Adjusted Closing Price
## --- for Each Security Over the Investment Period

library(quantmod)
library(xts)

data_amzn <- readRDS("./data/amzn.rds")
data_ibm <- readRDS("./data/ibm.rds")
data_gspc <- readRDS("./data/gspc.rds")

multi <- data_amzn[, "AMZN.Adjusted"]
multi <- merge(multi, data_gspc[, "GSPC.Adjusted"])
multi <- merge(multi, data_ibm[, "IBM.Adjusted"])

period_ret <- multi[c(1, nrow(multi)), ]
period_ret

## --- Step 2: Calculate Returns for Each Security Over the Investment Period

rets <- lapply(period_ret, Delt)
rets

## --- Step 3: Convert to a data.frame and Cleanup Data
rets <- data.frame(rets)
rets

rets <- rets[2, ] * 100
names(rets) <- paste(c("AMZN", "GSPC", "IBM"))
rets

## --- Step 4: Calculate Weight of Each Security in the Portfolio
i_amzn <- 50000
i_gspc <- 40000
i_ibm <- 10000

w_amzn <- i_amzn / (i_amzn + i_gspc + i_ibm)
w_amzn

w_gspc <- i_gspc / (i_amzn + i_gspc + i_ibm)
w_gspc

w_ibm <- i_ibm / (i_amzn + i_gspc + i_ibm)
w_ibm

## --- Step 5: Calculate Portfolio Return
port_ret_4asset <- w_amzn * rets$AMZN + w_gspc * rets$GSPC + w_ibm * rets$IBM
port_ret_4asset

## ---- 3.2 Constructing Portfolio Returns (Matrix Algebra)
wgt <- c(0.5, 0.4, 0.1)
mat_wgt <- matrix(wgt, 1)
mat_wgt

ret <- c(rets$AMZN, rets$GSPC, rets$IBM)
ret

mat_ret <- matrix(ret, 3)
mat_ret

port_ret <- mat_wgt %*% mat_ret
port_ret

## ---- 3.3 Constructing Benchmark Portfolio Returns
## --- Step 1: Importing the Price Data
data_amzn[c(1:3, nrow(data_amzn)), ]
data_ibm[c(1:3, nrow(data_ibm)), ]

## --- Step 2: Create Object with Only the Relevant Data
port <- data_amzn[, c("AMZN.Close", "AMZN.Adjusted")]
port <- merge(port, data_ibm[, c("IBM.Close", "IBM.Adjusted")])
port[c(1:3, nrow(port)), ]

## --- Step 3: Calculate Returns of Each Security
port$amzn_ret <- Delt(port$AMZN.Adjusted)
port$ibm_ret <- Delt(port$IBM.Adjusted)
port[c(1:3, nrow(port)), ]

## --- Step 4: Convert to data_frame Object and Subset Data
port <- cbind(data.frame(index(port)), data.frame(port))
names(port)[1] <- paste("date")
port[c(1:3, nrow(port)), ]

port <- subset(port,
               port$date >= "2012-12-31" &
               port$date <= "2013-12-31")
port[c(1:3, nrow(port)), ]

## --- 3.3.1 Equal-Weighted Portfolio
## -- Step 1: Keep Only Variables We Need to Construct EW Portfolio
ewport <- port[c("date", "amzn_ret", "ibm_ret")]
ewport[c(1:3, nrow(ewport)), ]

names(ewport) <- paste(c("date", "amzn", "ibm"))
rownames(ewport) <- seq_len(nrow(ewport))
ewport[c(1:3, nrow(ewport)), ]

## -- Step 2: Converting Net Returns to Gross Returns
ewport$amzn <- 1 + ewport$amzn
ewport$ibm <- 1 + ewport$ibm
ewport[c(1:3, nrow(ewport)), ]

## -- Step 3: Calculate EW Portfolio Values for 1Q 2013
ew_q1 <- subset(
  ewport,
  ewport$date >= as.Date("2012-12-31") &
    ewport$date <= as.Date("2013-03-31")
)
ew_q1[c(1:3, nrow(ew_q1)), ]

ew_q1[1, c("amzn", "ibm")] <- 1
ew_q1$amzn <- cumprod(ew_q1$amzn)
ew_q1$ibm <- cumprod(ew_q1$ibm)
ew_q1[c(1:3, nrow(ew_q1)), ]

num_sec <- 2

ew_q1$amzn_idx <- (1 / num_sec) * ew_q1$amzn
ew_q1$ibm_idx <- (1 / num_sec) * ew_q1$ibm
ew_q1[c(1:3, nrow(ew_q1)), ]

q1_val <- data.frame(rowSums(ew_q1[, c("amzn_idx", "ibm_idx")]))
q1_val[c(1:3, nrow(q1_val)), ]

names(q1_val) <- paste("port_val")
q1_val$date <- ew_q1$date
q1_val[c(1:3, nrow(q1_val)), ]

q2_inv <- q1_val[nrow(q1_val), 1]
q2_inv

## -- Step 4: Calculate EW Portfolio Values for 2Q 2013
ew_q2 <- subset(
  ewport,
  ewport$date >= as.Date("2013-04-01") &
    ewport$date <= as.Date("2013-06-30")
)
ew_q2[c(1:3, nrow(ew_q2)), ]

ew_q2$amzn <- cumprod(ew_q2$amzn)
ew_q2$ibm <- cumprod(ew_q2$ibm)
ew_q2[c(1:3, nrow(ew_q2)), ]

ew_q2$amzn_idx <- (q2_inv / num_sec) * ew_q2$amzn
ew_q2$ibm_idx <- (q2_inv / num_sec) * ew_q2$ibm
ew_q2[c(1:3, nrow(ew_q2)), ]

q2_val <- data.frame(rowSums(ew_q2[, c("amzn_idx", "ibm_idx")]))
q2_val[c(1:3, nrow(q2_val)), ]

names(q2_val) <- paste("port_val")
q2_val$date <- ew_q2$date
q2_val[c(1:3, nrow(q2_val)), ]

q3_inv <- q2_val[nrow(q2_val), 1]
q3_inv

## -- Step 5: Calculate EW Portfolio Values for 3Q 2013
ew_q3 <- subset(
  ewport,
  ewport$date >= as.Date("2013-07-01") &
    ewport$date <= as.Date("2013-09-30")
)
ew_q3[c(1:3, nrow(ew_q3)), ]

ew_q3$amzn <- cumprod(ew_q3$amzn)
ew_q3$ibm <- cumprod(ew_q3$ibm)
ew_q3[c(1:3, nrow(ew_q3)), ]

ew_q3$amzn_idx <- (q3_inv / num_sec) * ew_q3$amzn
ew_q3$ibm_idx <- (q3_inv / num_sec) * ew_q3$ibm
ew_q3[c(1:3, nrow(ew_q3)), ]

q3_val <- data.frame(rowSums(ew_q3[, c("amzn_idx", "ibm_idx")]))
q3_val[c(1:3, nrow(q3_val)), ]

names(q3_val) <- paste("port_val")
q3_val$date <- ew_q3$date
q3_val[c(1:3, nrow(q3_val)), ]

q4_inv <- q3_val[nrow(q3_val), 1]
q4_inv

## -- Step 6: Calculate EW Portfolio Values for 4Q 2013
ew_q4 <- subset(
  ewport,
  ewport$date >= as.Date("2013-10-01") &
    ewport$date <= as.Date("2013-12-31")
)
ew_q4[c(1:3, nrow(ew_q4)), ]

ew_q4$amzn <- cumprod(ew_q4$amzn)
ew_q4$ibm <- cumprod(ew_q4$ibm)
ew_q4[c(1:3, nrow(ew_q4)), ]

ew_q4$amzn_idx <- (q4_inv / num_sec) * ew_q4$amzn
ew_q4$ibm_idx <- (q4_inv / num_sec) * ew_q4$ibm
ew_q4[c(1:3, nrow(ew_q4)), ]

q4_val <- data.frame(rowSums(ew_q4[, c("amzn_idx", "ibm_idx")]))
q4_val[c(1:3, nrow(q4_val)), ]

names(q4_val) <- paste("port_val")
q4_val$date <- ew_q4$date
q4_val[c(1:3, nrow(q4_val)), ]

## -- Step 7: Combine Quarterly EW Portfolio Values into One Data Object
ew_portval <- rbind(q1_val, q2_val, q3_val, q4_val)
ew_portval[c(1:3, nrow(ew_portval)), ]

## --- 3.3.2 Value-Weighted Portfolio
## -- Step 1: Keep Only Variables We Need to Construct VW Portfolio
vwport <- port[, c("date", "AMZN.Close", "IBM.Close", "amzn_ret", "ibm_ret")]
vwport[c(1:3, nrow(vwport)), ]
rownames(vwport) <- seq_len(nrow(vwport))
vwport[c(1:3, nrow(vwport)), ]

## -- Step 2: Converting Net Returns to Gross Returns
vwport$amzn_ret <- 1 + vwport$amzn_ret
vwport$ibm_ret <- 1 + vwport$ibm_ret
vwport[c(1:3, nrow(vwport)), ]

## -- Step 3: Calculate the Market Capitalization
## -- of Each Security in the Portfolio
date <- seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by = 1)
date <- data.frame(date)
date[c(1:3, nrow(date)), ]

## - Create Data Object With Daily Prices,
## - Filling in Last Available Price on Non-trading Days
price_qtr <- vwport[, c("date", "AMZN.Close", "IBM.Close")]
price_qtr[c(1:3, nrow(price_qtr)), ]

price_qtr <- na.locf(
  merge(
    x = date,
    y = price_qtr,
    by = "date",
    all.x = TRUE
  )
)
price_qtr[c(1:3, nrow(price_qtr)), ]

## - Keep Only Prices at the End of Each Calendar Quarter
price_qtr <- subset(
  price_qtr,
  price_qtr$date == as.Date("2012-12-31") |
    price_qtr$date == as.Date("2013-03-31") |
    price_qtr$date == as.Date("2013-06-30") |
    price_qtr$date == as.Date("2013-09-30")
)
price_qtr

## - Obtain Shares Outstanding Data from SEC Filings
price_qtr$amzn_shout <- c(454000000, 455000000, 457000000, 458000000)
price_qtr$ibm_shout <- c(1117367676, 1108794396, 1095425823, 1085854383)
price_qtr

## - Calculate Market Capitalization of Each Security
str(price_qtr)

price_qtr$date <- as.Date(price_qtr$date)
price_qtr$amzn_close <- as.numeric(price_qtr$AMZN.Close)
price_qtr$ibm_close <- as.numeric(price_qtr$IBM.Close)
str(price_qtr)

weights <- price_qtr
weights$amzn_mcap <- weights$amzn_close * weights$amzn_shout
weights$ibm_mcap <- weights$ibm_close * weights$ibm_shout
weights

## - Calculate Quarter-end Aggregate Market Capitalization
weights$tot_mcap <- rowSums(weights[c("amzn_mcap", "ibm_mcap")])
weights

## -- Step 4: Calculate Quarter-end Weights of Each Security in the Portfolio
weights$amzn_wgt <- weights$amzn_mcap / weights$tot_mcap
weights$ibm_wgt <- weights$ibm_mcap / weights$tot_mcap
weights

weight <- weights[, c("date", "amzn_wgt", "ibm_wgt")]
weight

weight$date <- weight$date + 1
weight

## -- Step 5: Calculating the Quarterly VW Portfolio Values
date <- seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by = 1)
date <- data.frame(date)
date[c(1:3, nrow(date)), ]

vwret <- na.locf(merge(date, weight, by = "date", all.x = TRUE))
vwret[c(1:3, nrow(vwret)), ]

## -- Step 6: Create Pie Charts of the Weights
str(vwret)

vwret$date <- as.Date(vwret$date)
vwret$amzn_wgt <- as.numeric(vwret$amzn_wgt)
vwret$ibm_wgt <- as.numeric(vwret$ibm_wgt)
vwret[c(1:3, nrow(vwret)), ]

str(vwret)

q1_vw_wgt <- subset(vwret, vwret$date == as.Date("2013-01-01"))
q1_vw_wgt
q2_vw_wgt <- subset(vwret, vwret$date == as.Date("2013-04-01"))
q2_vw_wgt
q3_vw_wgt <- subset(vwret, vwret$date == as.Date("2013-07-01"))
q3_vw_wgt
q4_vw_wgt <- subset(vwret, vwret$date == as.Date("2013-10-01"))
q4_vw_wgt

png(file = "./images/chp03-plot1.png")
par(mfrow = c(2, 2))
q1_pie_values <- as.numeric(q1_vw_wgt[, -1])
q1_pie_labels <- names(q1_vw_wgt[, -1])
pie(q1_pie_values,
    labels = q1_pie_labels,
    col = rainbow(length(q1_pie_labels)),
    main = "Q1 Value Weighting")

q2_pie_values <- as.numeric(q2_vw_wgt[, -1])
q2_pie_labels <- names(q1_vw_wgt[, -1])
pie(q2_pie_values,
    labels = q2_pie_labels,
    col = rainbow(length(q2_pie_labels)),
    main = "Q2 Value Weighting")

q3_pie_values <- as.numeric(q3_vw_wgt[, -1])
q3_pie_labels <- c("Amazon", "IBM")
pct <- round(q3_pie_values * 100)
q3_pie_labels <- paste(q3_pie_labels, pct) # Add Pct to Labels
q3_pie_labels <- paste(q3_pie_labels, "%", sep = "") # Add % Sign
pie(q3_pie_values,
    labels = q3_pie_labels,
    col = rainbow(length(q3_pie_labels)),
    main = "Q3 Value Weighting")

q4_pie_values <- as.numeric(q4_vw_wgt[, -1])
q4_pie_labels <- c("Amazon", "IBM")
pct <- round(q4_pie_values * 100)
q4_pie_labels <- paste(q4_pie_labels, pct) # Add Pct to Labels
q4_pie_labels <- paste(q4_pie_labels, "%", sep = "") # Add % Sign
pie(q4_pie_values,
    labels = q4_pie_labels,
    col = rainbow(length(q4_pie_labels)),
    main = "Q4 Value Weighting")
dev.off()

## --- Step 7: CalculatingVW PortfolioValues for 1Q 2013
vw_q1 <- subset(vwport[, c("date", "amzn_ret", "ibm_ret")],
                vwport$date >= as.Date("2012-12-31") &
                vwport$date <= as.Date("2013-03-31"))
vw_q1[c(1:3, nrow(vw_q1)), ]

names(vw_q1) <- paste(c("date", "amzn", "ibm"))
vw_q1[c(1:3, nrow(vw_q1)), ]

vw_q1[1, c("amzn", "ibm")] <- 1
vw_q1$amzn <- cumprod(vw_q1$amzn)
vw_q1$ibm <- cumprod(vw_q1$ibm)
vw_q1[c(1:3, nrow(vw_q1)), ]

vw_q1$amzn_idx <- (1 * q1_vw_wgt$amzn_wgt) * vw_q1$amzn
vw_q1$ibm_idx <- (1 * q1_vw_wgt$ibm_wgt) * vw_q1$ibm
vw_q1[c(1:3, nrow(vw_q1)), ]

q1_vw_val <- data.frame(rowSums(vw_q1[, c("amzn_idx", "ibm_idx")]))
q1_vw_val[c(1:3, nrow(q1_vw_val)), ]

names(q1_vw_val) <- paste("port_val")
q1_vw_val$date <- vw_q1$date
q1_vw_val[c(1:3, nrow(q1_vw_val)), ]
q2_vw_inv <- q1_vw_val[nrow(q1_vw_val), 1]
q2_vw_inv

## --- Step 8: Calculating VW Portfolio Values for 2Q 2013
vw_q2 <- subset(vwport[, c("date", "amzn_ret", "ibm_ret")],
                vwport$date >= as.Date("2013-04-01") &
                vwport$date <= as.Date("2013-06-30"))
vw_q2[c(1:3, nrow(vw_q2)), ]

names(vw_q2) <- paste(c("date", "amzn", "ibm"))
vw_q2[c(1:3, nrow(vw_q2)), ]

vw_q2$amzn <- cumprod(vw_q2$amzn)
vw_q2$ibm <- cumprod(vw_q2$ibm)
vw_q2[c(1:3, nrow(vw_q2)), ]

vw_q2$amzn_idx <- (q2_vw_inv * q2_vw_wgt$amzn_wgt) * vw_q2$amzn
vw_q2$ibm_idx <- (q2_vw_inv * q2_vw_wgt$ibm_wgt) * vw_q2$ibm
vw_q2[c(1:3, nrow(vw_q2)), ]

q2_vw_val <- data.frame(rowSums(vw_q2[, c("amzn_idx", "ibm_idx")]))
q2_vw_val[c(1:3, nrow(q2_vw_val)), ]

names(q2_vw_val) <- paste("port_val")
q2_vw_val$date <- vw_q2$date
q2_vw_val[c(1:3, nrow(q2_vw_val)), ]

q3_vw_inv <- q2_vw_val[nrow(q2_vw_val), 1]
q3_vw_inv

## --- Step 9: Calculating VW Portfolio Values for 3Q 2013
vw_q3 <- subset(vwport[, c("date", "amzn_ret", "ibm_ret")],
                vwport$date >= as.Date("2013-07-01") &
                vwport$date <= as.Date("2013-09-30"))
names(vw_q3) <- paste(c("date", "amzn", "ibm"))
vw_q3[c(1:3, nrow(vw_q3)), ]

vw_q3$amzn <- cumprod(vw_q3$amzn)
vw_q3$ibm <- cumprod(vw_q3$ibm)
vw_q3[c(1:3, nrow(vw_q3)), ]

vw_q3$amzn_idx <- (q3_vw_inv * q3_vw_wgt$amzn_wgt) * vw_q3$amzn
vw_q3$ibm_idx <- (q3_vw_inv * q3_vw_wgt$ibm_wgt) * vw_q3$ibm
vw_q3[c(1:3, nrow(vw_q3)), ]

q3_vw_val <- data.frame(rowSums(vw_q3[, c("amzn_idx", "ibm_idx")]))
q3_vw_val[c(1:3, nrow(q3_vw_val)), ]
names(q3_vw_val) <- paste("port_val")

q3_vw_val$date <- vw_q3$date
q3_vw_val[c(1:3, nrow(q3_vw_val)), ]

q4_vw_inv <- q3_vw_val[nrow(q3_vw_val), 1]
q4_vw_inv

## --- Step 10: Calculating VW Portfolio Values for 4Q 2013
vw_q4 <- subset(vwport[, c("date", "amzn_ret", "ibm_ret")],
                vwport$date >= as.Date("2013-10-01") &
                vwport$date <= as.Date("2013-12-31"))
vw_q4[c(1:3, nrow(vw_q4)), ]

names(vw_q4) <- paste(c("date", "amzn", "ibm"))
vw_q4[c(1:3, nrow(vw_q4)), ]

vw_q4$amzn <- cumprod(vw_q4$amzn)
vw_q4$ibm <- cumprod(vw_q4$ibm)
vw_q4[c(1:3, nrow(vw_q4)), ]

vw_q4$amzn_idx <- (q4_vw_inv * q4_vw_wgt$amzn_wgt) * vw_q4$amzn
vw_q4$ibm_idx <- (q4_vw_inv * q4_vw_wgt$ibm_wgt) * vw_q4$ibm
vw_q4[c(1:3, nrow(vw_q4)), ]

q4_vw_val <- data.frame(rowSums(vw_q4[, c("amzn_idx", "ibm_idx")]))
q4_vw_val[c(1:3, nrow(q4_vw_val)), ]

names(q4_vw_val) <- paste("port_val")
q4_vw_val$date <- vw_q4$date
q4_vw_val[c(1:3, nrow(q4_vw_val)), ]

## --- Step 11: Combining Quarterly VW Portfolio Values into One Data Object
vw_portval <- rbind(q1_vw_val, q2_vw_val, q3_vw_val, q4_vw_val)
vw_portval[c(1:3, nrow(vw_portval)), ]


## --- 3.3.3 Normalized EW and VW Portfolio Price Chart
## -- Step 1: Combine the Data
port_val <- merge(vw_portval, ew_portval, by = "date")
port_val[c(1:3, nrow(port_val)), ]

## -- Step 2: Rename the Variables
names(port_val) <- paste(c("date", "vw_cum", "ew_cum"))
port_val[c(1:3, nrow(port_val)), ]

## -- Step 3: Plot the Data
png(file = "./images/chp03-plot2.png")
par(mfrow = c(1, 1))
y_range <- range(port_val[, c("vw_cum", "ew_cum")])
y_range
plot(port_val$ew_cum,
  type = "l",
  xlab = "Date",
  ylab = "Value of Investment",
  ylim = y_range,
  lty = 1,
  main = paste(
    "Value of $1 Investment in Equal-Weighted and Value-Weighted ",
    "\n",
    "Portfolios of AMZN, and IBM ",
    "\n",
    "December 31, 2012 - December 31, 2013"
  )
)
lines(port_val$vw_cum, lty = 2)
abline(h = 1, lty = 1)
legend("topleft",
  c("Equal-Weighted Portfolio", "Value-Weighted Portfolio"),
  lty = c(1, 2)
)
dev.off()

## --- 3.3.4 Saving Benchmark Portfolio Returns into a CSV File
## -- Step 1: Convert the Data into an xts Object
port_xts <- xts(port_val[, 2:3], order.by = port_val[, 1])
port_xts[c(1:3, nrow(port_xts)), ]

port_xts$lag_vw <- Lag(port_xts$vw_cum, k = 1)
port_xts$lag_ew <- Lag(port_xts$ew_cum, k = 1)
port_xts[c(1:3, nrow(port_xts)), ]

## -- Step 2: Create EW and vw Returns
port_xts$vw_ret <- port_xts$vw_cum / port_xts$lag_vw - 1
port_xts$ew_ret <- port_xts$ew_cum / port_xts$lag_ew - 1
port_xts[c(1:3, nrow(port_xts)), ]

## -- Step 3: Clean up the Data
port_ret <- port_xts[, c(1, 2, 5, 6)]
port_ret[c(1:3, nrow(port_ret)), ]

## -- Step 4: Create a Date Variable in the Data Object
csv_port <- cbind(data.frame(index(port_ret)), data.frame(port_ret))
names(csv_port)[1] <- paste("date")
csv_port[c(1:3, nrow(csv_port)), ]

## -- Step 5: Replace Index with Observation Numbers
rownames(csv_port) <- seq_len(nrow(csv_port))
csv_port[c(1:3, nrow(csv_port)), ]

## -- Step 6: Save the Data to a CSV File
write.csv(csv_port, "./data/HypotheticalPortfolioDaily.csv")
