##---- Part A Exercises

## --- A1
mydata <- "0 3 4 1 0 2 0 1 2 0 1 0 0 1 2 5 0 1 0 1 0 2 1 0 1 1 3 2 1 0 1 3 1 1 0 4 6 1 0 1"

mydata <- lapply(strsplit(mydata, " "), as.integer)[[1]]

mytable <- table(mydata)

mytable <-
  rbind(
    "Freq" = mytable,
    "%" = prop.table(mytable) * 100,
    "Cum %" = cumsum(prop.table(mytable)) * 100
  )

mytable <- rbind("Num of Falls" = as.integer(colnames(mytable)),
                 mytable)

falls <- as.data.frame(t(mytable))
falls

## --- A2
sum(falls[falls[, "Num of Falls"] > 0, "%"])

falls[falls$Freq == max(falls[, "Freq"]), ]

falls[falls$Freq == min(falls[, "Freq"]), ]

sum(falls[falls["Num of Falls"] <= 2, "%"])

sum(falls[, "Freq"])

boxplot(mydata)$out

## --- A3
png(file = "./images/chp01-plot1.png")
hist(
  mydata,
  breaks = -1:6,
  xlab = "Num of Falls",
  col = "red",
  main = paste(
    "The number of times nursing home residents",
    "\n",
    "who were aged 80 or older fell during a 12-month period."
  )
)
lines(seq(-0.5, 5.5), falls$Freq)
dev.off()

## --- A4
as.data.frame(psych::describe(mydata))[c("se", "skew", "kurtosis")]

## --- A5
age <- "
25to34 35to44 45to54 55to64 greater65
20 31 44 30 13
"

age <- read.table(text = age, header = TRUE)

png(file = "./images/chp01-plot2.png")
barplot(as.matrix(age),
  xlab = "Age Group",
  ylab = "Frequency",
  col = "red",
  main = "Patient's Age Group"
)
dev.off()
