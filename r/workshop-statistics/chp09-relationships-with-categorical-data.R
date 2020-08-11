##---- Activity 9-2: Age and Political Ideology
mydata <- "
age.group ideology count
under-30 liberal 83
30-49 liberal 119
over-50 liberal 88
under-30 moderate 140
30-49 moderate 280
over-50 moderate 284
under-30 conservative 73
30-49 conservative 161
over-50 conservative 214
"
mydata <- read.table(header = TRUE, text = mydata)

tmp <- c()
for (i in seq_len(nrow(mydata))) {
  ages <- rep(mydata[i, "age.group"], mydata[i, "count"])
  ideologies <- rep(mydata[i, "ideology"], mydata[i, "count"])
  tmp <- rbind(tmp, cbind(ages, ideologies))
}
mydata <- as.data.frame(tmp)

myfile <- "./data/chp09-politics.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp06-politics.rds"
mydata <- readRDS(myfile)

mytable <- with(mydata, table(ages, ideologies))
mytable <- mytable[c(3, 1, 2), c(2, 3, 1)]
mytable
margin.table(mytable, 1)["under-30"]
margin.table(mytable, 1)["30-49"]
margin.table(mytable, 1)["over-50"]

png(file = "./images/chp09-plot1.png")
barplot(margin.table(prop.table(mytable), 1),
        xlab = "Age", ylab = "Proportion",
        main = "Bar graph of age variable")
dev.off()

mytable["under-30", "liberal"]
mytable["under-30", "moderate"]
mytable["under-30", "conservative"]

counts <- t(prop.table(mytable[c("30-49", "over-50"), ], 1))
png(file = "./images/chp09-plot2.png")
barplot(counts,
  main = "Segmented bar graph of political ideology data",
  xlab = "Age", ylab = "Percentage",
  col = c("red", "yellow", "green"),
  legend = rownames(counts)
)
dev.off()

counts <- t(prop.table(mytable, 1))
png(file = "./images/chp09-plot3.png")
barplot(counts,
  main = "Segmented bar graph of political ideology data",
  xlab = "Age", ylab = "Percentage",
  col = c("red", "yellow", "green"),
  legend = rownames(counts)
)
dev.off()


## ---- Activity 9-4: Hypothetical Hospital Recovery Rates
mytable <- matrix(c(800, 900, 200, 100), 2, 2)
rownames(mytable) <- c("hospitalA", "hospitalB")
colnames(mytable) <- c("survived", "died")
mytable <- as.table(mytable)
mytable

addmargins(prop.table(mytable, 1), 2)

good_cond <- matrix(c(590, 870, 10, 30), 2, 2)
rownames(good_cond) <- c("hospitalA", "hospitalB")
colnames(good_cond) <- c("survived", "died")
good_cond <- as.table(good_cond)
good_cond

options(digits = 1)
addmargins(prop.table(good_cond, 1), 2)

bad_cond <- matrix(c(210, 30, 190, 70), 2, 2)
rownames(bad_cond) <- c("hospitalA", "hospitalB")
colnames(bad_cond) <- c("survived", "died")
mytable <- as.table(bad_cond)
mytable

addmargins(prop.table(bad_cond, 1), 2)

good_cond + bad_cond

## ---- Activity 9-5: Hypothetical Employee Retention Predictions
mytable <- matrix(c(63, 21, 12, 4), 2, 2)
rownames(mytable) <- c("predicted to stay", "predicted to leave")
colnames(mytable) <- c("actually stays", "actually leaves")
mytable <- as.table(mytable)
addmargins(mytable)

addmargins(prop.table(mytable, 1), 2)

png(file = "./images/chp09-plot4.png")
barplot(t(prop.table(mytable, 1)),
  main = "Segmented bar graph of political ideology data",
  ylab = "Percentage",
  legend = colnames(mytable)
)
dev.off()
