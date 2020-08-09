library(ggplot2)

## Stemplots
myfile <- "./data/chp01-unemployment.rds"
mydata <- readRDS(myfile)

stem(mydata$Rate, scale=2)

ggplot(mydata, aes(x=Rate)) + geom_dotplot() +
  xlim(2, 12) + labs(title="Umemployment rate") +
  theme(plot.title=element_text(hjust=0.5))

## Activity 2-3: British Rulers’ Reigns

mydata <- "
ruler;reign
William I;21
William II;13
Henry I;35
Stephen;19
Henry II;35
Richard I;10
John;17
Henry pIII;56
Edward I;35
Edward II;20
Edward III;50
Richard II;22
Henry IV;13
Henry V;9
Henry VI;39
Edward IV;22
Edward V;0
Richard III;2
Henry VII;24
Henry VIII;38
Edward VI;6
Mary I;5
Elizabeth I;44
James I;22
Charles I;24
Charles II;25
James II;3
William III;13
Mary II;6
Anne;12
George I;13
George II;33
George III;59
George IV;10
William IV;7
Victoria;63
Edward VII;9
George V;25
Edward VIII;1
George VI;15
"

mydata <- read.table(header=TRUE, text=mydata, sep=";")
myfile <- "./data/chp02-reigns.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp02-reigns.rds"
mydata <- readRDS(myfile)

tail(mydata[order(mydata$reign),], 1)
head(mydata[order(mydata$reign),], 1)

stem(mydata$reign)

summary(mydata$reign)
length(mydata$reign)

## Houses
mydata <- data.frame(price=c(94, 98, 104, 115, 239, 159, 142,
            179, 54, 145, 119, 199, 149, 69,
            375, 215, 145, 78, 64, 126, 121,
            94, 79))
mydata
ggplot(mydata, aes(x=price)) + geom_dotplot() +
  labs(title="House Prices") +
  theme(plot.title=element_text(hjust=0.5))

# note that the values are rounded up
stem(mydata$price, scale=2)

## Histograms
mydata <- data.frame(price=c(148, 164, 307, 121, 161, 59,
                             139, 109, 127, 205, 154, 149,
                             61, 113, 34, 97, 72, 225, 289,
                             115, 79, 36, 119, 79, 168, 122,
                             118, 129, 189, 118, 217, 179,
                             65, 179, 67, 117, 104, 73, 225,
                             209, 134, 264, 126, 298, 177,
                             99, 116, 79, 62, 449, 144, 143,
                             129, 76, 595, 126, 215, 86, 89,
                             68, 119, 139, 57, 169, 119, 115,
                             134, 54, 110, 77, 124, 74, 206,
                             58, 89, 125, 254, 121, 91, 79,
                             89, 135, 168, 64, 109, 123, 125,
                             158, 155, 64, 53, 137, 237, 109,
                             52, 229, 94, 39, 184, 139, 154,
                             175, 119, 75))

numbins = 12
png("./images/chp02-plot1.png")
histdata <- hist(mydata$price, numbins, xlab="Price", main="House Prices")
dev.off()
intervals <- cbind((histdata$breaks+1)[-(numbins+1)], histdata$breaks[-1])
data.frame(interval=paste("(", paste(intervals[,1], intervals[,2], sep="-"), ")", sep=""),
           count=histdata$counts,
           proportion=round(histdata$counts/sum(histdata$counts), 2))

## Activity 2-8: Marriage Agesn
mydata <- "
husband wife
25 22
25 32
51 50
25 25
38 33
30 27
60 45
54 47
31 30
54 44
23 23
34 39
25 24
23 22
19 16
71 73
26 27
31 36
26 24
62 60
29 26
31 23
29 28
35 36
"
mydata <- read.table(header=TRUE, text=mydata)
myfile <- "./data/chp03-marriage.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp03-marriage.rds"
mydata <- readRDS(file=myfile)

stem(mydata$husband, scale=2)
stem(mydata$wife, scale=2)

png("./images/chp02-plot2.png")
hist1 <- hist(mydata$husband)
hist2 <- hist(mydata$wife)
plot(hist1, col=rgb(0,0,1,1/4), xlab="Age", main="Husband vs Wife Marriage Age in 1993")
plot(hist2, col=rgb(1,0,0,1/4), add=TRUE)
legend("topright", c("Husband", "Wife"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
dev.off()
