library(ggplot2)

## Cereals

mydata <- "
Cereal;Sugar(gm)
100% Bran;6
Cracklin’ Oat Bran;7
Crispy Wheat & Raisins;10
Frosted Mini-Wheats;7
Honey Graham Ohs;11
Lucky Charms;12
Nutri-grain Wheat;2
Shredded Wheat;0
Shredded Wheat spoon size;0
Total Raisin Bran;14
Cocoa Puffs;13
Crispix;3
Frosted Flakes;11
Fruitful Bran;12
Kix;3
Maypo;3
Rice Krispies;3
Shredded Wheat ’n’Bran;0
Smacks;15
Wheat Chex;3
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")
names(mydata) <- c("Cereal", "Sugar")
myfile <- "./data/chp03-sugar.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp03-sugar.rds"
mydata <- readRDS(file=myfile)

stem(mydata$Sugar, scale=2)

mean(mydata$Sugar)
median(mydata$Sugar)

## Activity 3-1: Supreme Court Service
mydata <- "
Supreme Court Justice;year;tenure
William Rehnquist;1972;22
John Paul Stevens;1975;19
Sandra Day O’Connor;1981;13
Antonin Scalia;1986;8
Anthony Kennedy;1988;6
David Souter;1990;4
Clarence Thomas;1991;3
Ruth Bader Ginsburg;1993;1
Stephen Breyer;1994;0
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")
myfile <- "./data/chp03-justice.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp03-justice.rds"
mydata <- readRDS(file=myfile)

stem(mydata$tenure)

mean(mydata$tenure)
median(mydata$tenure)

png("./images/chp03-plot1.png")
ggplot(mydata, aes(x=tenure)) + geom_dotplot() +
  labs(title="Supreme Court Justice (1994)", x="Tenure")
mydata
dev.off()

png("./images/chp03-plot2.png")
hist(mydata$tenure, xlab="Tenure", main="Supreme Court Justice (1994)")
dev.off()

with(mydata, {
  avg <- mean(tenure)
  sprintf("%d serve more than %.1f and %d serve less than %.1f.",
          length(mydata[tenure > avg, "tenure"]),
          avg,
          length(mydata[tenure < avg, "tenure"]),
          avg)
})

## Activity 3-7: Supreme Court Service (cont.)
mydata <- data.frame(tenure=c(5, 31, 32, 23, 21, 21, 19, 22,
                              6, 6, 23, 1, 4, 14, 6, 9, 20,
                              3, 5, 34, 13, 31, 20, 34, 32,
                              8, 14, 15, 4, 8, 19, 7, 3, 8,
                              30, 28, 23, 34, 10, 5, 15, 23,
                              18, 4, 6, 16, 4, 18, 6, 2, 26,
                              16, 36, 7, 24, 9, 18, 28, 28,
                              7, 16, 5, 7, 9, 16, 17, 1, 33,
                              15, 14, 20, 13, 10, 16, 5, 16,
                              24, 13, 22, 19, 34, 11, 26, 10,
                              11, 1, 33, 15, 15, 20, 27, 8, 5,
                              29, 26, 15, 12, 5, 14, 4, 2, 5,
                              10))

png("./images/chp03-plot3.png")
hist(mydata$tenure, xlab="Tenure", main="Supreme Court Justice (pre 1994)")
dev.off()

mean(mydata$tenure)
median(mydata$tenure)

## Activity 3-9: Consumer Price Index
mydata <- "
Expenditure category;Percent increase
food at home;40
food away from home;30
alchoholic beverages;39
housing;33
fuel and other utlilities;27
house furnishings;7
apparel products;14
new vehicles;25
motor fuel;32
airline fares;65
intracity public transportation;46
prescription drugs;69
entertainment products;28
tobacco and smoking products;71
personal care;29
college tuition;98
"

mydata <- read.table(header=TRUE, text=mydata, sep=";")
names(mydata) <- c("expenditure", "percent.inc")
myfile <- "./data/chp03-cpi.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp03-cpi.rds"
mydata <- readRDS(file=myfile)

stem(mydata$percent.inc, scale=2)

head(mydata[order(-mydata$percent.inc),], 3)
head(mydata[order(mydata$percent.inc),], 3)



mean(mydata$percent.inc)
median(mydata$percent.inc)
