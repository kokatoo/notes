##---- Ohio vs North Carolina
mydata <- "
Day;Ohio;NorthCarolina
January 18;31;50
February 3;20;51
October 23;65;79
December 8;38;54
December 26;36;57
January 19;29;46
February 23;53;70
November 7;57;66
December 10;35;55
December 28;39;65
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")

myfile <- "./data/chp06-weather.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp06-weather.rds"
mydata <- readRDS(myfile)

png("./images/chp06-plot1.png")
plot(mydata$Ohio, mydata$NorthCarolina,
     xlab="Ohio", ylab="North Carolina",
     main="North Carolina vs Ohio Temperature (F)")
dev.off()
##-----

##---- A second example
mydata <- "
Car Displacement Mileage
Cavalier 2189 25
Neon 1996 29
Summit 1468 33
Aspire 1324 38
Escort 1856 26
Metro 993 43
Prizm 1587 27
Civic 1590 33
Accent 1495 29
Protege 1489 31
Mirage 1468 32
Sentra 1597 30
Sunfire 2189 24
Saturn 1901 28
Impreza 1820 24
Esteem 1590 31
Swift 1298 39
Corolla 1587 28
Tercel 1497 31
"
mydata <- read.table(header=TRUE, text=mydata)

myfile <- "./data/chp06-cars.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp06-cars.rds"
mydata <- readRDS(myfile)

png("./images/chp06-plot1.png")
plot(mydata$Displacement, mydata$Mileage,
     xlab="Displacement", ylab="Mileage",
     main="Mileage vs Displacement")
text(Mileage ~ Displacement, labels=Car,
     data=mydata, cex=0.7, pos=3)
dev.off()

##---- Activity 6-1: Cars’ Fuel Efficiency (cont.)
mydata <- "
model;weight;mpg
BMW 3-Series;3250;28
BMW 5-Series;3675;23
Cadillac Eldorado;3840;19
Cadillac Seville;3935;20
Ford Aspire;2140;43
Ford Crown Victoria;4010;22
Ford Escort;2565;34
Ford Mustang;3450;22
Ford Probe;2900;28
Ford Taurus;3345;25
Ford Taurus SHO;3545;24
Honda Accord;3050;31
Honda Civic;2540;34
Honda Civic del Sol;2410;36
Honda Prelude;2865;30
Lincoln Mark VIII;3810;22
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")

myfile <- "./data/chp06-mpg.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp06-mpg.rds"
mydata <- readRDS(myfile)

png("./images/chp06-plot2.png")
plot(mydata$weight, mydata$mpg,
     xlab="weight", ylab="mpg",
     main="MPG vs Weight")
dev.off()

##---- Activity 6-5: Space Shuttle O-Ring Failures
mydata <- "
FlightDate O-ringFailures Temperature
4/12/81 0 66
11/12/81 1 70
3/22/82 0 69
11/11/82 0 68
4/4/83 0 67
6/18/83 0 72
8/30/83 0 73
11/28/83 0 70
2/3/84 1 57
4/6/84 1 63
8/30/84 1 70
10/5/84 0 78
11/8/84 0 67
1/24/85 3 53
4/12/85 0 67
4/29/85 0 75
6/17/85 0 70
7/29/85 0 81
8/27/85 0 76
10/3/85 0 79
10/30/85 2 75
11/26/85 0 76
1/12/86 1 58
"
mydata <- read.table(header=TRUE, text=mydata)

myfile <- "./data/chp06-oring.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp06-oring.rds"
mydata <- readRDS(myfile)

names(mydata) <- c("date", "failures", "temp")

png("./images/chp06-plot3.png")
plot(mydata$temp, mydata$failures,
     xlab="Temperature", ylab="O'Ring Failures",
     main="O'Ring Failures vs Temperature")
dev.off()

