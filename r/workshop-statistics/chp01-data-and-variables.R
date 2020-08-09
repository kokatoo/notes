library(ggplot2)

## Activity 1-1 Type of Variables
mydata <- "
  year brand
  1992 Honda
  1994 Oldsmobile
  1967 Chevrolet
  1985 Nissan
  1994 Toyota
  1987 Ford
  1968 Chevrolet
  1980 Ford
  1989 Pontiac
  1993 Toyota
  1991 Chevrolet
  1988 Oldsmobile
  1990 Volkswagon
  1984 Mercury
  1994 Ford
  1989 Ford
  1992 Ford
  1986 Volkswagon
  1995 Buick
  1977 Jeep
  "
mydata <- read.table(header=TRUE, text=mydata)

myfile <- "./data/chp01-cars.rds"
saveRDS(mydata, file=myfile)
mydata <- readRDS(myfile)
mytable <- table(mydata$brand)

mydata <- as.data.frame(cbind(addmargins(mytable), addmargins(prop.table(mytable))))
names(mydata) <- c("Count", "Proportion")

barplot(mytable, main="Used Car Ads",
        ylab="Num Ads", cex.names=.9, las=2)

## Activity 1-3: Value of Statistics
mydata <- "
City;Rate
New York City;8.3
Los Angeles;10.0
Chicago;5.6
Houston;6.9
Philadelphia;6.5
San Diego;8.3
Dallas;5.6
Phoenix;4.9
Detroit;6.8
San Antonio;5.6
San Jose;7.1
Indianapolis;4.4
San Francisco;6.5
Baltimore;6.3
Jacksonville;5.1
Columbus;4.1
Milwaukee;4.5
Memphis;4.4
Washington, D.C.;4.2
Boston;5.3
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")

myfile <- "./data/chp01-unemployment.rds"
saveRDS(mydata, file=myfile)
mydata <- readRDS(myfile)

mydata <- mydata[order(mydata$Rate),]

ggplot(mydata, aes(x=Rate)) + geom_dotplot() +
  xlim(2, 12)
labs(title="Umemployment rate") +
  theme(plot.title=element_text(hjust=0.5))


## Activity 1-5: Women Employed
mydata <- "
Occupation;Male;Female;Total
Architect;131;32;163
Engineer;1772;162;1934
Math/Computer Science;813;382;1195
Natural Science;377;142;519
Physician;524;169;693
Registered Nurse;136;1841;1977
Teacher - PK, K;9;489;498
Teacher - Elem.;276;1462;1738
Teacher - Second.;530;702;1232
Lawyer;658;236;894
Musician;101;60;161
Photographer;99;37;136
Barber;73;14;87
Hairdresser;60;690;750
Social Worker;233;494;727
Librarian;31;164;195
"

myfile <- "./data/chp01-women.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp01-women.rds"
mydata <- readRDS(myfile)
mydata <- transform(mydata, WomenRatio=Female/(Male+Female))

mydata <- mydata[order(-mydata$WomenRatio),]
head(mydata$Occupation, n=3)
tail(mydata$Occupation, n=3)
mean(mydata$WomenRatio)

png("./images/chp01-plot3.png")
ggplot(mydata, aes(x=WomenRatio)) + geom_dotplot() +
  labs(title="% Women vs Men Employed", x="% in occupation") +
  theme(plot.title=element_text(hjust=0.5))
dev.off()

## Activity 1-12: Hazardousness of Sports
mydata <- "
sport;injuries;participants
Basketball;647;26,200
Bicycle riding;601;54,000
Baseball, softball;460;36,100
Football;454;13,300
Soccer;150;10,000
Swimming;130;66,200
Volleyball;130;22,600
Roller skating;113;26,500
Weightlifting;86;39,200
Fishing;84;47,000
Horseback riding;71;10,100
Skateboarding;56;8,000
Ice hockey;55;1,800
Golf;39;24,700
Tennis;30;16,700
Ice skating;29;7,900
Water skiing;27;9,000
Bowling;25;40,400
"

mydata <- read.table(header=TRUE, text=mydata, sep=";")

myfile <- "./data/chp01-sports.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp01-sports.rds"
mydata <- readRDS(myfile)
mydata <- transform(mydata, participants=as.numeric(gsub(",", "", participants)))

cmp <- function(mydata, sportname1, sportname2, colname) {
  with(mydata, {
    sprintf("%s vs %s %s: %d vs %d",
            sportname1, sportname2, colname,
            mydata[sport==sportname1, colname],
            mydata[sport==sportname2, colname])
  })
}

cmp(mydata, "Bicycle riding", "Football", "injuries")
cmp(mydata, "Soccer", "Ice hockey", "injuries")
cmp(mydata, "Swimming", "Skateboarding", "injuries")

mydata <- transform(mydata, per.thousand=round(injuries/participants*1000))

cmp(mydata, "Bicycle riding", "Football", "per.thousand")
cmp(mydata, "Soccer", "Ice hockey", "per.thousand")
cmp(mydata, "Swimming", "Skateboarding", "per.thousand")

head(mydata[order(mydata$per.thousand), c("sport", "per.thousand")], 1)
head(mydata[order(-mydata$per.thousand), c("sport", "per.thousand")], 1)
