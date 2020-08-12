## ---- Introduction

ohio_temps <- c(31, 20, 65, 38, 36, 29, 53, 57, 35, 39)
ohio_temps <- scale(ohio_temps)
ohio_temps

nc_temps <- c(50, 51, 79, 54, 57, 46, 70, 66, 55, 65)
nc_temps <- scale(nc_temps)
nc_temps

corr <- sum(ohio_temps * nc_temps) / (length(ohio_temps) - 1)
corr

## ---- Activity 7-2: Televisions and Life Expectancy

mydata <- "
country;life exp;per TV
Angola;44;200
Australia;76.5;2
Cambodia;49.5;177
Canada;76.5;1.7
China;70;8
Egypt;60.5;15
France;78;2.6
Haiti;53.5;234
Iraq;67;18
Japan;79;1.8
Madagascar;52.5;92
Mexico;72;6.6
Morocco;64.5;21
Pakistan;56.5;73
Russia;69;3.2
South Africa;64;11
Sri Lanka;71.5;28
Uganda;51;191
United Kingdom;76;3
United States;75.5;1.3
Vietnam;65;29
Yemen;50;38
"
mydata <- read.table(header = TRUE, text = mydata, sep = ";")

myfile <- "./data/chp07-life.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp07-life.rds"
mydata <- readRDS(myfile)

mydata[order(-mydata$"per.TV"), ][1, 1]
mydata[order(mydata$"per.TV"), ][1, 1]

png(file = "./images/chp07-plot1.png")
plot(mydata$per.TV, mydata$life.exp,
     main = "Life Expectancy vs People Per TV",
     xlab = "People Per TV", ylab = "Life Expectancy")
dev.off()

cor(mydata$per.TV, mydata$life.exp)

## ---- Activity 7-3: High School Completion Rates
mydata <- "
State;Completion.Rate;Distance
Alabama;83.3;940
Arizona;83.7;1060
Arkansas;87.5;890
California;78.9;720
Colorado;87.6;610
Connecticut;92.6;220
Delaware;93.7;440
Florida;83.2;830
Georgia;79.4;560
Idaho;86.7;330
Illinois;86.7;560
Indiana;88.4;500
Iowa;94.2;440
Kansas;92.2;670
Kentucky;83.3;560
Louisiana;83.9;1220
Maine;94.0;220
Maryland;92.9;440
Massachusetts;91.2;330
Michigan;89.2;280
Minnesota;93.2;220
Mississippi;88.8;1110
Missouri;90.0;670
Montana;91.6;170
Nebraska;95.9;560
Nevada;83.4;670
New Hampshire;86.6;280
New Jersey;91.0;330
New Mexico;83.7;890
New York;87.5;170
North Carolina;85.3;610
North Dakota;96.6;170
Ohio;89.6;170
Oklahoma;83.1;890
Oregon;82.9;280
Pennsylvania;89.7;330
Rhode Island;90.7;390
South Carolina;87.0;670
South Dakota;93.2;330
Tennessee;82.3;720
Texas;80.5;1280
Utah;93.9;560
Vermont;89.8;60
Virginia;88.6;500
Washington;87.3;110
West Virginia;85.6;280
Wisconsin;93.4;330
Wyoming;91.6;560
"
mydata <- read.table(header = TRUE, text = mydata, sep = ";")

myfile <- "./data/chp07-school.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp07-school.rds"
mydata <- readRDS(myfile)

png(file = "./images/chp07-plot2.png")
plot(mydata$Distance, mydata$Completion.Rate,
  main = "High School Rate vs Distance",
  xlab = "Distance from Canada",
  ylab = "High School Completion Rate"
)
dev.off()

cor(mydata$Completion.Rate, mydata$Distance)

## ---- Activity 7-11: Climatic Conditions
mydata <- "
city;Jan hi;Jan lo;July hi;July lo;precip;precday;snow;sun
Atlanta;50.4;31.5;88;69.5;50.77;115;2;61
Baltimore;40.2;23.4;87.2;66.8;40.76;113;21.3;57
Boston;35.7;21.6;81.8;65.1;41.51;126;40.7;58
Chicago;29;12.9;83.7;62.6;35.82;126;38.7;55
Cleveland;31.9;17.6;82.4;61.4;36.63;156;54.3;49
Dallas;54.1;32.7;96.5;74.1;33.7;78;2.9;64
Denver;43.2;16.1;88.2;58.6;15.4;89;59.8;70
Detroit;30.3;15.6;83.3;61.3;32.62;135;41.5;53
Houston;61;39.7;92.7;72.4;46.07;104;0.4;56
Kansas City;34.7;16.7;88.7;68.2;37.62;104;20;62
Los Angeles;65.7;47.8;75.3;62.8;12.01;35;0;73
Miami;75.2;59.2;89;76.2;55.91;129;0;73
Minneapolis;20.7;2.8;84;63.1;28.32;114;49.2;58
Nashville;45.9;26.5;89.5;68.9;47.3;119;10.6;56
New Orleans;60.8;41.8;90.6;73.1;61.88;114;0.2;60
New York;37.6;25.3;85.2;68.4;47.25;121;28.4;58
Philadelphia;37.9;22.8;82.6;67.2;41.41;117;21.3;56
Phoenix;65.9;41.2;105.9;81;7.66;36;0;86
Pittsburgh;33.7;18.5;82.6;61.6;36.85;154;42.8;46
St. Louis;37.7;20.8;89.3;70.4;37.51;111;19.9;57
Salt Lake City;36.4;19.3;92.2;63.7;16.18;90;57.8;66
San Diego;65.9;48.9;76.2;65.7;9.9;42;0;68
San Francisco;55.6;41.8;71.6;65.7;19.7;62;0;66
Seattle;45;35.2;75.2;55.2;37.19;156;12.3;46
Washington;42.3;26.8;88.5;71.4;38.63;112;17.1;56
"
mydata <- read.table(header = TRUE, text = mydata, sep = ";")

myfile <- "./data/chp07-temperature.rds"
saveRDS(mydata, file = myfile)

myfile <- "./data/chp07-temperature.rds"
mydata <- readRDS(myfile)

cors <- cor(mydata[-1])
options(digits = 2)
cors

tmp <- cors
diag(tmp) <- 0
tmp

idxs <- which(tmp == max(tmp), arr.ind = TRUE)
row.names(idxs)
cors[idxs]

idxs <- which(tmp == min(tmp), arr.ind = TRUE)
row.names(idxs)
cors[idxs]

tmp <- abs(cors)
idxs <- which(tmp == min(tmp), arr.ind = TRUE)
row.names(idxs)
cors[idxs]



