## Comparing Groups Using Boxplots

mylist <- list(RubberSoul=c(150, 126, 202, 165, 139, 164, 162,
                                  171, 154, 147, 148, 137, 144, 139),
                     SergeantPepper=c(119, 166, 205, 167, 153, 204,
                                      156, 303, 158, 163, 155, 80, 303),
                     TheWhiteAlbum=c(165, 240, 130, 190, 62, 185, 286,
                                     167, 148, 121, 140, 124, 213, 232,
                                     102, 106, 177, 160, 241, 166, 145,
                                     195, 270, 188, 253, 162, 175, 191,
                                     495, 194))

mydata <- melt(mylist)
names(mydata) <- c("duration", "album")

png("./images/chp05-plot1.png")
boxplot(duration ~ album, data=mydata,
        horizontal=TRUE, main="Beatles Album")
dev.off()

## Activity 5-2: Professional Golfers’ Winnings

mydata <- "
tournament golfer winnings
PGA Norman 1165
PGA Levi 1024
PGA Stewart 976
PGA Azinger 944
PGA Mudd 911
PGA Irwin 838
PGA Calcavecchia 834
PGA Simpson 809
PGA Couples 757
PGA O’Meara 707
PGA Morgan 702
PGA Mayfair 693
PGA Wadkins 673
PGA Mize 668
PGA Kite 658
PGA Baker-Finch 611
PGA Beck 571
PGA Elkington 548
PGA Jacobsen 547
PGA Love 537
PGA Grady 527
PGA Price 520
PGA Tway 495
PGA Roberts 478
PGA Gallagher 476
PGA Pavin 468
PGA Gamez 461
PGA Cook 448
PGA Tennyson 443
PGA Huston 435
LPGA Daniel 863
LPGA Sheehan 732
LPGA King 543
LPGA Gerring 487
LPGA Bradley 480
LPGA Jones 353
LPGA Okamoto 302
LPGA Lopez 301
LPGA Ammacapane 300
LPGA Rarick 259
LPGA Coe 240
LPGA Mochrie 231
LPGA Walker 225
LPGA Johnson 187
LPGA Richard 186
LPGA Geddes 181
LPGA Keggi 180
LPGA Crosby 169
LPGA Massey 166
LPGA Figg-Currier 157
LPGA Johnston 156
LPGA Green 155
LPGA Mucha 149
LPGA Eggeling 147
LPGA Rizzo 145
LPGA Brown 140
LPGA Mallon 129
LPGA Hammel 128
LPGA Benz 128
LPGA Turner 122
Seniors Trevino 1190
Seniors Hill 895
Seniors Coody 762
Seniors Archer 749
Seniors Rodriguez 729
Seniors Dent 693
Seniors Charles 584
Seniors Douglass 568
Seniors Player 507
Seniors McBee 480
Seniors Crampton 464
Seniors Henning 409
Seniors Geiberger 373
Seniors Hill 354
Seniors Nicklaus 340
Seniors Beard 327
Seniors Mowry 314
Seniors Thompson 308
Seniors Dill 278
Seniors Zembriski 276
Seniors Barber 274
Seniors Moody 273
Seniors Bies 265
Seniors Kelley 263
Seniors Jimenez 246
Seniors Shaw 235
Seniors Massengale 229
Seniors January 216
Seniors Cain 208
Seniors Powell 208
"

mydata <- read.table(header=TRUE, text=mydata)

myfile <- "./data/chp05-golf.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp05-golf.rds"
mydata <- readRDS(myfile)

png("./images/chp05-plot2.png")
boxplot(winnings ~ tournament, data=mydata, main="Golf Tournaments")
dev.off()

## Activity 5-7: Automobile Theft Rates

mydata <- "
region;state;theft
East;Alabama West;348
East;Connecticut;731
East;Delaware;444
East;Florida;826
East;Georgia;674
East;Illinois;643
East;Indiana;439
East;Kentucky;199
East;Maine;177
East;Maryland;709
East;Massachusetts;924
East;Michigan;714
East;Mississippi;208
East;New Hampshire;244
East;New Jersey;940
East;New York;1043
East;North Carolina;284
East;Ohio;491
East;Pennsylvania;506
East;Rhode Island;954
East;South Carolina;386
East;Tennessee;572
East;Vermont;208
East;Virginia;327
East;West Virginia;154
East;Wisconsin;416
West;Alaska;565
West;Arizona;863
West;Arkansas;289
West;California;1016
West;Colorado;428
West;Hawaii;381
West;Idaho;165
West;Iowa;170
West;Kansas;335
West;Louisiana;602
West;Minnesota;366
West;Missouri;539
West;Montana;243
West;Nebraska;178
West;Nevada;593
West;New Mexico;337
West;North Dakota;133
West;Oklahoma;602
West;Oregon;459
West;South Dakota;110
West;Texas;909
West;Utah;238
West;Washington;447
West;Wyoming;149
"
mydata <- read.table(header=TRUE, text=mydata, sep=";")
myfile <- "./data/chp05-theft.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp05-theft.rds"
mydata <- readRDS(myfile)

png("./images/chp05-plot3.png")
boxplot(theft ~ region, data=mydata,
        main="US Theft Rate Per 100,000 (1990)")
dev.off()

## Activity 5-15: Mutual Funds’ Returns

mylist <- list(NoLoad=c(13.9, 17.8, 21.9, 12.4, 22.1, 15.4, 13.9, 12.5, 9, 9.1,
                        23.3, 36.7, 13.1, 13.1, 5.2, 26.3, 81.8, 13.9, 6.7, 13.1,
                        19.3, 21.3, 11.2, 13.2, 14, 13.8, 18.9, 35.1, 12.9, 19.1,
                        13.6, 9.8, 16.2, 12.8, 10.2, 25.5, 6.5, 20.5, 12.6, 15.6,
                        25.9, 18.4, 12.2, 21.4, 22.9, 8.1, 5.5, 13.8, 12.5, 36.5),
               Load=c(24.5, 19.5, 33.4, 20.2, 21.4, 24.7, 26.4, 24.7, 26.8, 8.3,
                      19.9, 40.1, 27.2, 63.9, 16.2, 21.1))

mydata <- melt(mylist)
names(mydata) <- c("return", "type")

png("./images/chp05-plot4.png")
boxplot(return ~ type, data=mydata, notch=TRUE,
        main="Mutual Funds % Return (1993)")
dev.off()

##
mydata <- "
season1 season2 season3 season4 season5 season6 season7
50 103 113 8 5 23 146
51 90 143 153 16 68 157
101 104 151 25 109 57 78
164 70 91 77 140 127 121
173 105 92 120 76 81 97
148 62 95 108 6 18 61
163 48 160 38 20 3 168
149 161 67 69 42 13 80
175 31 100 112 135 122 128
40 4 87 167 60 64 83
84 172 33 118 39 36 14
139 94 123 82 73 116 58
125 176 12 166 53 117 141
34 75 35 115 147 59 177
162 132 37 144 47 43 124
145 156 88 129 26 49 110
86 52 98 65 7 29 56
155 66 133 137 71 107 134
85 165 72 46 138 32 54
63 99 119 171 111 158 136
150 106 19 21 152 55 74
169 45 89 30 24 11 93
44 178 102 28 79  174
41 126 9 96 15  114
142 159  170 130  1
   17 131  2
   22 27  
    154  
    10  
"
mydata <- read.table(header=TRUE, text=mydata, sep=" ")

myfile <- "./data/chp05-star-trek.rds"
saveRDS(mydata, file=myfile)

myfile <- "./data/chp05-star-trek.rds"
mydata <- readRDS(myfile)
mydata <- melt(mydata)
names(mydata) <- c("season", "rank")
mydata <- mydata[!is.na(mydata$rank),]

png("./images/chp05-plot5.png")
boxplot(rank ~ season, data=mydata,
        horizontal=TRUE, cex.axis=0.7,
        xlab="Ranking", ylab="Season",
        main="Star Trek Episode Ranking")
dev.off()
