library(ggplot2)

## Activity 4-2: Supreme Court Service (cont.)

myfile <- "./data/chp03-justice.rds"
mydata <- readRDS(file=myfile)

summary(mydata$tenure)

png("./images/chp04-plot1.png")
boxplot(mydata$tenure, main="Supreme Court Justice Tenure", xlab="Years")
dev.off()

sd(mydata$tenure)
with(mydata, {
  res <-(tenure-mean(tenure))^2
  res <- sum(res) / (length(tenure)-1)
  res <- sqrt(res)
  res
})

## Activity 4-5: Placement Exam Scores
scores <- 1:19
counts <- c(1,1,5,7,12,13,16,15,17,
            32,17,21,12,16,8,4,7,5,4)

mydata <- c()
for(i in 1:length(scores)) {
  mydata <- c(mydata, rep(scores[i], counts[i]))
}

stem(mydata)

png("./images/chp04-plot2.png")
mydata <- data.frame(score=mydata)
ggplot(mydata, aes(x=score)) + geom_dotplot() +
  labs(title="Exam Scores", x="Scores")
dev.off()

png("./images/chp04-plot3.png")
hist(mydata$score, xlab="Scores", main="Exam Scores")
dev.off()

quantile2 <- function(mydata, q) {
  with(mydata, {
    length(mydata[score <= quantile(score, q),])/length(score)
   })
}

quantile2(mydata, 0.25)
quantile2(mydata, 0.5)
quantile2(mydata, 0.75)

## The 68 - 95 - 99.7 rule
prob_within_mean <- function(mydata, num) {
  with(mydata, {
    mu <- mean(score)
    sigma <- num*sd(score)
    (length(mydata[score > (mu-sigma) &
                   score < (mu+sigma),])) / length(score)
  })
}

prob_within_mean(mydata, 1)
prob_within_mean(mydata, 2)
prob_within_mean(mydata, 3)

## Activity 4-17: Limitations of Boxplots

library(reshape2)
mydata <- data.frame(classA=c(50, 50, 50, 63, 70, 70, 70, 71,
                              71, 72, 72, 79, 91, 91, 92),
                     classB=c(50, 54, 59, 63, 65, 68, 69, 71,
                              73, 74, 76, 79, 83, 88, 92),
                     classC=c(50, 61, 62, 63, 63, 64, 66, 71,
                              77, 77, 77, 79, 80, 80, 92))
mydata <- melt(mydata, variable.name="class", value.name="score")

png("./images/chp04-plot4.png")
ggplot(mydata, aes(x=class, y=score)) + geom_dotplot(binaxis='y') + coord_flip()
dev.off()

png("./images/chp04-plot5.png")
boxplot(score ~ class, data=mydata)
dev.off()
