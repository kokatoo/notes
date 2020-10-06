myvars <- c("mpg", "hp", "wt")
head(mtcars[myvars])
dim(mtcars)
nrow(mtcars)

summary(mtcars[myvars])
sapply(mtcars[myvars], mean)
sapply(mtcars[myvars], fivenum)

Hmisc::describe(mtcars[myvars])
psych::describe(mtcars[myvars])

pastecs::stat.desc(mtcars[myvars], basic = TRUE, desc = FALSE)
pastecs::stat.desc(mtcars[myvars], basic = FALSE, desc = TRUE)
pastecs::stat.desc(mtcars[myvars], basic = FALSE, desc = FALSE, norm = TRUE)

aggregate(mtcars[myvars], by = list(am = mtcars$am), mean)

by(mtcars[myvars], mtcars$am, summary)
by(mtcars[myvars], mtcars$am, function(x) sapply(x, fivenum))

doBy::summaryBy(mpg ~ am + cyl, data = mtcars, FUN = summary)

psych::describeBy(mtcars[myvars], list(am = mtcars$am, cyl = mtcars$cyl))

mytable <- with(vcd::Arthritis, table(Improved))
mytable <- with(vcd::Arthritis, table(Treatment, Improved))
margin.table(mytable, 1)
margin.table(mytable, 2)

prop.table(mytable)
prop.table(mytable, 1)
prop.table(mytable, 2)

mytable <- with(vcd::Arthritis, table(Treatment, Sex, Improved))
ftable(mytable)
ftable(prop.table(mytable, 1))
ftable(prop.table(mytable, c(1, 3)))

mytable <- with(vcd::Arthritis, table(Treatment, Improved))
chisq.test(mytable)
fisher.test(mytable)

mytable <- with(vcd::Arthritis, table(Treatment, Improved, Sex))
mantelhaen.test(mytable)
vcd::assocstats(mytable)

states <- state.x77[, 1:6]
x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)

colnames(states)
cor(states[, 1], states[, 5])
ggm::pcor(c(1, 5, 2, 3, 6), cov(states))
ppcor::pcor.test(
  states[, 1],
  states[, 5],
  states[, c(2, 3, 6)]
)

cor.test(states[, 1], states[, 5])
psych::corr.test(states)

t.test(Prob ~ So, data = MASS::UScrime)
wilcox.test(Prob ~ So, data = MASS::UScrime)

with(MASS::UScrime, t.test(U1, U2))
with(MASS::UScrime, t.test(U1, U2, paired = TRUE))
with(MASS::UScrime, wilcox.test(U1, U2, paired = TRUE))

states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data = states)

source("http://www.statmethods.net/RiA/wmc.txt")
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data = states, method = "holm")
