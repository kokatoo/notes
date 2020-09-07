experimental <- c(1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2)
control <- c(2, 2, 1, 2, 2, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2)

data <- rbind(
  data.frame(type = rep("experimental", length(experimental)), complied = experimental),
  data.frame(type = rep("control", length(control)), complied = control)
)

data$complied <- ifelse(data$complied == 1, "No", "Yes")
data$complied <- factor(data$complied, levels = c("Yes", "No"))
data$type <- factor(data$type, levels = c("experimental", "control"))

options(digits = 3)

mytable <- table(data$complied, data$type)
mytable

prop.table(mytable, 1)
prop.table(mytable, 2)

infractions <- c(
  "Drug abuse at work",
  "Drug abuse off work",
  "Violating a prior Board agreement",
  "Working with lapsed license",
  "Care errors",
  "Medication errors",
  "Providing care without physician order",
  "Documentation errors",
  "All other infractions"
)
males <- c(36, 15, 9, 2, 4, 8, 2, 8, 35)
females <- c(117, 63, 46, 36, 32, 26, 26, 19, 143)

mytable <- as.matrix(cbind(males, females))
dimnames(mytable) <- list(infractions, c("males", "females"))
mytable <- as.table(mytable)
mytable

row_percent <- prop.table(mytable, 1)
row_percent[row_percent[, "males"] < 0.075, ]

max_idx <- which.max(row_percent[, "males"])
names(max_idx)
row_percent[max_idx, ]

mytable <- matrix(
  c(15, 5, 35, 45),
  nrow = 2,
  ncol = 2,
  dimnames = list(c("Control", "Experimental"), c("Ulcer", "NoUlcer"))
)
mytable <- as.table(mytable)

prop.table(mytable, 1)

## Absolute Risk (Exposure):
prop.table(mytable, 1)["Control", "Ulcer"]

## Absolute Risk (Non-Exposure):
prop.table(mytable, 1)["Experimental", "Ulcer"]

## Absolute Risk Reduction
arr <- prop.table(mytable, 1)["Control", "Ulcer"] - prop.table(mytable, 1)["Experimental", "Ulcer"]
arr

## Relative Risk
prop.table(mytable, 1)["Control", "Ulcer"] / prop.table(mytable, 1)["Experimental", "Ulcer"]

## Relative Risk Reduction
arr / prop.table(mytable, 1)["Experimental", "Ulcer"]

## Odds Ratio
odds_exposed <- mytable["Control", "Ulcer"] / mytable["Control", "NoUlcer"]
odds_nonexposed <- mytable["Experimental", "Ulcer"] / mytable["Experimental", "NoUlcer"]
odds_ratio <- odds_exposed / odds_nonexposed
odds_ratio

## Numbers to Treat
1 / arr

diastolic <- c(90, 80, 90, 78, 76, 78, 80, 70, 76, 74)
systolic <- c(130, 126, 140, 118, 114, 112, 120, 110, 114, 116)

png(file = "./images/chp03-plot1.png")
plot(diastolic, systolic)
dev.off()

cor(diastolic, systolic)

