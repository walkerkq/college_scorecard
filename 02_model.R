############################ MODEL BUILDING ############################

library(mice)
library(ggplot2)
library(lattice)
library(MASS)

setwd("/Users/kaylinwalker/R/college_scorecard")
data <- read.csv("data/merged_2010-2013.csv", stringsAsFactors=FALSE) # from 01_datacleansing.R

# Format data
data$Type <- gsub("1", "Public", data$Type)
data$Type <- gsub("2", "Private", data$Type)
for (u in c(c(2:4,6), 20)) data[,u] <- as.factor(data[,u])
for (u in c(7:19)) data[,u] <- as.numeric(gsub(",", "", data[,u]))

# Get data into similar scales of measurement  
for (u in c(7, 13:18)) data[,u] <- data[,u]*100 # change percents from decimals to whole #
for (u in c(8:12, 19)) data[,u] <- data[,u]/1000 # change dollars to thousands


# Subset: at least 500 students, public or non-profit private, primarly Bachelor-granting
data2 <- data[data$Predominant.Degree==3 & data$Type!=3 & data$Enrollment > .5, ] 
data2 <- data2[!is.na(data2$Type), c(1, 5, 7:20)]

# Check IVs for collinearity
collin <- round(cor(data2[,c(3:15)], use = "pair"), 2) 
# avg. fam income and pell grants are at 86%
# net revenue and average cost at 85%
# completion and retention at 80%
data2 <- data2[,-c(6, 11, 15)] # drop family income, net revenue and retention

############### DATA IMPUTATION ##############

# check for and visualize missing data
missing <- data.frame(var=names(data2))
for(g in 1:13) { missing$NAs[g] <- sum(is.na(data2[,g])) }
missing$NA.rate <- round(missing$NAs / 6780, 3)
ggplot(missing, aes(var, NA.rate)) + geom_bar(stat="identity") + theme_classic() + xlab("Variable") + ylab("Percent Missing") +
     theme(axis.text.x = element_text(angle=30, hjust=1, size=12)) + labs(title="Missingness by Variable")

# manual solution: put default.2y and .3y vars together
for(r in seq_along(data2$Default.3Y)) { if(is.na(data2$Default.3Y[r])) data2$Default.3Y[r] <- data2$Default.2Y[r] }
data2$Default.2Y <- NULL

# use MICE package to impute missing data
#tempData <- mice(data2[,2:11], m=1, maxit=50, meth='pmm', seed=500)
#summary(tempData)
#densityplot(tempData)
#completed <- complete(tempData, 1)
#data3 <- cbind(data2[ ,c(12, 1)], completed)

# write.csv(data3, "data/imputed_data.csv", row.names=FALSE)
data3 <- read.csv("data/imputed_data.csv", stringsAsFactors=FALSE)

############### CHECK NORMALITY/TRANSFORM ##############

ggplot(data3, aes(Default.3Y)) + geom_histogram(stat="bin", binwidth=1) + theme_classic() + labs(title="Default Rate")
ggplot(data3, aes(log(Default.3Y))) + geom_histogram(stat="bin", binwidth=.2) + theme_classic() + labs(title="Log of Default Rate")

data3$Default.3Y <- data3$Default.3Y + 1 # decide to do a log transform, but need to add 1 across the board to account for a few 0s.

############### HOLD OUT A VALIDATION SET ##############

sample <- floor(0.80 * nrow(data3))
set.seed(555)
train_ind <- sample(seq_len(nrow(data3)), size = sample)
train <- data3[train_ind, 3:12]
test <- data3[-train_ind, 3:12]

############### LINEAR REGRESSION ##############

fit <- lm(log(Default.3Y) ~ ., train) 
fit2 <- stepAIC(fit, direction="both", trace=FALSE) #0.59 R-sq, adj
# try keeping only the largest influences 
fitSmall <- lm(log(Default.3Y) ~ Completion + Pell.Grant.Pct, train) #0.575 R-sq, adj
# use the smallest model. adding Type only added 0.002% adj R-sq
test$predictions <- predict(fitSmall, test)
errors <- log(test$Default.3Y) - test$predictions
test.rsq <- 1 - (sum(errors^2) / sum((log(test$Default.3Y)-mean(log(test$Default.3Y)))^2))

ggplot(test, aes(log(Default.3Y), predictions)) + geom_point()  + theme_classic() + ylim(c(0,4)) + xlim(c(0,4)) +
    xlab("Log(Default rate)") + ylab("Predicted Value") + labs(title="Predicted Value vs. Actual Value, test set") + 
    geom_abline(slope=1, intercept=0, color="blue") + annotate("text", hjust=1, x=5, y=1, label=paste("R-squared:", round(test.rsq, 4)))


############################ INTERPRET ############################

results <- data.frame(coef=names(fitSmall$coefficients), log.estimate=fitSmall$coefficients)
results$exp.estimate <- as.numeric(exp(results$log.estimate))
results$exp.estimate.minus1 <- results$exp.estimate - 1
for(g in c(2,3,4)) results[,g] <- format(results[,g], scientific=F)
for(g in c(2,3,4)) results[,g]<- round(as.numeric(results[,g]), 7)

results <- results[order(-results$exp.estimate), ]
