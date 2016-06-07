############################ MODEL BUILDING ############################

library(mice)
library(ggplot2)
library(lattice)
library(MASS)

setwd("/Users/kwalker/git_projects/college_scorecard")
data <- read.csv("data/merged_2010-2013.csv", stringsAsFactors=FALSE) # from 01_datacleansing.R

# format numbers and factors
data$Type <- gsub("1", "Public", data$Type)
data$Type <- gsub("2", "Private", data$Type)
for (u in c(c(2:4,6), 22)) data[,u] <- as.factor(data[,u])
for (u in c(7:21)) data[,u] <- as.numeric(gsub(",", "", data[,u]))

# keep schools with > 500 students who mostly grant Bachelors and drop for-profit private
data2 <- data[data$Predominant.Degree==3 & data$Type!=3 & data$Enrollment > 500, ] 
data2 <- data2[!is.na(data2$Type), c(1, 5, 7:22)]

############### DATA IMPUTATION ##############

# visualize missing data
missing <- data.frame(var=names(data2))
for(g in 1:18) { missing$NAs[g] <- sum(is.na(data2[,g])) }
missing$NA.rate <- round(missing$NAs / 6745, 3)
ggplot(missing, aes(var, NA.rate)) + geom_bar(stat="identity") + theme_classic() + xlab("Variable") + ylab("Percent Missing") +
     theme(axis.text.x = element_text(angle=30, hjust=1, size=12)) + labs(title="Missingness by Variable")

# manual solution: put default.2y and .3y vars together
for(r in seq_along(data2$Default.3Y)) { if(is.na(data2$Default.3Y[r])) data2$Default.3Y[r] <- data2$Default.2Y[r] }
data2$Default.2Y <- NULL

# use MICE package to impute missing data
#tempData <- mice(data2[,2:16], m=1, maxit=50, meth='pmm', seed=500)
#summary(tempData)
#densityplot(tempData)
#completed <- complete(tempData, 1)
#data3 <- cbind(data2[ ,c(17, 1)], completed)

# write.csv(data3, "data/imputed_data.csv", row.names=FALSE)
data3 <- read.csv("data/imputed_data.csv", stringsAsFactors=FALSE)

############### HOLD OUT A VALIDATION SET ##############

sample <- floor(0.80 * nrow(data3))
set.seed(555)
train_ind <- sample(seq_len(nrow(data3)), size = sample)
train <- data3[train_ind, 3:17]
test <- data3[-train_ind, 3:17]


############### LINEAR REGRESSION ##############
fit <- lm(sqrt(Default.3Y) ~ ., train) # leave out school, state, degree, region and year
fit2 <- stepAIC(fit, direction="both", trace=FALSE)
test$predictions <- predict(fit2, test)
errors <- sqrt(test$Default.3Y) - test$predictions
test.rsq <- 1 - (sum(errors^2) / sum((sqrt(test$Default.3Y)-mean(sqrt(test$Default.3Y)))^2))

ggplot(test, aes(sqrt(Default.3Y), predictions)) + geom_point() + ylim(c(0,.7)) + xlim(c(0,.7)) + theme_classic() +
    xlab("Default rate") + ylab("Predicted Value") + labs(title="Predicted Value vs. Actual Value, test set") + 
    geom_abline(slope=1, intercept=0, color="blue") + annotate("text", hjust=1, x=.6, y=.5, label=paste("R-squared:", round(test.rsq, 4)))


