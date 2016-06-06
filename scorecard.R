library(mice)
library(VIM)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(gridExtra)
setwd("/Users/kaylinwalker/R/college_scorecard/data/")
data10 <- read.csv("merged_2010_PP.csv", stringsAsFactors=FALSE)
data11 <- read.csv("merged_2011_PP.csv", stringsAsFactors=FALSE)
data12 <- read.csv("merged_2012_PP.csv", stringsAsFactors=FALSE)
data13 <- read.csv("merged_2013_PP.csv", stringsAsFactors=FALSE)
data10$Year <- 2010
data11$Year <- 2011
data12$Year <- 2012
data13$Year <- 2013
data0 <- rbind(data10, data11, data12, data13)

data <- data0[ , c(4, 6, 15, 16, 17, 19, 37, 291, 377, 384, 385, 386, 387, 429, 438, 440, 441, 
                   1380, 1432, 1710, 1730)] 

colnames(data) <- c("School", "State", "Predominant.Degree", "Highest.Degree", "Type", "Region", "Adm.Rate", "Enrollment",
                    "Avg.Cost", "Avg.Faculty.Pay", "Percent.FTE.Faculty", 
                    "Pell.Grant.Pct", "Completion.4Y", "Retention.FT.4Y",  "Pct.Fed.Loan", 
                    "Default.2Y", "Default.3Y", "Repayment.No.Default.5Y", 
                    "Avg.Fam.Income.Dependent", "Md.Grad.Debt.n30", "Year")

#write.csv(data, "merged_2010-2013.csv", row.names=FALSE)
data <- read.csv("merged_2010-2013.csv", stringsAsFactors=FALSE)

# format numbers and factors
data$Type <- gsub("1", "Public", data$Type)
data$Type <- gsub("2", "Private", data$Type)
data$Type <- gsub("3", "Private For-Profit", data$Type)
for (u in c(2:6, 21)) data[,u] <- as.factor(data[,u])
for (u in c(7:20)) data[,u] <- as.numeric(data[,u])


####### Predicting
# problem. lots of missing values in chosen variables.
# try mice package
# visualize missing data
missing <- data.frame(var=names(data2))
for(g in 1:21) { missing$NAs[g] <- sum(is.na(data2[,g])) }
ggplot(missing, aes(var, NAs)) + geom_bar(stat="identity") + theme_classic() + xlab("Variable") + ylab("Number Missing (out of 6080)") +
     theme(axis.text.x = element_text(angle=60, hjust=1))

# put default.2y and .3y vars together
for(r in seq_along(data2$Default.3Y)) { if(is.na(data2$Default.3Y[r])) data2$Default.3Y[r] <- data2$Default.2Y[r] }
data2$Default.2Y <- NULL


### MORE TO COME.


# TRAIN AND TEST (MORE RIGOROUS METHOD LATER)
# training (2011 and 2012) and test (2013) set
data3 <- data2[complete.cases(data2), ]
train <- data3[data3$Year!=2013, ]
test <- data3[data3$Year==2013, ]

#linear regression
fit <- lm(Default.3Y ~ ., train[, c(5, 7:16, 18, 19)])
step <- stepAIC(fit, direction="both")
fit2 <- lm(formula = Default.3Y ~ Adm.Rate + Avg.Cost + Avg.Faculty.Pay + 
                Percent.FTE.Faculty + Pell.Grant.Pct + Completion.4Y + Retention.FT.4Y + 
                Pct.Fed.Loan + Md.Grad.Debt.n30, data = train[, c(5, 7:16, 18, 19)])
test$predictions <- predict(fit2, test)
ggplot(test, aes(Default.3Y, predictions)) + geom_point() + ylim(c(0,.4)) + xlim(c(0, .4))
check <- lm(Default.3Y ~ predictions, test) # 64%




########### PLOTS




# keep schools with > 500 students who mostly grant Bachelors and drop for-profit private
data2 <- data[data$Predominant.Degree==3 & data$Type!="Private For-Profit" & data$Enrollment > 500, ] 
data2 <- data2[!is.na(data2$Type), ]

# Default Rates Higher than Mortgage Default Rates
# https://research.stlouisfed.org/fred2/series/DRSFRMACBS
meandef <- mean(data2[!is.na(data2$Default.3Y), ]$Default.3Y)  # mean default rate
mddef <- median(data2[!is.na(data2$Default.3Y), ]$Default.3Y)  # median default rate
ggplot(data2, aes(Default.3Y)) + geom_histogram(stat="bin", binwidth=.01, fill="lightblue") + theme_classic() + 
     geom_vline(xintercept=mddef, color="black", size=2) + 
     annotate("text", x=(mddef+.02), y=130, hjust=0, color="black", label="Median Student Loan\nDefault Rate: 6.7%") +
     xlab("Default Rate (3 year cohort)") + labs(title="High Student Loan Default Rate the Norm") + ylab("Number of Colleges") +
     annotate("text", x=.4, y=160, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov")

# Default Rate Higher at Public Schools
typedeft <- t.test(sqrt(Default.3Y) ~ Type, data2) # not equal. mean for private is .25 and public is 0.28 (sqrt)
ggplot(data2, aes(Type, Default.3Y)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
     theme_classic() + ylab("Default Rate\n(3 year cohort)") + xlab("") + labs(title="Default Rate Higher at Public Schools") +
     annotate("text", y=.35, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typedeft$p.value, sep=""))


# Finish on Time to Avoid Loan Default
compdef <- lm(Default.3Y ~ Completion.4Y, data2)
ggplot(data2, aes( Completion.4Y, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
     stat_smooth(method="lm", se=FALSE, color="black") + annotate("text", x=1, y=.35, hjust=1, label="Default = 0.1889 + Completion Rate * -0.2031") +
     theme_classic() + xlab("Completion Rate (6 years)") + scale_color_brewer(palette="Paired") +
     ylab("Default Rate\n(3 year cohort)") + labs(title="Finish on Time to Avoid Loan Default") +
     annotate("text", x=1, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov")

# Completion Rate Lower at Public Schools
typecompt <- t.test(Completion.4Y ~ Type, data2) # not equal. mean for private is .56 and public is 0.48 (sqrt)
ggplot(data2, aes(Type, Completion.4Y)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
     theme_classic() + ylab("Completion Rate (6 years)") + xlab("") + labs(title="Completion Rate Lower at Public Schools") +
     annotate("text", y=.01, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typecompt$p.value, sep=""))


# Pay More, Owe Less?
fit <- lm(Default.3Y ~ Avg.Cost + Type, data2)
res <- data.frame(school=data2[complete.cases(data2[,c(5,9,18), ]), 1], residuals=fit$residuals)
res <- res[order(res$residuals), ]
fitpub <- lm(Default.3Y ~ Avg.Cost, data2[data2$Type=="Public", ]) # int .01856 slope -.000004797
fitpriv <- lm(Default.3Y ~ Avg.Cost, data2[data2$Type=="Private", ]) # int .01876 slope -.000002914

ggplot(data2, aes( Avg.Cost, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
     annotate("text", y=.34, x=60000, hjust=1, label="Default = 0.1920 +\n-0.000003048(Avg.Cost) +\n-0.04137(1 if public)(Avg.Cost)")+
     theme_classic() + xlab("Avg. Cost (Gross)") + scale_color_brewer(palette="Paired") +
     ylab("Default Rate\n(3 year cohort)") + labs(title="Higher Cost ≠ Higher Default Rate") +
     annotate("text", x=60000, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") +
     geom_abline(intercept=0.1856, slope=-.000004797) + geom_abline(intercept=0.1876, slope=-.000002914)

# But, High Family Income Eases Default Rates
fit2 <- lm(Default.3Y ~ Avg.Fam.Income.Dependent, data2)
ggplot(data2, aes( Avg.Fam.Income.Dependent, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) + 
     stat_smooth(method="lm", se=FALSE,color="black") + annotate("text", y=.34, x=150000, hjust=1,
                                                                 label="Default = 0.2 +\n-0.00000164(Avg. Family Income)")+
     theme_classic() + xlab("Avg. Family Income - Dependent Students; 2014 Dollars") + scale_color_brewer(palette="Paired") +
     ylab("Default Rate\n(3 year cohort)") + labs(title="High Family Income Eases Default Rates") +
     annotate("text", x=150000, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") + ylim(c(0,.4))

#typeinc <- t.test(Avg.Fam.Income.Dependent ~ Type, data2) # not equal. private mean is 76K public mean is 66K
#ggplot(data2, aes(Type, Avg.Fam.Income.Dependent)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
     theme_classic() + ylab("Avg. Family Income") + xlab("") + labs(title="Average Family Income Lower at Public Schools") +
     annotate("text", y=.01, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typeinc$p.value, sep=""))

#ggplot(data2, aes( Avg.Fam.Income.Dependent, Avg.Cost )) + geom_point(aes(color=Type))

# Admission Rate not a Factor
fit3 <- lm(Default.3Y ~ (Adm.Rate)^2, data2)
ggplot(data2, aes( Adm.Rate, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) + 
     stat_smooth(method="lm", se=FALSE,color="black") + annotate("text", y=.34, x=1, hjust=1,
                                                                 label="Default = 0.064 +\n-0.017(Adm.Rate)\np=0.014")+
     theme_classic() + xlab("Admission Rate") + scale_color_brewer(palette="Paired") +
     ylab("Default Rate\n(3 year cohort)") + labs(title="Admission Rate Less Important") +
     annotate("text", x=1, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") + ylim(c(0,.4))

