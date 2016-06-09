###################### PLOTS ######################
library(ggplot2)
library(RColorBrewer)
data13 <- read.csv("data/imputed_data.csv", stringsAsFactors=FALSE)
data13$Default.3Y <- data13$Default.3Y + 1 # for log transforms

# Default Rates Higher than Mortgage Default Rates
# https://research.stlouisfed.org/fred2/series/DRSFRMACBS
meandef <- mean(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # mean default rate
mddef <- median(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # median default rate
ggplot(data13, aes(Default.3Y)) + geom_histogram(stat="bin", binwidth=1, fill="lightblue") + theme_classic() + 
    geom_vline(xintercept=mddef, color="black", size=2) + 
    annotate("text", x=(mddef+2), y=130, hjust=0, color="black", label="Median Student Loan\nDefault Rate: 6.1%") +
    xlab("Default Rate (3 year cohort)") + labs(title="High Student Loan Default Rate the Norm") + ylab("Number of Colleges") +
    annotate("text", x=40, y=160, hjust=1, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov")

# Default Rate Higher at Public Schools
typedeft <- t.test(sqrt(Default.3Y) ~ Type, data13) # not equal. mean for private is 2.45 and public is 2.72 %
ggplot(data13, aes(Type, Default.3Y)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
    theme_classic() + ylab("Default Rate\n(3 year cohort)") + xlab("") + labs(title="Default Rate Higher at Public Schools") +
    annotate("text", y=.35, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typedeft$p.value, sep=""))


# Finish on Time to Avoid Loan Default
compdef <- lm(log(Default.3Y) ~ Completion, data13)
ggplot(data13, aes( Completion, log(Default.3Y) )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
    stat_smooth(method="lm", se=FALSE, color="black") + annotate("text", x=100, y=4, hjust=1, label="Default = 3.255 + Completion Rate * -0.0245") +
    theme_classic() + xlab("Completion Rate (6 years)") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="Finish on Time to Avoid Loan Default") +
    annotate("text", x=1, y=.25, hjust=0, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov")

# Completion Rate Lower at Public Schools
typecompt <- t.test(Completion ~ Type, data13) # not equal. mean for private is .56 and public is 0.48 (sqrt)
ggplot(data13, aes(Type, Completion)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
    theme_classic() + ylab("Completion Rate (6 years)") + xlab("") + labs(title="Completion Rate Lower at Public Schools") +
    annotate("text", y=.01, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typecompt$p.value, sep=""))


# Pay More, Owe Less?
fit <- lm(log(Default.3Y) ~ Avg.Cost + Type, data13)
res <- data.frame(school=data13[complete.cases(data13[,c(3,6,12), ]), 1], residuals=fit$residuals)
res <- res[order(res$residuals), ]
fitpub <- lm(log(Default.3Y) ~ Avg.Cost, data13[data13$Type=="Public", ]) # int 2.633 slope -.029
fitpriv <- lm(log(Default.3Y) ~ Avg.Cost, data13[data13$Type=="Private", ]) # int 3.124 slope -.0339

ggplot(data13, aes( Avg.Cost, log(Default.3Y) )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
    annotate("text", y=3.5, x=60, hjust=1, label="Default = 3.08 +\n-0.0329(Avg.Cost) +\n-0.3792(1 if public)(Avg.Cost)")+
    theme_classic() + xlab("Avg. Cost (Gross)") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="Higher Cost ≠ Higher Default Rate") +
    annotate("text", x=1, y=0.4, hjust=0, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov") +
    geom_abline(intercept=2.633, slope=-.029) + geom_abline(intercept=3.124, slope=-.0339)


