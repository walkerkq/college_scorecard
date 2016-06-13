###################### PLOTS ######################
library(ggplot2)
library(RColorBrewer)
data13 <- read.csv("data/imputed_data.csv", stringsAsFactors=FALSE)
data13$Default.3Y <- data13$Default.3Y + 1 # for log transforms

# Default Rates Higher than Mortgage Default Rates
# https://research.stlouisfed.org/fred2/series/DRSFRMACBS
meandef <- mean(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # mean default rate
mddef <- median(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # median default rate
ggplot(data13, aes(Default.3Y)) + geom_histogram(stat="bin", binwidth=1, fill="cadetblue") + theme_classic() + 
    geom_vline(xintercept=mddef, color="black", size=2) + 
    annotate("text", x=(mddef+1.5), y=130, hjust=0, color="black", label="Median Student Loan\nDefault Rate: 6.1%") +
    xlab("Default Rate (3 year cohort)") + labs(title="High Student Loan Default Rate the Norm") + ylab("Number of Colleges") +
    annotate("text", x=40, y=160, hjust=1, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov")

# Finish on Time 
compdef <- lm(log(Default.3Y) ~ Completion, data13)
ggplot(data13, aes( Completion, log(Default.3Y) )) + geom_point(color="cadetblue", size = 2.5, alpha=.75) +
    stat_smooth(method="lm", se=FALSE, color="black") + annotate("text", x=100, y=4, hjust=1, label="Log(Default Rate) = 3.255 + Completion Rate * -0.0245
                                                                 p < 0.000, adj. R-sq: 0.5521") +
    theme_classic() + xlab("Completion Rate (6 years)") + scale_color_brewer(palette="Paired") +
    ylab("Log(Default Rate + 1)") + labs(title="Lower Completion Rate, More Loan Defaults") +
    annotate("text", x=1, y=.25, hjust=0, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov")

# Fewer Pell Grants
pelldef <- lm(log(Default.3Y) ~ Pell.Grant.Pct, data13)
ggplot(data13, aes( Pell.Grant.Pct, log(Default.3Y) )) + geom_point(color="cadetblue", size = 2, alpha=.75) +
     stat_smooth(method="lm", se=FALSE, color="black") + annotate("text", x=100, y=4, hjust=1, label="Log(Default Rate) = 1.010 + Percent of Students with Pell Grants * 0.0248
                                                                  p < 0.000, adj. R-sq: 0.4429") +
     theme_classic() + xlab("Percentage of Students with Pell Grants") + scale_color_brewer(palette="Paired") +
     ylab("Log(Default Rate + 1)") + labs(title="More Pell Grants, More Loan Defaults") +
     annotate("text", x=1, y=.25, hjust=0, label="College Scorecard Data, 2010-13\ncollegescorecard.ed.gov")


