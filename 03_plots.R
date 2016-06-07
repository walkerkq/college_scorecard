

########### PLOTS
data13 <- data2[data2$Year==2013, ]


# Default Rates Higher than Mortgage Default Rates
# https://research.stlouisfed.org/fred2/series/DRSFRMACBS
meandef <- mean(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # mean default rate
mddef <- median(data13[!is.na(data13$Default.3Y), ]$Default.3Y)  # median default rate
ggplot(data13, aes(Default.3Y)) + geom_histogram(stat="bin", binwidth=.01, fill="lightblue") + theme_classic() + 
    geom_vline(xintercept=mddef, color="black", size=2) + 
    annotate("text", x=(mddef+.02), y=130, hjust=0, color="black", label="Median Student Loan\nDefault Rate: 6.7%") +
    xlab("Default Rate (3 year cohort)") + labs(title="High Student Loan Default Rate the Norm") + ylab("Number of Colleges") +
    annotate("text", x=.4, y=160, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov")

# Default Rate Higher at Public Schools
typedeft <- t.test(sqrt(Default.3Y) ~ Type, data13) # not equal. mean for private is .25 and public is 0.28 (sqrt)
ggplot(data13, aes(Type, Default.3Y)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
    theme_classic() + ylab("Default Rate\n(3 year cohort)") + xlab("") + labs(title="Default Rate Higher at Public Schools") +
    annotate("text", y=.35, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typedeft$p.value, sep=""))


# Finish on Time to Avoid Loan Default
compdef <- lm(Default.3Y ~ Completion, data13)
ggplot(data13, aes( Completion, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
    stat_smooth(method="lm", se=FALSE, color="black") + annotate("text", x=1, y=.35, hjust=1, label="Default = 0.1889 + Completion Rate * -0.2031") +
    theme_classic() + xlab("Completion Rate (6 years)") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="Finish on Time to Avoid Loan Default") +
    annotate("text", x=1, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov")

# Completion Rate Lower at Public Schools
typecompt <- t.test(Completion ~ Type, data13) # not equal. mean for private is .56 and public is 0.48 (sqrt)
ggplot(data13, aes(Type, Completion)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
    theme_classic() + ylab("Completion Rate (6 years)") + xlab("") + labs(title="Completion Rate Lower at Public Schools") +
    annotate("text", y=.01, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typecompt$p.value, sep=""))


# Pay More, Owe Less?
fit <- lm(Default.3Y ~ Avg.Cost + Type, data13)
res <- data.frame(school=data2[complete.cases(data13[,c(5,9,16), ]), 1], residuals=fit$residuals)
res <- res[order(res$residuals), ]
fitpub <- lm(Default.3Y ~ Avg.Cost, data13[data13$Type=="Public", ]) # int .01856 slope -.000004797
fitpriv <- lm(Default.3Y ~ Avg.Cost, data13[data13$Type=="Private", ]) # int .01876 slope -.000002914

ggplot(data13, aes( Avg.Cost, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) +
    annotate("text", y=.34, x=60000, hjust=1, label="Default = 0.1920 +\n-0.000003048(Avg.Cost) +\n-0.04137(1 if public)(Avg.Cost)")+
    theme_classic() + xlab("Avg. Cost (Gross)") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="Higher Cost ≠ Higher Default Rate") +
    annotate("text", x=60000, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") +
    geom_abline(intercept=0.1856, slope=-.000004797) + geom_abline(intercept=0.1876, slope=-.000002914)

# But, High Family Income Eases Default Rates
fit2 <- lm(Default.3Y ~ Avg.Fam.Income.Dependent, data13)
ggplot(data13, aes( Avg.Fam.Income.Dependent, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) + 
    stat_smooth(method="lm", se=FALSE,color="black") + annotate("text", y=.34, x=150000, hjust=1,
                                                                label="Default = 0.2 +\n-0.00000164(Avg. Family Income)")+
    theme_classic() + xlab("Avg. Family Income - Dependent Students; 2014 Dollars") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="High Family Income Eases Default Rates") +
    annotate("text", x=150000, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") + ylim(c(0,.4))

#typeinc <- t.test(Avg.Fam.Income.Dependent ~ Type, data13) # not equal. private mean is 76K public mean is 66K
#ggplot(data13, aes(Type, Avg.Fam.Income.Dependent)) + geom_boxplot(aes(fill=Type)) + scale_fill_brewer(palette="Paired") + 
# theme_classic() + ylab("Avg. Family Income") + xlab("") + labs(title="Average Family Income Lower at Public Schools") +
# annotate("text", y=.01, x=2.5, hjust=1, label=paste("Welch Two-Sample t-test\np=", typeinc$p.value, sep=""))

#ggplot(data13, aes( Avg.Fam.Income.Dependent, Avg.Cost )) + geom_point(aes(color=Type))

# Admission Rate not a Factor
fit3 <- lm(Default.3Y ~ (Adm.Rate)^2, data13)
ggplot(data13, aes( Adm.Rate, Default.3Y )) + geom_point(aes(color=Type), size = 2, alpha=.75) + 
    stat_smooth(method="lm", se=FALSE,color="black") + annotate("text", y=.34, x=1, hjust=1,
                                                                label="Default = 0.064 +\n-0.017(Adm.Rate)\np=0.014")+
    theme_classic() + xlab("Admission Rate") + scale_color_brewer(palette="Paired") +
    ylab("Default Rate\n(3 year cohort)") + labs(title="Admission Rate Less Important") +
    annotate("text", x=1, y=0.4, hjust=1, label="College Scorecard Data, 2013\ncollegescorecard.ed.gov") + ylim(c(0,.4))

