###################### PLOTS ######################
library(ggplot2)
library(RColorBrewer)

########### Make default rate over time plot ######

setwd("/Users/kaylinwalker/Desktop/large_data/")
data04 <- read.csv("merged_2004_PP.csv", stringsAsFactors=FALSE)
data05 <- read.csv("merged_2005_PP.csv", stringsAsFactors=FALSE)
data06 <- read.csv("merged_2006_PP.csv", stringsAsFactors=FALSE)
data07 <- read.csv("merged_2007_PP.csv", stringsAsFactors=FALSE)
data08 <- read.csv("merged_2008_PP.csv", stringsAsFactors=FALSE)
data09 <- read.csv("merged_2009_PP.csv", stringsAsFactors=FALSE)
data10 <- read.csv("merged_2010_PP.csv", stringsAsFactors=FALSE)
data11 <- read.csv("merged_2011_PP.csv", stringsAsFactors=FALSE)
data12 <- read.csv("merged_2012_PP.csv", stringsAsFactors=FALSE)
data13 <- read.csv("merged_2013_PP.csv", stringsAsFactors=FALSE)
data04$Year <- 2004
data05$Year <- 2005
data06$Year <- 2006
data07$Year <- 2007
data08$Year <- 2008
data09$Year <- 2009
data10$Year <- 2010
data11$Year <- 2011
data12$Year <- 2012
data13$Year <- 2013
data0 <- rbind(data04, data05, data06, data07, data08, data09, data10, data11, data12, data13)
data0 <- data0[ ,c(15, 17, 291, 440, 441, 1730)]
colnames(data0) <- c("Predominant.Degree", "Type", "Enrollment", "Default.2Y", "Default.3Y", "Year")
data0 <- data0[data0$Predominant.Degree==3 & data0$Type!=3 & data0$Enrollment > .5, ] 
data0 <- data0[ ,4:6]
for(j in c(1,2)) data0[,j] <- as.numeric(data0[,j])
for(r in seq_along(data0$Default.3Y)) { if(is.na(data0$Default.3Y[r])) data0$Default.3Y[r] <- data0$Default.2Y[r] }
data0$Default.2Y <- NULL
data0$Default.3Y <- data0$Default.3Y*100


# Default Rates Higher than Mortgage Default Rates
# https://research.stlouisfed.org/fred2/series/DRSFRMACBS
meandef <- mean(data0[!is.na(data0$Default.3Y), ]$Default.3Y)  # mean default rate
mddef <- median(data0[!is.na(data0$Default.3Y), ]$Default.3Y)  # median default rate
aggdef <- aggregate(Default.3Y ~ Year, data0, median)

ggplot(aggdef, aes(Year, Default.3Y)) + geom_point(color="cadetblue", size=3) + geom_line(color="cadetblue", size=1.5) + ylim(c(0,8)) + 
     ylab("Default Rate (3 year cohort)") + labs(title="Median Student Loan Default Rate") + xlab("") +
     annotate("text", x=2013, y=1, hjust=1, label="College Scorecard Data, 2004-13\ncollegescorecard.ed.gov") + theme_classic()

#################### Other plots #######################

data13 <- read.csv("data/imputed_data.csv", stringsAsFactors=FALSE)
data13$Default.3Y <- data13$Default.3Y + 1 # for log transforms

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


