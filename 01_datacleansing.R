############################ DATA CLEANSING ############################

setwd("/Users/kaylinwalker/R/college_scorecard/data/")
#setwd("/Users/kwalker/git_projects/college_scorecard/data/")

setwd("/Users/kaylinwalker/Desktop/large_data/")
data10 <- read.csv("merged_2010_PP.csv", stringsAsFactors=FALSE)
data11 <- read.csv("merged_2011_PP.csv", stringsAsFactors=FALSE)
data12 <- read.csv("merged_2012_PP.csv", stringsAsFactors=FALSE)
data13 <- read.csv("merged_2013_PP.csv", stringsAsFactors=FALSE)
data10$Year <- 2010
data11$Year <- 2011
data12$Year <- 2012
data13$Year <- 2013
data0 <- rbind(data10, data11, data12, data13)

data <- data0[ , c(4, 6, 15, 16, 17, 19, 37, 291, 377, 382, 383, 384, 385, 386, 387, 429, 438, 440, 441, 
                   1432, 1709, 1730)] 

colnames(data) <- c("School", "State", "Predominant.Degree", "Highest.Degree", "Type", "Region", "Adm.Rate", "Enrollment",
                    "Avg.Cost", "Net.Revenue", "Inst.Expense", "Avg.Faculty.Pay", "Percent.FTE.Faculty", "Pell.Grant.Pct", 
                    "Completion", "Retention",  "Pct.Fed.Loan", "Default.2Y", "Default.3Y", "Avg.Fam.Income.Dependent", 
                    "Md.Debt.n30", "Year")

# remove FAFSA, Mn.Earnings.6Y, Md.Earnings.6Y, Md.Household.Income [6080 NAs / 100%]
# consider removing Default.3Y, Default.2Y [1532 NAs / about 25%] & Avg.Fam.Income.Dependent [908 NAs / 15%]

#write.csv(data, "merged_2010-2013.csv", row.names=FALSE)